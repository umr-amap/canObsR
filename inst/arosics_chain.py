
import rasterio 
import numpy as np
import argparse
import os
from arosics import COREG, COREG_LOCAL, DESHIFTER
from rasterio.windows import Window
import pandas as pd
import warnings
from osgeo import gdal
import pickle
from shapely.geometry import Polygon
import multiprocessing

parser = argparse.ArgumentParser()
parser.add_argument('--path_in', type=str)
parser.add_argument('--ref_filepath', type=str)
parser.add_argument('--out_dir_path', type=str)
parser.add_argument('--corr_type', type=str, default='global')
parser.add_argument('--dynamic_corr', type=bool, default=False)
parser.add_argument('--mp', default=1)
parser.add_argument('--max_shift', type=int, default=250)
parser.add_argument('--max_iter', type=int, default=100)
parser.add_argument('--ws', default=None)
parser.add_argument('--wp', default=(None, None))
parser.add_argument('--grid_res', type=int, default=1000)
parser.add_argument('--apply_matrix', default=False)
parser.add_argument('--save_plot', default=False)
parser.add_argument('--save_data', default=True)
parser.add_argument('--compress_lzw', default=False)
args = parser.parse_args()


def str2bool(v):
    """
    Converts string to bool. Ex : str2bool('True') = True
    """
    if v is None or isinstance(v, bool):
        return v
    if v.lower()=='none':
        return None
    elif v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    elif v in [0, 1]:
        return bool(v)
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

def compress_LZW(file_path):
    with rasterio.open(file_path) as src:
        
        meta = src.meta.copy()
        if src.profile.get('compress', 'Uncompressed')!='lzw':
            print(f"Image {file_path} is being compressed")
            # Update the metadata to use LZW compression
            meta.update(compress='lzw', bigtiff=True)
            im = src.read()
            src.close()
            # Write the data to a new file with LZW compression
            with rasterio.open(file_path, 'w', **meta) as dst:
                dst.write(im)

        else:
            print(f"No compression needed for image : {file_path}")


def harmonize_crs(input_path, ref_path, check_ref=True, compress_lzw=False):
    """
    Forces two raster files to have the same coordinate system : takes the crs of the reference file and writes it into the input file. Also optionnally perform a LZW compression on the image(s)

    Parameters:
    input file (str): 
        Path to the first image
    ref_path (str): 
        Path to the second image (reference)
    check_ref (bool, optional):  
        If True (default), perform an additional safety measure by rewriting the crs of the reference image aswell. May prevent errors if both files have their crs defined from different libraries (rasterio.CRS and pyproj.CRS)
    compress_lzw (bool, optional):  
        If True (default), perform a lzw compression on the image(s)
    """
    ref_compr = compress_lzw
    input_compr = compress_lzw

    with rasterio.open(ref_path) as ds_ref:
        metadata_ref = ds_ref.meta.copy()
        crs_ref = metadata_ref['crs']

        if compress_lzw:
            if ds_ref.profile.get('compress', 'Uncompressed')!='lzw':
                pass
                ref_compr = False
                metadata_ref.update(compress='lzw', bigtiff=True)
                print(f"Reference image {ref_path} will be compressed")
            else:
                print(f"No compression needed for reference image : {ref_path}")
        
        with rasterio.open(input_path) as ds_in:
            img_in = ds_in.read()
            metadata_in = ds_in.meta.copy()
            correction_needed = metadata_in['crs']!=metadata_ref['crs']
            print("harmonization needed : ", correction_needed)
            if correction_needed:
                metadata_in['crs'] = crs_ref
            
            if compress_lzw:
                if ds_in.profile.get('compress', 'Uncompressed')!='lzw':
                    input_compr = False
                    metadata_in.update(compress='lzw', bigtiff=True)
                    print(f"Input image {input_path} will be compressed")
                else:
                    print(f"No compression needed for input image : {input_path}")

            ds_in.close() 


        if check_ref and (correction_needed or not ref_compr):
            metadata_ref['crs'] = crs_ref
            img_ref = ds_ref.read()
            ds_ref.close()
            with rasterio.open(ref_path, "w", **metadata_ref) as ds_ref_out:
                ds_ref_out.write(img_ref)
                ds_ref_out.close()     
        ds_ref.close()
    if correction_needed or not input_compr:
        with rasterio.open(input_path, "w", **metadata_in) as ds_out:
            ds_out.write(img_in)
            ds_out.close()


def to_GCPList(points_table, fill_val=-9999):
    """
    Creates a list of GCP in the proper format from the csv of coregistration results.
    """

    try:
        GDF = points_table.loc[points_table.ABS_SHIFT != fill_val, :].copy()
    except AttributeError:
        # self.CoRegPoints_table has no attribute 'ABS_SHIFT' because all points have been excluded
        return []

    if getattr(GDF, 'empty'):  # GDF.empty returns AttributeError
        return []
    else:
        # exclude all points flagged as outliers
        if 'OUTLIER' in GDF.columns:
            outliers = []
            for a in GDF.OUTLIER:
                try:
                    a=int(a)
                except:
                    if a == 'True':
                        a = True
                    elif a == 'False':
                        a = False
                outliers.append(a)
    
            GDF.OUTLIER = outliers

            GDF = GDF[GDF.OUTLIER.__eq__(False)].copy()

        avail_TP = len(GDF)
        if not avail_TP:
            # no point passed all validity checks
            return []

        if avail_TP > 7000:
            GDF = GDF.sample(7000)
            warnings.warn('By far not more than 7000 tie points can be used for warping within a limited '
                            'computation time (due to a GDAL bottleneck). Thus these 7000 points are randomly chosen '
                            'out of the %s available tie points.' % avail_TP)

        # calculate GCPs
        GDF['X_MAP_new'] = GDF.X_MAP + GDF.X_SHIFT_M
        GDF['Y_MAP_new'] = GDF.Y_MAP + GDF.Y_SHIFT_M
        GDF['GCP'] = GDF.apply(lambda GDF_row: gdal.GCP(GDF_row.X_MAP_new,
                                                        GDF_row.Y_MAP_new,
                                                        0,
                                                        GDF_row.X_IM,
                                                        GDF_row.Y_IM),
                                axis=1)
        GCPList = GDF.GCP.tolist()

        return GCPList
    


def apply_saved_matrix(im_path, out_dir_path, metadata_path, GCP_path = None):
    """
    Calls arosics functions to perform a global or local co-registration between two images. Option to save the coregistrated image, and in the case of a local CoReg, the tie points data and the vector shift map.

    Parameters:
        :param str im_path: Path to the target image, or to a folder containing multiple target images. Images must be of Geotiff format.
        :param str out_dir_path: Directory where the outputs will be saved.
        :param str metadata_path: path to the .pkl file where the metadata of the specified tranformation have been saved.
        :param str GCP_path: path to the .csv file containing the results of the desired local coregistration. Defaults to None. If the desired transform is a result of a global co-registration, leave it that way.
        
    Returns:
        Nothing
    """
    corr_type = 'global'
    extensions = ('.tif', '.tiff', '.TIF', '.TIFF')
    files = [file for file in sorted(os.listdir(im_path)) if file.endswith(extensions)]
    print("files : ", files)
    for file in files:
        with open(metadata_path, 'rb') as file:
            coreg_info = pickle.load(file)
            file.close()
        
        if GCP_path is not None:
            corr_type = 'local'
            GCP_df = pd.read_csv(GCP_path)
        
        coreg_info['GCPList'] = to_GCPList(GCP_df, -9999)
        current_file_path = os.path.join(im_path, file)
        path_out = os.path.join(out_dir_path, file.split('.')[0].replace("_temp", "") + f'_aligned_{corr_type}.tif')
        CR = DESHIFTER(current_file_path, coreg_info, path_out=path_out, fmt_out="GTIFF")
        CR.correct_shifts() 



def call_arosics(path_in, path_ref, path_out=None, corr_type = 'global', max_shift=250, max_iter=100, window_size=1500, window_pos = (None, None), mp=None, grid_res=1000, save_data = True, save_vector_plot = False, queue=None):
    """
    Calls arosics functions to perform a global or local co-registration between two images. Option to save the coregistrated image, and in the case of a local CoReg, the tie points data and the vector shift map.

    Parameters:
        path_in (str): 
            source path of the target image, i.e. the image to be shifted
        path_ref (str): 
            Path to the refernce image 
        path_out (str, optional): 
            target path of the coregistered image. Defaults to None, in which case nothing will be written to disk
        corr_type (str): 
            Type of co-registration. Either 'global' (default) or 'local'
        max_shift (int): 
            maximum shift distance in reference image pixel units
        max_iter (int): 
            maximum number of iterations for matching (default: 5)
        window_size (int): 
            custom matching window size [pixels] as (X, Y) tuple (default: (256,256))
        window_pos (tuple(int)): 
            custom matching window position as (X, Y) map coordinate in the same projection as the reference image (default: central position of image overlap). Only used when performing global co-registration
        grid_res (int): 
            tie point grid resolution in pixels of the target image (x-direction). Only applies to local co-registration
        mp (int): 
            Number of CPUs to use. If None (default), all available CPUs are used. If mp=1, no multiprocessing is done. 
        save_data (bool): 
            If True (default), saves the transformation metadata in a .pkl file, and the tie points data in a csv file. The latter only happens when performing local co-registration
        save_vector_plot (bool): 
            If True (default), saves the a map of the calculated tie point grid in a JPEG file. Has an effect only when performing local co-registration

    Returns:
        A COREG object containing all info on the calculated shifts
    """
    #CPUs = None if mp else 1
    CPUs = mp if mp is None else int(mp)
    print("CPUs : ", CPUs)

    print("Input image : ", os.path.basename(path_in))
    print("Reference image : ", os.path.basename(path_ref))

    if corr_type=='global':
        CR = COREG(path_ref, path_in, path_out=path_out, fmt_out="GTIFF", ws=(window_size, window_size), wp=window_pos, max_shift=max_shift, max_iter=max_iter, CPUs=CPUs)
        CR.correct_shifts()
        if save_data :
            #shifts = CR.coreg_info['corrected_shifts_map']
            #shift_x, shift_y = shifts['x'], shifts['y']
            #df = pd.DataFrame({'Shift_X':[shift_x], 'Shift_Y':[shift_y]})
            #df.to_csv(os.path.join(os.path.dirname(path_out), os.path.basename(path_out).split('.')[0] + '_shift.csv'), index=False)
            with open(os.path.join(os.path.dirname(path_out), os.path.basename(path_out).split('.')[0] + '_metadata.pkl'), 'wb') as file:
                pickle.dump(CR.coreg_info, file)

    elif corr_type=='local':
        CR = COREG_LOCAL(path_ref, path_in, path_out=path_out, fmt_out="GTIFF", window_size=(window_size, window_size), max_shift=max_shift, max_iter=max_iter, CPUs=CPUs, grid_res=grid_res)
        CR.correct_shifts()
        if save_data:
            df = CR.CoRegPoints_table
            df.to_csv(os.path.join(os.path.dirname(path_out), os.path.basename(path_out).split('.')[0] + '_CoRegPoints_table.csv'), index=False)
            cor_info = CR.coreg_info
            del(cor_info['GCPList'])
            with open(os.path.join(os.path.dirname(path_out), os.path.basename(path_out).split('.')[0] + '_metadata.pkl'), 'wb') as file:
                pickle.dump(cor_info, file)
        if save_vector_plot:
            DPI=300
            vector_scale=15
            CR.view_CoRegPoints(shapes2plot = 'vectors', savefigPath = path_out.split('.')[0] + f"_vector_map_{DPI}DPI.JPEG", savefigDPI=DPI, vector_scale=vector_scale, backgroundIm='tgt')
        compress_LZW(path_out)
    if queue and corr_type=="global":
        queue.put(CR.coreg_info)
    else:
        return CR.coreg_info

def complete_arosics_process(path_in, ref_filepath, out_dir_path, corr_type = 'global', max_shift=250, max_iter=100, grid_res=1000, window_size=None, window_pos = (None, None), mp=None, compress_lzw=False, save_data = True, save_vector_plot = False, dynamic_corr = False, apply_matrix=False):
    """
    Complete pipeline that uses arosics to perform a global or local co-registration on a file or a group of files located inside a folder. In the case of a local CoReg, option to save the tie points data and the vector shift map.

    :param str path_in: Path to the target image, or to a folder containing multiple target images. Images must be of Geotiff format.
    :param str ref_filepath: Path to the reference image.
    :param str out_dir_path: Directory where the outputs will be saved.
    :param str corr_type: Type of co-registration. Either 'global' (default) or 'local'.
    :param int max_shift: Maximum shift distance in reference image pixel units.
    :param int max_iter: Maximum number of iterations for matching (default: 5).
    :param int grid_res: Tie point grid resolution in pixels of the target image (x-direction). Only applies to local co-registration.
    :param int window_size: Custom matching window size [pixels] as (X, Y) tuple (default: (256,256)).
    :param tuple window_pos: Custom matching window position as (X, Y) map coordinate in the same projection as the reference image (default: central position of image overlap). Only used when performing global co-registration.
    :param int mp: Number of CPUs to use. If None (default), all available CPUs are used. If mp=1, no multiprocessing is done.
    :param bool compress_lzw:  If True (default), perform a lzw compression on the image(s)
    :param bool save_data: If True (default), saves the transformation metadata in a .pkl file, and the tie points data in a csv file. The latter only happens when performing local co-registration
    :param bool save_vector_plot: If True (default), saves the a map of the calculated tie point grid in a JPEG file. Has an effect only when performing local co-registration.
    :param bool dynamic_corr: When correcting multiple images, whether or not to use the last corrected image as reference for the next co-registration.
        If False (default), all images are corrected using 'ref_filepath' as the reference image.
        If True, image 1 will use 'ref_filepath' as a reference, then image N (N>=2) will use the corrected version of image N-1 as reference.
    :param bool apply_matrix: When correcting multiple images, whether or not to directly apply the shifts computed for the first image to all the remaining ones, instead of computing the shifts for each one independently. Defaults to False.
        Using this option allows faster computing time and better alignment between input images.
        The time saved will decrease if the images have different bounds, as additionnal work is necessary to ensure a correct alignement. (Currently, temporary padded images are created in that case, we hope to change that soon)
                        
    :returns: A COREG info object (if path_in is a file or if apply_matrix is True) or a list of COREG info objects (if path_in is a folder and apply_matrix is False) containing all info on the calculated shift(s).
    """

    assert corr_type in ['global', 'local']
    rm_temp_files = False
    dynamic_corr = str2bool(dynamic_corr)
    apply_matrix = str2bool(apply_matrix)
    save_vector_plot = str2bool(save_vector_plot)
    save_data = str2bool(save_data)
    compress_lzw = str2bool(compress_lzw)
    mp = mp if mp is None else int(mp)
    grid_res = int(grid_res)
    window_size = window_size if window_size is None else int(window_size)
    max_iter = int(max_iter)
    max_shift = int(max_shift)
    #Set default values for window_size
    if corr_type == 'global':
        if window_size is None :
            window_size = 1500
        grid_res = ""
    elif corr_type == 'local':
        if window_size is None :
            window_size = 4000 

    if not os.path.exists(out_dir_path):
        os.mkdir(out_dir_path)

    extensions = ('.tif', '.tiff', '.TIF', '.TIFF')

    if os.path.isfile(path_in):
        if not path_in.endswith(extensions):
            raise ValueError(f"The specified file '{path_in}' must be of GeoTiff format")
        else:
            harmonize_crs(path_in, ref_filepath, compress_lzw=compress_lzw)
            path_out = os.path.join(out_dir_path, path_in.split('/')[-1].split('\\')[-1].split('.')[0] + f'_aligned_{corr_type}.tif')
            CR_info = call_arosics(path_in, ref_filepath, path_out=path_out, corr_type=corr_type, mp=mp, window_size=window_size, window_pos=window_pos, max_shift=max_shift, max_iter=max_iter, grid_res=grid_res, save_vector_plot=save_vector_plot, save_data=save_data)
            return CR_info
            
    elif os.path.isdir(path_in):
        
        files = [file for file in sorted(os.listdir(path_in)) if file.endswith(extensions)]
        print("files : ", files)

        if len(files)==0:
            raise ValueError(f"The specified directory '{path_in}' does not contain any GeoTiff files.")

        elif dynamic_corr or not apply_matrix :
            list_CR_info = []
            for i in range(len(files)):
                file = files[i]
                current_file_path = os.path.join(path_in, file)
                harmonize_crs(current_file_path, ref_filepath, check_ref = True if i==0 else False, compress_lzw=compress_lzw)
                path_out = os.path.join(out_dir_path, file.split('.')[0].replace("_temp", "") + f'_aligned_{corr_type}.tif')
                CR_info = call_arosics(current_file_path, ref_filepath, path_out=path_out, corr_type=corr_type, mp=mp, window_size=window_size, window_pos=window_pos, max_shift=max_shift, max_iter=max_iter, grid_res=grid_res, save_vector_plot=save_vector_plot, save_data=save_data)
                queue = multiprocessing.Queue()
                process = multiprocessing.Process(target=call_arosics, args=(current_file_path, ref_filepath, path_out, corr_type, max_shift, max_iter, window_size, window_pos, mp, grid_res, save_data, save_vector_plot, queue))
                process.start()
                process.join()     
                # Terminate the process if needed (ensure cleanup)
                if corr_type=='global':
                    CR_info = queue.get()
                list_CR_info.append(CR_info)
                
                process.terminate()

                if process.is_alive():         
                    raise TimeoutError("The arosics process is taking too much time and has been terminated")
                else:
                    print("Process terminated successfully")
                
                if dynamic_corr:
                    ref_filepath = path_out

            return list_CR_info
          
        else:
            hlist, blist, glist, dlist, band_count = [], [], [], [], []
            for file in files:
                meta = rasterio.open(os.path.join(path_in, file)).meta.copy()
                tf = list(meta['transform'])
                hlist.append(tf[5])
                blist.append(tf[5] + tf[4]*meta['height'])
                glist.append(tf[2])
                dlist.append(tf[2] + tf[0]*meta['width'])
                band_count.append(meta['count'])
            
            
            if not (all(x == hlist[0] for x in hlist) and all(x == glist[0] for x in glist)):     #and (np.max(dlist)-np.max(glist)) < 0.9*(np.max(dlist)-np.min(glist)) and (np.min(hlist)-np.max(blist)) < 0.9*(np.max(hlist)-np.max(blist))  #if apply_mask
                rm_temp_files=True
                mask_coords = [np.min(glist), np.min(blist), np.max(dlist), np.max(hlist)]
                geom = Polygon([(mask_coords[0], mask_coords[1]), (mask_coords[0], mask_coords[3]), (mask_coords[2], mask_coords[3]), (mask_coords[2], mask_coords[1])])
                num_cols = int(np.ceil((mask_coords[2]-mask_coords[0]) / tf[0]))
                num_rows = int(np.ceil((mask_coords[1]-mask_coords[3]) / tf[4]))
                print("mask dimensions : ", (num_cols, num_rows))
                print("Mask : ", geom.bounds)
                #All images are padded to fit the geometry of the mask
                for i in range(len(files)) :
                    file = files[i]
                    rast = rasterio.open(os.path.join(path_in, file))
                    img = rast.read()
                    print("image shape : ", img.shape)

                    padded_data = np.ones((band_count[i], num_rows, num_cols), dtype=img.dtype)*255
                    
                    # Calculate the offset to pad the smaller raster
                    row_offset = int((geom.bounds[3] - rast.bounds.top) / abs(rast.res[0]))
                    col_offset = int((rast.bounds.left - geom.bounds[0]) / abs(rast.res[1]))
                    
                    # Define the window to copy the smaller raster into the padded data array
                    window = Window(col_offset, row_offset, img.shape[2], img.shape[1])

                    # Copy the smaller raster into the padded data array
                    padded_data[:, window.row_off : window.row_off+window.height, window.col_off : window.col_off+window.width] = img
                    # Update the metadata for the padded raster
                    target_tf = rasterio.Affine(list(rast.meta.copy()['transform'])[0], 0.0, mask_coords[0], 0.0, list(rast.meta.copy()['transform'])[4], mask_coords[3])
                    profile = rast.profile
                    profile.update(width=padded_data.shape[2], height=padded_data.shape[1], transform=target_tf,
                                    bounds=geom.bounds, count = band_count[i])
                    print("padded img shape : ", padded_data.shape)
                    out_path = os.path.join(path_in, f"{file.split('.')[0]}_temp.tif")
                    files[i] = f"{file.split('.')[0]}_temp.tif"
                    rast.close()
                    # Write the padded data to a new raster file
                    with rasterio.open(out_path, "w", **profile) as dst:
                        dst.write(padded_data)
                        dst.close()

            first_file = files[0]
            harmonize_crs(os.path.join(path_in, first_file), ref_filepath, compress_lzw=compress_lzw)
            path_out = os.path.join(out_dir_path, first_file.split('.')[0].replace("_temp", "") + f'_aligned_{corr_type}.tif')
            CR_info = call_arosics(os.path.join(path_in, first_file), ref_filepath, path_out=path_out, corr_type=corr_type, mp=mp, window_size=window_size, window_pos=window_pos, max_shift=max_shift, max_iter=max_iter, grid_res=grid_res, save_vector_plot=save_vector_plot, save_data=save_data)
            """"
            queue = multiprocessing.Queue()
            process = multiprocessing.Process(target=call_arosics, args=(os.path.join(path_in, first_file), ref_filepath, path_out, corr_type, max_shift, max_iter, window_size, window_pos, mp, grid_res, save_data, save_vector_plot, queue))
            process.start()
            process.join()    
            # Terminate the process if needed (ensure cleanup)
            CR_info = queue.get()

            process.terminate()

            if process.is_alive():
                raise TimeoutError("The arosics process is taking too much time and has been terminated")
            else:
                print("Process terminated successfully")
            """
            for file in files[1:]:
                current_file_path = os.path.join(path_in, file)
                harmonize_crs(current_file_path, ref_filepath, check_ref=False, compress_lzw=compress_lzw)
                path_out = os.path.join(out_dir_path, file.split('.')[0].replace("_temp", "") + f'_aligned_{corr_type}.tif')
                DS = DESHIFTER(current_file_path, CR_info, path_out=path_out, fmt_out="GTIFF")
                DS.correct_shifts()

            if rm_temp_files:
                for file in files:
                    os.remove(os.path.join(path_in, file))
            return CR_info
    
    else:
        raise ValueError(f"The specified path '{path_in}' is not a file nor a directory.")
    
    

if __name__ == '__main__':
    print(args)
    complete_arosics_process(path_in = args.path_in,
                             ref_filepath = args.ref_filepath, 
                             out_dir_path = args.out_dir_path, 
                             corr_type = args.corr_type, 
                             mp = args.mp,
                             max_shift = args.max_shift,
                             max_iter = args.max_iter,
                             grid_res = args.grid_res,
                             window_pos = args.wp,
                             window_size = args.ws,
                             dynamic_corr = args.dynamic_corr,
                             apply_matrix = args.apply_matrix,
                             save_data = args.save_data,
                             save_vector_plot = args.save_plot,
                             compress_lzw = args.compress_lzw,
                             )
