# This script is based on the following work :
# Fabrice Vinatier, & Denis Feurer. (2023). Time-SIFT module for Agisoft Metashape software. Zenodo. https://doi.org/10.5281/zenodo.8359983
# The code of the original plugin is available using the following DOI : 10.5281/zenodo.8359982

import os
from os import path
import re
import numpy as np
import Metashape as scan
import time
import argparse
import shutil

#scan.License().activate('your_license_key')

parser = argparse.ArgumentParser()
parser.add_argument('--crs', type=str, default="EPSG::32622")
parser.add_argument('--pathDIR', type=str)
parser.add_argument('--out_dir_ortho', type=str)
parser.add_argument('--out_dir_dem', type=str, default = None,)
parser.add_argument('--out_dir_project', type=str, default = None)
parser.add_argument('--resol_ref', type=float, default = 0.05)
parser.add_argument('--data_type', type=str, default = 'RGB')
parser.add_argument('--site_name', type=str, default = '')
parser.add_argument('--calibrate_col', default = True)
parser.add_argument('--sun_sensor', default = False)
parser.add_argument('--group_by_flight', default = False)
parser.add_argument('--downscale_factor_alignement', type=int, default = 1)
parser.add_argument('--downscale_factor_depth_map', type=int, default = 2)
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
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')


def add_all_chunks(doc, pathDIR=None):
    """
    Loads all RGB photos into the project, one chunk per subfolder in pathDIR
    """
    print(pathDIR)
    os.chdir(pathDIR)
    epochs = os.listdir(pathDIR)
    # we select only the non-empty subfolders
    list_files = [] 
    for (dirpath, dirnames, filenames) in os.walk(pathDIR):
        list_files += [os.path.join(dirpath, file) for file in filenames]
    
    ep_relative_paths = np.unique([os.path.relpath(os.path.dirname(file), start=pathDIR) for file in list_files])
    print("ep paths : ", ep_relative_paths)
    epochs = [os.path.basename(dir_path) for dir_path in ep_relative_paths]
    print("epochs : ", epochs)
    
    # We remove all existing chunks and add them one by one
    for chk in doc.chunks:
        doc.remove(chk)
    for i in range(len(ep_relative_paths)):
        ep_name, ep_path = epochs[i], ep_relative_paths[i]
        add_TimeSIFT_chunk(doc, ep_path = ep_path, epoch_name = ep_name)


def add_TimeSIFT_chunk(doc, ep_path="", epoch_name=""):
    """
    Adds a single RGB chunk to the project
    """
    if len([chk for chk in doc.chunks if re.search(epoch_name,chk.label) is not None])==0:
        doc.addChunk()
        chunk=doc.chunks[len(doc.chunks)-1]
        chunk.label=epoch_name
        [f for f in os.listdir(ep_path) if os.path.isfile(os.path.join(ep_path, f))]
        dirName=ep_path
        listOfFiles = list()
        for (dirpath, dirnames, filenames) in os.walk(dirName):
            listOfFiles += [os.path.join(dirpath, file) for file in filenames]

        chunk.addPhotos(listOfFiles)
        for cam in chunk.cameras:
            cam.label = (str(chunk.label) + "_EPOCH_" + cam.label)


def merge_chunk_TimeSIFT(doc):
    """
    Merges all chunks into a single one
    """
    start_time = time.time()
    for chk_sel in doc.chunks:
        chunk_non_aligned = [chk.key for chk in doc.chunks if re.search("TimeSIFT", chk.label) is not None]
        chunk_non_aligned.append(chk_sel.key)
        doc.mergeChunks(chunks=chunk_non_aligned)
        TS_chunk = [chk for chk in doc.chunks if re.search("TimeSIFT", chk.label) is not None]
        doc.remove(TS_chunk)
        merged_chunk = doc.chunks[-1]
        merged_chunk.label = "TimeSIFT"
        doc.remove(chk_sel)
        for cam in [i for i in merged_chunk.cameras if re.search(chk_sel.label,i.label) is not None]:
            cam.transform = None
    print("Temps écoulé pour la fusion : ", time.time() - start_time)


def align_TimeSIFT_chunk(doc, downscale_factor = 1):
    """
    Aligns all cameras in the merged chunk
    """
    start_time = time.time()
    TS_chunk=[chk for chk in doc.chunks if re.search("TimeSIFT",chk.label) is not None][0]
    TS_chunk.matchPhotos(downscale=downscale_factor, generic_preselection=True, reference_preselection=True,
                      reference_preselection_mode=scan.ReferencePreselectionSource, keypoint_limit=100000,
                      tiepoint_limit=10000,keep_keypoints=False)
    # boucle pour ré-aligner les photos non-alignées
    nb_aligned_before = 0
    nb_aligned_after = 100
    while nb_aligned_after != nb_aligned_before:
        nb_aligned_before = len([i for i in TS_chunk.cameras if i.transform])
        TS_chunk.matchPhotos(downscale=downscale_factor, generic_preselection=True, reference_preselection=True,
                          reference_preselection_mode=scan.ReferencePreselectionSource, keypoint_limit=200000,
                          tiepoint_limit=20000,keep_keypoints=False)
        TS_chunk.alignCameras()
        nb_aligned_after = len([i for i in TS_chunk.cameras if i.transform])
    aligned_cameras = [i for i in TS_chunk.cameras if i.transform]
    print("------",str(len(aligned_cameras)),"cameras aligned out of",len(TS_chunk.cameras),"----")
    TS_chunk.resetRegion()
    TS_chunk.optimizeCameras()
    TS_chunk.updateTransform()
    print("Temps écoulé pour l'alignement : ", time.time() - start_time)


def split_TimeSIFT_chunk(doc, group_by_flight = False):
    """
    After the alignement, splits the merged chunk into smaller chunks, each representing a date (default) or a flight

    Parameters:
    group_by_flight (bool): 
        If True, regroups data by flight. Else, regroup it by date (default)
    """
    TS_chunk = [chk for chk in doc.chunks if (re.search("TimeSIFT", chk.label) is not None)][0]
    if group_by_flight:
        TS_chunk_names=np.unique([cam.label.split("_EPOCH_")[0] for cam in TS_chunk.cameras])
    else:
        #searching for a date in YYYYMMDD or YYYYMM format in cameras_names to do the slpit
        pattern = r'\d{4}(0[1-9]|1[0-2])([0-2][0-9]|3[01])?'
        TS_chunk_names=np.unique([re.search(pattern, cam.label).group() if re.search(pattern, cam.label) else cam.label for cam in TS_chunk.cameras])              #r'\d+'
    print(TS_chunk_names)
    for chk_name in TS_chunk_names:
        if len([chk for chk in doc.chunks if re.search(chk_name,chk.label) is not None])==0:
            NewChunk = TS_chunk.copy()
            NewChunk.label=chk_name
            #pattern = str(chk_name) + "_EPOCH_"
            pattern = str(chk_name)
            t = [pattern in cam.label for cam in NewChunk.cameras]
            list_cameras = [NewChunk.cameras[i] for i, x in enumerate(t) if not x]
            NewChunk.remove(list_cameras)


def merge_chunk_with_same_date(doc):
    """
    Not useful anymore. Merging by date now done in split_TIMESift_chunk
    """
    dates = []
    Non_ts_chunks = [chk for chk in doc.chunks if re.search("TimeSIFT",chk.label) is None]
    for chk in Non_ts_chunks:
        chunk_date = chk.label[:8]
        if chunk_date not in dates :
            dates.append(chunk_date)
    print("Dates : ", dates)
    for date in dates:
        chunks_to_merge = [chk.key for chk in Non_ts_chunks if chk.label[:8]==date]
        doc.mergeChunks(chunks=chunks_to_merge)
        merged_chunk = doc.chunks[-1]
        merged_chunk.label = date
        for chk in [chk for chk in Non_ts_chunks if chk.label[:8]==date]:  
            doc.remove(chk)

    

def process_splited_TimeSIFT_chunks_one_by_one(doc, out_dir_ortho = None, out_dir_DEM = None, site_name="", resol_ref = None, crs = None, downscale_factor_depth_map = 2):
    """
    Generate depth map, dense cloud, DEM and orthomosaic for one image. Always saves orthomosaic and saves DEM if specified

    Parameters:
    out_dir_ortho (str): 
        Folder where the orthomosaics are saved 
    out_dir_DEM (str, optional): 
        Folder where the DEMs are saved. If no path is specified, the DEMs are not saved by default
    site_name (str, optional): 
        Adds the data site name into the names of all created folders and files, to better separate generated data from different projects. If not specified, the names will stay generic
    """
    TS_chunks = [chk for chk in doc.chunks if (re.search("TimeSIFT", chk.label) is None)]
    TS_chunks = [chk for chk in TS_chunks if chk.enabled]

    for chk in TS_chunks:
        start_time = time.time()
        NewChunk=chk
        NewChunk.buildDepthMaps(downscale=downscale_factor_depth_map, filter_mode=scan.AggressiveFiltering)
        t_depth_maps = time.time()
        print(f"Time to build depth map : {t_depth_maps - start_time} seconds")
        NewChunk.buildPointCloud(point_colors=True)
        t_dense_cloud = time.time()
        print(f"Time to build dense cloud : {t_dense_cloud - t_depth_maps} seconds")
        NewChunk.buildDem(source_data=scan.PointCloudData,resolution=resol_ref)
        t_DEM = time.time()
        print(f"Time to build DEM : {t_DEM - t_dense_cloud} seconds")
        NewChunk.buildOrthomosaic(surface_data=scan.ElevationData,resolution=resol_ref)
        t_ortho = time.time()
        print(f"Time to build ortho : {t_ortho - t_DEM} seconds")
        print(f"Total process time for the image : {t_ortho - start_time} seconds")
        proj = scan.OrthoProjection()
        proj.type=scan.OrthoProjection.Type.Planar
        proj.crs=scan.CoordinateSystem(crs)
        img_compress=scan.ImageCompression(tiff_compression = scan.ImageCompression.TiffCompressionLZW, tiff_big = True)
        img_compress.tiff_big = True            #apparently necessary, maybe because of ImageCompression init that doesn't deal with bigTiff ?
        doc.save(os.path.join(out_dir_ortho, '_temp_.psx'))

        try :
            NewChunk.exportRaster(os.path.join(out_dir_ortho, f"{str(NewChunk.label)}{site_name}_ORTHO.tif"),source_data=scan.OrthomosaicData, image_format=scan.ImageFormatTIFF,
                                projection=proj, resolution=resol_ref,clip_to_boundary=True,save_alpha=False, split_in_blocks = False, image_compression = img_compress)
            
        # if the raster file is too big and bigTiff doesn't work for some reason, it will be divided into 10000*10000 blocks
        except:
            os.remove(os.path.join(out_dir_ortho, f"{str(NewChunk.label)}{site_name}_ORTHO.tif"))
            NewChunk.exportRaster(os.path.join(out_dir_ortho, f"{str(NewChunk.label)}{site_name}_ORTHO.tif"),source_data=scan.OrthomosaicData, image_format=scan.ImageFormatTIFF,
                                    projection=proj, resolution=resol_ref,clip_to_boundary=True,save_alpha=False, split_in_blocks = True, block_width=10000, block_height=10000, image_compression=img_compress)

        if out_dir_DEM is not None:
            NewChunk.exportRaster(os.path.join(out_dir_DEM, f"{str(NewChunk.label)}{site_name}_DEM.tif"),source_data=scan.ElevationData, image_format=scan.ImageFormatTIFF, image_compression = img_compress,
                                projection=proj, resolution=resol_ref,clip_to_boundary=True, save_alpha=False)


#TODO : confirm whether or not DEMs and project are to be saved by default
def Time_SIFT_process(pathDIR,
                      out_dir_ortho, 
                      out_dir_DEM=None,      
                      out_dir_project=None,   
                      data_type="RGB",
                      resol_ref=0.05, 
                      crs="EPSG::32622", 
                      site_name = "",
                      calibrate_col = True,
                      sun_sensor = False,
                      group_by_flight = False,
                      downscale_factor_alignement = 1,
                      downscale_factor_depth_map = 2,
                      ):
    """
    Executes the complete Time_SIFT process : Loads all photos from the input folder and its subdirectories into a Metashape project . These photos will then be merged, aligned,
    before the orthomosaic and (optionally) DEMs will be generated for each date (or for each flight)

    :param str pathDIR: Path to the folder where the data is located. Inside this folder, there should be one subfolder per flight, with the date of the flight specified in the folder name in the YYYYMMDD or YYYYMM format.
    :param str out_dir_ortho: Folder where the orthomosaics are saved.
    :param str out_dir_DEM: Folder where the DEMs are saved. If no path is specified, the DEMs are not saved by default.
    :param str out_dir_project: Folder where the Metashape project is saved. If no path is specified, the project is not saved by default.
    :param str data_type: The type of the data used. Either 'RGB' (default) or 'MS' (for multispectral images).
    :param float resol_ref: The resolution (in meters) used to generate DEMs and orthomosaics. Defaults to 0.05.
    :param str crs: Coordinate system used, in a string format. Example: crs="EPSG::32622" (default value).
    :param str site_name: Adds the data site name into the names of all created folders and files, to better separate generated data from different projects. If not specified, the names will stay generic.
    :param bool calibrate_col: Whether or not to apply white balance. Defaults to True.
    :param bool sun_sensor: Whether or not to calibrate the reflectance using the sun sensor. Only applies to multispectral images. Defaults to False.
    :param bool group_by_flight: If True, regroups data by flight. Else, regroup it by date (default).
    :param int downscale_factor_alignement: Alignment accuracy (0 - Highest, 1 - High, 2 - Medium, 4 - Low, 8 - Lowest). Defaults to 1.
    :param int downscale_factor_depth_map: Depth map quality (1 - Ultra high, 2 - High, 4 - Medium, 8 - Low, 16 - Lowest). Defaults to 2.

    :return: None
    """
    doc = scan.Document()
    assert data_type in ["RGB", "MS"]
    #for file naming purposes
    if site_name != "":
       site_name = "_" + site_name

    calibrate_col = str2bool(calibrate_col)
    sun_sensor = str2bool(sun_sensor)
    group_by_flight = str2bool(group_by_flight)

    if not os.path.exists(out_dir_ortho):
        os.mkdir(out_dir_ortho)
    
    if out_dir_DEM is not None:  
        if out_dir_DEM == "" :
           out_dir_DEM = os.path.join(os.path.dirname(out_dir_ortho), "DEM")
        if not os.path.exists(out_dir_DEM):
           os.mkdir(out_dir_DEM)

    #TODO : store all times into log file, or add progress bars
    start_time = time.time()
    if data_type == "RGB" or data_type == "MS":
        add_all_chunks(doc, pathDIR = pathDIR)
        merge_chunk_TimeSIFT(doc)
        t_add_data = time.time()
        print(f"Temps écoulé pour le chargement et la fusion des photos : {t_add_data - start_time} seconds")


    """
    elif data_type == "MS" :
        add_all_MS_photos(doc, pathDIR = pathDIR)
        t_add_data = time.time()
        print("Temps écoulé pour le chargement des photos : ", t_add_data - start_time)
    """
    
    if sun_sensor and data_type=='MS':
        TS_chunk = [chk for chk in doc.chunks if (re.search("TimeSIFT", chk.label) is not None)][0]
        TS_chunk.calibrateReflectance(use_sun_sensor=True)

    align_TimeSIFT_chunk(doc, downscale_factor = downscale_factor_alignement)
    t_align = time.time()
    print(f"Temps écoulé pour l'alignement : {t_align - t_add_data} seconds")
    
    #The project needs to be saved before building DEMs and orthomosaics
    doc.save(os.path.join(out_dir_ortho, '_temp_.psx'))

    #Color calibration
    if calibrate_col and (data_type=='RGB' or data_type=='MS'):
        TS_chunk = [chk for chk in doc.chunks if (re.search("TimeSIFT", chk.label) is not None)][0]
        TS_chunk.calibrateColors(scan.TiePointsData, white_balance=True)
    


    doc.save(os.path.join(out_dir_ortho, '_temp_.psx'))
    
    split_TimeSIFT_chunk(doc, group_by_flight = group_by_flight)
    #merge_chunk_with_same_date(doc)
    t_split = time.time()
    #print("Temps écoulé pour la division et regroupement par date : ", t_split - t_align)
    process_splited_TimeSIFT_chunks_one_by_one(doc, out_dir_ortho = out_dir_ortho, out_dir_DEM = out_dir_DEM, site_name = site_name, resol_ref = resol_ref, crs = crs, downscale_factor_depth_map=downscale_factor_depth_map)
    print(f"Temps écoulé pour le process final : {time.time() - t_split} seconds")
    print(f"Temps écoulé pour la pipeline complète : {time.time() - start_time} seconds")
    doc.save(os.path.join(out_dir_ortho, '_temp_.psx'))
    
    if out_dir_project is not None :
        if out_dir_project == "" :
            out_dir_project = out_dir_ortho
        if not os.path.exists(out_dir_project):
            os.mkdir(out_dir_project)
        doc.save(os.path.join(out_dir_project, f"Metashape_Project_{site_name}.psx"))
        
    os.remove(os.path.join(out_dir_ortho, '_temp_.psx'))
    shutil.rmtree(os.path.join(out_dir_ortho, '_temp_.files'))
    

if __name__ == '__main__':

    print("args : ", args)
    Time_SIFT_process(pathDIR = args.pathDIR, 
                      out_dir_ortho = args.out_dir_ortho, 
                      out_dir_DEM = args.out_dir_dem, 
                      out_dir_project = args.out_dir_project,
                      data_type = args.data_type, 
                      resol_ref = args.resol_ref, 
                      crs = args.crs,
                      site_name = args.site_name,
                      calibrate_col = args.calibrate_col,
                      sun_sensor = args.sun_sensor,
                      group_by_flight = args.group_by_flight,
                      downscale_factor_alignement = args.downscale_factor_alignement,
                      downscale_factor_depth_map = args.downscale_factor_depth_map
                      )
