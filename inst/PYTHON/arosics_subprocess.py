import logging 
import subprocess
import os

def launch_arosics_in_subproc(path_in, 
                             ref_filepath, 
                             out_dir_path, 
                             corr_type = "global", 
                             max_shift=250,
                             max_iter=5, 
                             min_reliability=60,
                             grid_res=None, 
                             window_size=None, 
                             window_pos = (None, None), 
                             mp=None, 
                             compress_lzw=False, 
                             save_data = True, 
                             save_vector_plot = False, 
                             apply_matrix=False, 
                             suffix = "",) :
    

    logging.basicConfig(level=logging.INFO)
    mp = mp if mp is None else int(mp)
    window_size = window_size if window_size is None else int(window_size)
    command = f"python D:/githubs/canObsR/inst/PYTHON/arosics_chain.py --path_in={path_in} --ref_filepath={ref_filepath} --out_dir_path={out_dir_path} --corr_type={corr_type} --max_shift={int(max_shift)} --max_iter={int(max_iter)} --ws={window_size} --mp={mp} --grid_res={int(grid_res)} --min_reliability={int(min_reliability)} --compress_lzw={compress_lzw} --apply_matrix={apply_matrix} --save_data={save_data} --save_plot={save_vector_plot} --suffix={suffix}"   #{'' if window_pos==(None, None) else f'--wp={window_pos}'}

    if window_pos!=(None, None) :
        assert len(window_pos)==2
        wp = f"({int(window_pos[0])},{int(window_pos[1])})"
        print(wp)
        print(wp.strip("()").split(","))
        print(tuple(map(int, wp.strip("()").split(","))))
        command += f" --wp=({int(window_pos[0])},{int(window_pos[1])})"
    
    print(command)

    try:
        logging.info(f"Running command: {command}")
        result = subprocess.run(command, shell=True, check=True, capture_output=True, text=True)
        logging.info(f"Command output: {result.stdout}")
    except subprocess.CalledProcessError as e:
        logging.error(f"Command failed with error: {e.stderr}")
    except Exception as e:
        logging.error(f"Unexpected error: {str(e)}")