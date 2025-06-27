import logging 
import subprocess
import os
import argparse

def parse_tuple(arg):
    try:
        # Evaluate the string to convert it to a tuple
        return tuple(map(int, arg.strip("()").split(",")))
    except ValueError:
        raise argparse.ArgumentTypeError(f"Invalid tuple format: '{arg}'. Expected format: '(int,int)'")
    
parser = argparse.ArgumentParser()
parser.add_argument('--path_in', type=str)
parser.add_argument('--ref_filepath',type=str)
parser.add_argument('--out_dir_path', type=str)
parser.add_argument('--corr_type', type=str, default='global')
parser.add_argument('--mp', default=None)
parser.add_argument('--max_shift', type=int, default=250)
parser.add_argument('--max_iter', type=int, default=5)
parser.add_argument('--ws', default=None)
parser.add_argument('--wp', type=parse_tuple, default=(None, None))
parser.add_argument('--min_reliability', type=int, default=60)
parser.add_argument('--grid_res', type=int, default=None)
parser.add_argument('--apply_matrix', default=False)
parser.add_argument('--save_plot', default=False)
parser.add_argument('--save_data', default=True)
parser.add_argument('--compress_lzw', default=False)
parser.add_argument('--suffix', type=str, default="")
parser.add_argument('--subprocess', default=False)
args = parser.parse_args()

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
    command = f"python D:/githubs/canObsR/inst/PYTHON/arosics_chain.py --path_in={path_in} --ref_filepath={ref_filepath} --out_dir_path={out_dir_path} --corr_type={corr_type} --max_shift={int(max_shift)} --max_iter={int(max_iter)} --min_reliability={int(min_reliability)} --compress_lzw={compress_lzw} --apply_matrix={apply_matrix} --save_data={save_data} --save_plot={save_vector_plot} --suffix={suffix}"

    if grid_res!=None:
        command += f"--grid_res={int(grid_res)}"

    if mp!=None:
        command += f"--mp={int(mp)}"

    if window_size!=None:
        command += f"--ws={int(window_size)}"

    if window_pos!=(None, None) :
        assert len(window_pos)==2
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


if __name__ == '__main__':
    print(args)
    launch_arosics_in_subproc(path_in = args.path_in,
                              ref_filepath = args.ref_filepath, 
                              out_dir_path = args.out_dir_path, 
                              corr_type = args.corr_type, 
                              mp = args.mp,
                              max_shift = args.max_shift,
                              max_iter = args.max_iter,
                              grid_res = args.grid_res,
                              min_reliability = args.min_reliability,
                              window_pos = args.wp,
                              window_size = args.ws,
                              apply_matrix = args.apply_matrix,
                              save_data = args.save_data,
                              save_vector_plot = args.save_plot,
                              compress_lzw = args.compress_lzw,
                              suffix = args.suffix,
                              )