import os
import subprocess
import sys
import argparse
import glob

def run_command(command, check=True):
    """Run a command and return its output."""
    print(f"Executing: {command}")
    try:
        result = subprocess.run(
            command, 
            check=check, 
            shell=True, 
            stdout=subprocess.PIPE, 
            stderr=subprocess.PIPE,
            universal_newlines=True
        )
        print(result.stdout)
        if result.stderr:
            print(f"Error output: {result.stderr}")
        return result
    except subprocess.CalledProcessError as e:
        print(f"Command failed with return code {e.returncode}")
        print(f"Error: {e.stderr}")
        if check:
            sys.exit(1)
        return e

def process_fortran_file(fortran_file, sapfor_dir):
    """Process a single Fortran file."""
    # Extract file name without extension
    file_name = os.path.splitext(fortran_file)[0]
    
    print(f"\n{'='*50}")
    print(f"Processing file: {fortran_file}")
    print(f"{'='*50}\n")
    
    # 1. Compile the Fortran file with profiling options
    compile_cmd = f"gfortran -O3 -g -fprofile-arcs -ftest-coverage {fortran_file} -o {file_name}"
    result = run_command(compile_cmd, check=False)
    
    if result.returncode != 0:
        print(f"Compilation failed for {fortran_file}, skipping...")
        return False
    
    # 2. Run the compiled program
    run_result = run_command(f".\\{file_name}", check=False)
    
    if run_result.returncode != 0:
        print(f"Execution failed for {file_name}, but continuing with analysis...")
    
    # 3. Generate coverage information
    run_command(f"gcov -b {fortran_file}", check=False)
    
    # 4. Check if the .gcov file was created
    gcov_file = f"{fortran_file}.gcov"
    if not os.path.exists(gcov_file):
        print(f"Warning: {gcov_file} was not created. Continuing anyway...")
    else:
        print(f"Successfully created coverage file: {gcov_file}")
    
    # 5. Parse the Fortran program with SAPFOR
    parse_cmd = f"{sapfor_dir}\\Sapfor_F -parse -spf {fortran_file}"
    parse_result = run_command(parse_cmd, check=False)
    
    if parse_result.returncode != 0:
        print(f"SAPFOR parsing failed for {fortran_file}, skipping analysis...")
        return False
    
    # 6. Run SAPFOR analysis
    analysis_cmd = f"{sapfor_dir}\\Sapfor_F -passN GET_STATS_FOR_PREDICTOR -keepDVM"
    analysis_result = run_command(analysis_cmd, check=False)
    
    # 7. Check if the analysis was successful and save results
    output_dir = os.path.join("results", file_name)
    os.makedirs(output_dir, exist_ok=True)
    
    # Move results to the output directory
    if os.path.exists("stats.csv"):
        os.rename("stats.csv", os.path.join(output_dir, "stats.csv"))
        print(f"Statistics saved to {output_dir}/stats.csv")
    
    if os.path.exists("info.json"):
        os.rename("info.json", os.path.join(output_dir, "info.json"))
        print(f"JSON data saved to {output_dir}/info.json")
    
    # Clean up generated files
    try:
        os.remove(f"{file_name}.exe")
        os.remove(f"{file_name}.gcda")
        os.remove(f"{file_name}.gcno")
        if os.path.exists("dvm.proj"):
            os.remove("dvm.proj")
        for dep_file in glob.glob("*.dep"):
            os.remove(dep_file)
    except Exception as e:
        print(f"Warning: Clean-up error - {e}")
    
    return True

def main():
    parser = argparse.ArgumentParser(description='Run SAPFOR analysis workflow on multiple Fortran files')
    parser.add_argument('--dir', help='Directory containing Fortran files', default='.')
    parser.add_argument('--pattern', help='Pattern for Fortran files (e.g., "*.f")', default='*.f')
    parser.add_argument('--sapfor_dir', help='Path to SAPFOR directory', default='D:\\Diploma\\SAPFOR\\_bin\\Release')
    args = parser.parse_args()
    
    # Create results directory
    os.makedirs("results", exist_ok=True)
    
    # Find all Fortran files in the specified directory
    fortran_files = glob.glob(os.path.join(args.dir, args.pattern))
    
    if not fortran_files:
        print(f"No Fortran files found matching pattern '{args.pattern}' in directory '{args.dir}'")
        sys.exit(1)
    
    print(f"Found {len(fortran_files)} Fortran files to process:")
    for f in fortran_files:
        print(f"  - {f}")
    
    # Process each file
    successful = 0
    failed = 0
    
    for fortran_file in fortran_files:
        if process_fortran_file(fortran_file, args.sapfor_dir):
            successful += 1
        else:
            failed += 1
    
    print(f"\n{'='*50}")
    print(f"Processing completed: {successful} successful, {failed} failed")
    print(f"{'='*50}")

if __name__ == "__main__":
    print('hello')
    main()