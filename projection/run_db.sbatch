#!/bin/bash
#SBATCH --job-name=mortality-db
#SBATCH --output=/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/projection/SLURM_output/mortality-db-%A-%a.out
#SBATCH --account=cil
#SBATCH --partition=caslake
#SBATCH --time=36:00:00
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --array=0-199

# Time the whole process
start_time=$(date +%s)

# load in required modules
module load netcdf
module load curl/7.79.1
module load udunits
module load geos/3.9.1
module load sqlite/3.36.0
module load proj/7.2
module load gdal/3.3.3
module load gcc/12.2.0 
module load python/anaconda-2022.05
module load R/4.2.1

# navigate to directory
cd /project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/projection

# distribute the workload
rcp_gcm_list=(
  "rcp45 ACCESS1-0"
  "rcp45 BNU-ESM"
  "rcp45 CanESM2"
  "rcp45 CSIRO-Mk3-6-0"
  "rcp45 IPSL-CM5A-LR"
  "rcp45 IPSL-CM5A-MR"
  "rcp45 MIROC-ESM"
  "rcp45 MIROC-ESM-CHEM"
  "rcp85 GFDL-ESM2M"
  "rcp85 inmcm4"
)

years=($(seq 2040 2059))

# Total combinations
num_rcp_gcm=${#rcp_gcm_list[@]}
num_years=${#years[@]}

# Flatten index
idx=${SLURM_ARRAY_TASK_ID}
rcp_gcm_index=$(( idx / num_years ))
year_index=$(( idx % num_years ))

read rcp gcm <<< "${rcp_gcm_list[$rcp_gcm_index]}"
year=${years[$year_index]}

echo "Running task: rcp=$rcp, gcm=$gcm, year=$year"

#Run projection
echo "Running delta beta..."

Rscript 1_mortality_db_script.R "$rcp" "$gcm" "$year" 2>&1

# Capture end time after run finishes
end_time=$(date +%s)
duration=$(( end_time - start_time ))
echo "Job completed at $(date)"
echo "Total elapsed time: ${duration} seconds"
