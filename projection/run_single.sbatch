#!/bin/bash
#SBATCH --job-name=mortaltiy-rcp85-inmcm4
#SBATCH --output=/project/cil/home_dirs/egrenier/repos/projections/output/mortality/single/rcp85-inmcm4.out
#SBATCH --error=/project/cil/home_dirs/egrenier/repos/projections/error/mortality/single/rcp85-inmcm4.err
#SBATCH --account=cil
#SBATCH --partition=caslake
#SBATCH --time=36:00:00
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=40G

#Load in required modules
module load python

#Activate environment
cd /project/cil/home_dirs/egrenier/repos/impact-calculations/
source activate /project/cil/home_dirs/egrenier/envs/impact-env

#Run projection
echo "Running projection..."
./generate.sh /project/cil/home_dirs/egrenier/repos/mortality/2_projection/2_run_projections/main_specification/configs/mortality-generate-single.yml

conda deactivate