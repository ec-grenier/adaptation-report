# This script is designed to pull all extractions of the mortality impacts from the projections that are needed for the inequality paper
# It defines bins based on the finding_gwl_bins.R script please double check you are using the most up to date Global Warming Level (GWL)  info and the correct models for each level
# GWL binlist was updated with current GWL as of 3-29-23
# More information on using the quantiles.py script can be found in the RA Manual under Extractions and in the documentation for James Rising's prospectus-tools repo
# This is only apprioriate to uses for Ag extractions in the inequality paper. We do not weight gcms inequality. If you are modifying this script for another purpose and need to weight extractions by gcm again please see ag repo for how to do that properly.

#Set directories
REPO="/project/cil/home_dirs/egrenier/repos" #confirm path points to your repos folder, if you follow CLI convention this should not change
DB="/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/extracted"  #path to where data should be placed
output="${DB}/agriculture"
mkdir -p ${output}

#Set up log file, since this program runs through many iterations I felt it prudent to write a log rather than have it spit up in the terminal
NOW=$(date --iso-8601=seconds)
CUR_SCRIPT=$0
LOG_DIR="${DB}/logs"
mkdir -p ${LOG_DIR}
LOG_FILE="${LOG_DIR}/${CUR_SCRIPT}_${NOW}.txt"

echo "Output directory is ${output}" >> "${LOG_FILE}"

# Iterable arguments
# Iterable arguments
binlist=( 3_c_midc_a 3_c_midc_b )
ssplist=( SSP2 ) # SSP2 only 
iamlist=( low ) # low, high
scn=( fulladapt ) # noadapt incadapt fulladapt
crop=( allcrops ) 
spatiallist=( ir_level ) #pop-aggregated, wage-aggregated, gdp-aggregated, ir_level
# unit for labor is daily minutes worked per person

# Setting up the correct environment
CONFIG=${REPO}/../cil-comms/adaptation_report/code/extract_physical/extract_agriculture_config.yml
echo "Config file used ${CONFIG}" >> "${LOG_FILE}"

cd ${REPO}/prospectus-tools/gcp/extract
echo "Repo is now ${REPO}" >> "${LOG_FILE}"


#Iternate through each combination
results_root="/project/cil/gcp/outputs/agriculture/agval/adaptation_report/montecarlo" #path to Monte carlos to be used

# Note the montecarlo directories and file basenames have the a suffix with the data the projection was run on
	# I know these date are correct as of 3-29-23
	# If the projections have been rerun then these suffixes will change and the script will need to be updated

	# Setting correct paths based on crop names
echo "Monte Carlos pulled from ${results_root}" >> "${LOG_FILE}"


basename="disaggregated_damages"


echo "The current basename is ${basename}" >> "${LOG_FILE}"

for bin in ${binlist[@]}; do

		#Defining the correct GCM models and RCP level for each Global Warming Level and time period combo
		#1.5 c in mid century uses valuescsv not edfcsv because we need to get quanitles over two different groups of models with two different rcp levels
			#so we need to combine the underlying values from both subgroup a and b to reconstruct the distribution and quantiles for the whole bin
		#Note for Ag only ACCESS1-0 at rcp8.5 should not be extracted, the GWL bins as of 3-29-23 do not have this issue but if the GWLs change please double check this
    	case ${bin} in
                2_c_midc)
                        only_models=[bcc-csm1-1,CESM1-BGC,GFDL-ESM2M,inmcm4,MPI-ESM-LR,MRI-CGCM3,NorESM1-M]
                        rcplist=(rcp45)
			yearlist=[[2040,2059]]
			format="edfcsv"
                        ;;
                3_c_midc_a)
                        only_models=[ACCESS1-0,BNU-ESM,CanESM2,CSIRO-Mk3-6-0,IPSL-CM5A-LR,IPSL-CM5A-MR,MIROC-ESM,MIROC-ESM-CHEM]
                        rcplist=(rcp45)
			yearlist=[[2040,2059]]
			format="valuescsv"
                        ;;
                3_c_midc_b)
                        only_models=[GFDL-ESM2M,inmcm4]
                        rcplist=(rcp85)
			yearlist=[[2040,2059]]
			format="valuescsv"
                        ;;
                4_c_midc)
                        only_models=[bcc-csm1-1,CCSM4,CESM1-BGC,CNRM-CM5,MIROC5,MPI-ESM-LR,MPI-ESM-MR,MRI-CGCM3,NorESM1-M]
                        rcplist=(rcp85)
			yearlist=[[2040,2059]]
			format="edfcsv"
                        ;;
        esac
    
    for spatial in ${spatiallist[@]}; do	
        for rcp in ${rcplist[@]}; do
    		for ssp in ${ssplist[@]}; do
    			for iam in ${iamlist[@]}; do
     
					valcol="--column=delta_no_reallocation"
					post="none"
                	outsuffix="-${bin}-no_surrogate-${scn}-${crop}-${iam}"
	
					if [ "$spatial" = "aggregated" ]; then
						endtag+="-aggregated"
						outsuffix+="-aggregated"
					elif [ "$spatial" = "ir_level" ]; then
						endtag+=""
						outsuffix+="-ir_level"
					fi

					#adding a unit so it matches the other sectors
   					outsuffix+="-delta_cals-${format}"

					srcfile="${basename}"
					echo "Input scenario ${srcfile}" >> "${LOG_FILE}"

                    echo "Extracting ${rcp}-${ssp}${outsuffix}.csv for years ${yearlist} and GCMs ${only_models}" >> "${LOG_FILE}"
					python quantiles.py ${CONFIG} --results-root=${results_root} --output-format=${format} --only-rcp=${rcp} --only-iam=${iam} --only-ssp=${ssp} --only-models=${only_models} --yearsets=${yearlist} --suffix=${outsuffix} ${valcol} --output-dir=${output} ${srcfile} >> "${LOG_FILE}"

					echo "Finished extracting ${rcp}-${ssp}${outsuffix}.csv"
				done
			done
        done
	done
done

