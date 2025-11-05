# This script is designed to pull all extractions of the mortality impacts from the projections that are needed for the inequality paper
# It defines bins based on the finding_gwl_bins.R script please double check you are using the most up to date Global Warming Level (GWL)  info and the correct models for each level
# GWL binlist was updated with current GWL as of 2-8-23
# More information on using the quantiles.py script can be found in the RA Manual under Extractions and in the documentation for James Rising's prospectus-tools repo

#Set directories
REPO="/project/cil/home_dirs/egrenier/repos" #confirm path points to your repos folder, if you follow CLI convention this should not change 
results_root="/project/cil/battuta-shares-S3-archive/gcp/outputs/mortality/impacts-darwin/montecarlo" #path to Monte carlos to be used
	#Currently pointing mortality MCs that corrected Mexico bug up to date as of Fall 2022
DB="/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/extracted"  #path to where data should be placed
output=${DB}/mortality
mkdir -p ${output}

#Set up log file, since this program runs through many iterations I felt it prudent to write a log rather than have it spit up in the terminal
NOW=$(date --iso-8601=seconds)
CUR_SCRIPT=$0
LOG_DIR="${DB}/logs"
mkdir -p ${LOG_DIR}
LOG_FILE="${LOG_DIR}/${CUR_SCRIPT}_${NOW}.txt"

echo "Monte Carlos pulled from ${results_root}" >> "${LOG_FILE}"
echo "Output directory is ${output}" >> "${LOG_FILE}"

# Iterable arguments
binlist=( 3_c_midc_a 3_c_midc_b ) # 2_c_midc 3_c_midc_a 3_c_midc_b 4_c_midc 3_c_endc_a 3_c_endc_b 
ssplist=( SSP2 ) # only SSP2
iamlist=( low ) # only IIASA for this
agelist=( combined )  # young, older, oldest, combined
scnlist=( incadapt ) # noadapt incadapt fulladapt costs fulladaptcosts
spatiallist=( aggregated ) #aggregated, ir_level 
unitslist=( rates ) #rates, levels

# NOTE: when extracting fulladaptcosts you will get the correct rates even though the rebased variable in fulladapt is in deaths/person/year and costs are in deaths/100,000/year. quantiles.py does some in the box stuff (divide costs by 100,000) to make costs into rates. 

# Fixed arguments
basename=Agespec_interaction_response
CONFIG=${REPO}/../cil-comms/adaptation_report/code/extract_physical/extract_mortality_config.yml
echo "Config file used ${CONFIG}" >> "${LOG_FILE}"

cd ${REPO}/prospectus-tools/gcp/extract
echo "Repo is now ${REPO}" >> "${LOG_FILE}"

#Iternate through each combination
for bin in ${binlist[@]}; do

	# Here we bin models according to 
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
                3_c_endc_a)
                        only_models=[ACCESS1-0,BNU-ESM,CanESM2,CSIRO-Mk3-6-0,IPSL-CM5A-LR,IPSL-CM5A-MR,MIROC-ESM,MIROC-ESM-CHEM]
                        rcplist=(rcp45)
            			yearlist=[[2080,2099]]
                		format="valuescsv" # edfcsv, valuescsv
                        ;;
                3_c_endc_b)
                        only_models=[GFDL-ESM2M,inmcm4]
                        rcplist=(rcp85)
            			yearlist=[[2080,2099]]
            			format="valuescsv" # edfcsv, valuescsv
                        ;;
        esac

	for spatial in ${spatiallist[@]}; do
		for unit in ${unitslist[@]}; do
			for rcp in ${rcplist[@]}; do
				for ssp in ${ssplist[@]}; do
					for iam in ${iamlist[@]}; do
						for age in ${agelist[@]}; do
							for scn in ${scnlist[@]}; do

								insuffix="-${age}"
								outsuffix="-${bin}-no_surrogate-${scn}-${age}-${iam}"

								#The scenario used effects several things
									#Which netcdf should be used as input
									#If the column arguement is needed as input in to quantiles.py
									#If historical climate (hisclim) should be subtracted from and/or adaption costs added to the final impact
								#The generation of the insuffix and endtag are needed to generate the correct input files fed to quantiles.py
								#valcol determines if the column argument is used
								#srcfile sets if any projections need to be subtracted or added from the final impact
								case $scn in  
									noadapt)
										insuffix+="-${scn}"
										valcol="--column=rebased"
										post="none"
										;;
									incadapt)
										insuffix+="-${scn}"
										valcol="--column=rebased"
										post=histclim
										;;
									fulladapt)
										valcol="--column=rebased"
										post=histclim
										;;
									costs)
										insuffix+="-${scn}"
										valcol="--column=costs_ub"
										post="none"
										;;
									fulladaptcosts)
										valcol=""
										post=costs
										;;
								esac

								endtag=""
								if [ "$spatial" = "aggregated" ]; then
									endtag+="-aggregated"
									outsuffix+="-aggregated"
								elif [ "$spatial" = "ir_level" ]; then
									endtag+=""
									outsuffix+="-ir_level"
								fi

								if [ "$unit" = "rates" ]; then
									endtag+=""
									outsuffix+="-rates-$format"
								elif [ "$unit" = "levels" ]; then
									endtag+="-levels"
									outsuffix+="-levels-$format"
								fi

								srcfile="${basename}${insuffix}${endtag}"
								if [ "$post" = "histclim" ]; then
									srcfile+=" -${basename}-${age}-histclim${endtag}"
								elif [ "$post" = "costs" ]; then
									srcfile+=" -${basename}-${age}-histclim${endtag} ${basename}-${age}-costs${endtag}"
								fi
								echo "Input scenario ${srcfile}" >> "${LOG_FILE}"

								echo "Extracting ${rcp}-${ssp}${outsuffix}.csv for years ${yearlist} and GCMs ${only_models}" >> "${LOG_FILE}"
                                python quantiles.py ${CONFIG} --results-root=${results_root} --output-format=${format} --only-rcp=${rcp} --only-iam=${iam} --only-ssp=${ssp} --only-models=${only_models} --yearsets=${yearlist} --suffix=${outsuffix} --output-dir=${output} ${srcfile} >> "${LOG_FILE}"

								echo "Finished extracting ${rcp}-${ssp}${outsuffix}.csv"
							done
						done
					done
				done
			done
		done
	done
done

