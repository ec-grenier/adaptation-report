# This script is designed to pull all extractions of the labor impacts from the projections that are needed for the inequality paper
# It defines bins based on the finding_gwl_bins.R script please double check you are using the most up to date Global Warming Level (GWL)  info and the correct models for each level
# GWL binlist was updated with current GWL as of 2-8-23
# More information on using the quantiles.py script can be found in the RA Manual under Extractions and in the documentation for James Rising's prospectus-tools repo

#Set directories
REPO="/project/cil/home_dirs/egrenier/repos" #confirm path points to your repos folder, if you follow CLI convention this should not change 
results_root="/project/cil/gcp/outputs/labor/impacts-woodwork/montecarlo/uninteracted_main_model" #path to Monte carlos to be used 
DB="/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/extracted"  #path to where data should be placed
output="${DB}/labor"
mkdir -p ${output}

#Set up log file, since this program runs through many iterations I felt it prudent to write a log rather than have it spit up in the terminal
NOW=$(date --iso-8601=seconds)
CUR_SCRIPT=$0
LOG_DIR="${DB}/logs"
mkdir -p ${LOG_DIR}
LOG_FILE="${LOG_DIR}/${CUR_SCRIPT}_labor_${NOW}.txt"

echo "Projection system outputs pulled from ${results_root}" >> "${LOG_FILE}"
echo "Extraction output directory is ${output}" >> "${LOG_FILE}"

# Iterable arguments
binlist=( 3_c_midc_a 3_c_midc_b )
ssplist=( SSP2 ) # SSP2 only 
iamlist=( low ) # low, high
risklist=( allrisk )  # allrisk, highrisk, lowrisk, riskshare
scnlist=( fulladapt ) # noadapt incadapt fulladapt
spatiallist=( ir_level ) #pop-aggregated, wage-aggregated, gdp-aggregated, ir_level
# unit for labor is daily minutes worked per person

# Fixed arguments
basename=uninteracted_main_model #This value changes with the MCs used

# Setting up the correct environment
CONFIG=${REPO}/../cil-comms/adaptation_report/code/extract_physical/extract_labor_config.yml
echo "Config file used ${CONFIG}" >> "${LOG_FILE}"

cd ${REPO}/prospectus-tools/gcp/extract
echo "Repo is now ${REPO}" >> "${LOG_FILE}"


#Iternate through each combination
for bin in ${binlist[@]}; do

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
					for risk in ${risklist[@]}; do
						for scn in ${scnlist[@]}; do
                            
							insuffix=""
							outsuffix="-${bin}-no_surrogate-${scn}-${risk}-${iam}"

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
									post="none"
									;;
								incadapt)
									insuffix+="-${scn}"
									if [ "$risk" != "riskshare" ]; then                                    
										post=histclim
									elif [ "$risk" = "riskshare" ]; then
										post="none"
									fi
									;;
								fulladapt)
									if [ "$risk" != "riskshare" ]; then                                    
										post=histclim
									elif [ "$risk" = "riskshare" ]; then
										post="none"
									fi
									;;
							esac
                            
							case $risk in  
								allrisk)
									valcol="--column=rebased"
									;;
								highrisk)
									valcol="--column=highriskimpacts"
									;;
								lowrisk)
									valcol="--column=lowriskimpacts"
									;;
								riskshare)
									valcol="--column=clip"
									;;                                    
							esac

							endtag=""
							if [ "$spatial" != "ir_level" ]; then
								endtag+="-${spatial}"
								outsuffix+="-${spatial}"
							elif [ "$spatial" = "ir_level" ]; then
								endtag+=""
								outsuffix+="-ir_level"
							fi
                            
							if [ "$risk" != "riskshare" ]; then                                    
								outsuffix+="-min-$format"
							elif [ "$risk" = "riskshare" ]; then
								outsuffix+="-$format"
							fi                            

							srcfile="${basename}${insuffix}${endtag}"
							if [ "$post" = "histclim" ]; then
								srcfile+=" -${basename}-histclim${endtag}"
							fi
							echo "Input scenario ${srcfile}" >> "${LOG_FILE}"

							echo "Extracting ${rcp}-${ssp}${outsuffix}.csv for years ${yearlist} and GCMs ${only_models}" >> "${LOG_FILE}"
							python quantiles.py ${CONFIG} --results-root=${results_root} --output-format=${format} --only-rcp=${rcp} --only-iam=${iam} --only-ssp=${ssp} --only-models=${only_models} --yearsets=${yearlist} --suffix=${outsuffix} ${valcol} --output-dir=${output} ${srcfile} >> "${LOG_FILE}"

							echo "Finished extracting ${rcp}-${ssp}${outsuffix}.csv"
						done
					done
				done
			done
		done
	done
done
