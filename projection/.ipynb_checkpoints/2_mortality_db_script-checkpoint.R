#====================================================================================#
#' Mortality Delta Beta Script
#'
#' Author: Elliot Grenier (egrenier@uchicago.edu)
#' Date Created: Feb 10 2025
#' Last Modified: Aug 21 2025
#'
#' Description:
#
#'  This script runs energy delta-betas for all impact regions. 
#'  Currently set up to only run one ssp-iam-rcp-gcp scenario combination at a time. 
#'   
#' How To Run:
#'   - Pick run spec (agegroups + climate/income scenario)
#'   - Pick true/false for variables global and full_db
#'   - Run on a compute node through command line (Rscript db_wrapper.R) or SLURM job
#'
#====================================================================================#

# 0. Source, set packages and paths -----------

# Time the whole process
start_time = Sys.time()

library(glue)
source("/project/cil/home_dirs/egrenier/repos/regional-scc/utils/paths.R")
source(glue("{REPO}/regional-scc/data/response_function/functions/mortality_db_wrapper.R"))

#===================================#
# 1. Set globals ----
#===================================#

agelist = c(3,2,1)
het.list = list(age = c(rep.int(1, 12),rep.int(2, 12),rep.int(3, 12))) # to define agegroups for csvv coeffs

gcm = "CCSM4"
rcp = "rcp45"
ssp = "SSP2"
iam = "low"

# This loads in global covars. NOTE: a random region will be chosen for the "delta" portion.
# running this for global only returns the global beta. The "delta" here should be ignored and is
# just passed through the script to ensure that it functions as intended (see mortality_db_wrapper.R for specifics)

global = F  # <- pick T or F here 
g_slug = ifelse(global, "-global", "")
  
# Set paths below
csvv.dir = glue("{DB}/deltabeta/input/csvv/mortality/") # Should have trailing slash
csvv.name = "Agespec_interaction_response.csvv"
cov.dir = glue("{DB}/deltabeta/input/covariates/econ_clim-{gcm}-{rcp}-{ssp}-{iam}{g_slug}.csv")
output.dir = glue("{DB}/deltabeta/output/mortality/") 

# Get list of regions to loop over 
region_list = list(unique((fread(cov.dir))$region))

# Testing with subsets or specific regions
# region_list = region_list[[1]][1:1] # any range in [1,24378]
# region_list = list('GHA.5.70')

#-----set args here-----
args = list(years=2099,
            base_year=1993,
            rebase_year=2005,
            csvv.dir=csvv.dir,
            csvv.name=csvv.name,
            het.list=het.list,
            cov.dir=cov.dir,
            covarkey='region',
            list.names=c('climtas','loggdppc'),
            covar.names=c('climtas','loggdppc'),
            func=get.clipped.curve.mortality,
            get.covars=T,
            tas_value="tas",
            ncname="1.6",
            TT_upper_bound=100,
            TT_lower_bound=-100,
            TT_step=1,
            do.clipping=T,
            goodmoney.clipping=T, 
            do.diffclip=T,
            full_db=T, # <- pick T or F here 
            do_global=global
            )


#===================================#
# 2. Run Function ----
#===================================#
stored=data.frame()
for (age in agelist){
  df = lapply(region_list, get_all_db_tables, age, args)
  df = do.call(rbind, df) %>% as.data.frame()
  
  agegroup = ifelse(age==3, "oldest", ifelse(age==2, "older", "young"))
  
  df = df %>% mutate(age = agegroup)
  stored = rbind(stored, df)
  save_sector_output(df=df, agegroup=agegroup, ssp=ssp, iam=iam, rcp=rcp, gcm=gcm,
                     year=args$years, out=output.dir, full_db=args$full_db, global=args$do_global,
                     slug='')
}

# Get time
time = Sys.time() - start_time
message(time)
