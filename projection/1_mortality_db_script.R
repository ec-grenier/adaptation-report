#==============================================================================#
#' Mortality Delta Beta Script
#'
#' to run: Rscript 1_mortality_db_script.R <rcp> <gcm> <year>   
#==============================================================================#

#==============================================================================#
# 0. Source, set packages and paths -----------
library(glue)

REPO = '/project/cil/home_dirs/egrenier/repos'
source(glue("/project/cil/home_dirs/egrenier/repos/post-projection-tools/response_function/yellow_purple_package.R"))
source(glue("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/projection/2_mortality_db_wrapper.R"))

# get command line args
args = commandArgs(trailingOnly = TRUE)

# Set paths below
csvv_dir = glue("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/csvv/") # Should have trailing slash
csvv_name = "Agespec_interaction_response.csvv"
cov_dir = glue("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/covars")
out_dir = glue("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/deltabeta") 

#==============================================================================#
# 1. Set globals ----

age_list = c(3,2,1)
het_list = list(age = c(rep.int(1, 12),rep.int(2, 12),rep.int(3, 12))) # to define agegroups for csvv coeffs

rcp = args[1]
gcm = args[2]
year = as.numeric(args[3])

# rcp = 'rcp45'
# gcm = 'ACCESS1-0'
# year = 2050

ssp = "SSP2" # Hard coded. for now not changing. easy to change by including in CLI args
iam = "low"

cov_dir = glue("{cov_dir}/{rcp}/{gcm}/{iam}/{ssp}/mortality-econ_clim.csv")

# Get list of regions to loop over 
region_list = list(unique((fread(cov_dir))$region))

# Testing with subsets or specific regions
# region_list = region_list[[1]][1:3] # any range in [1,24378]
#region_list = list('BRA.22.4329.Raaa3e9be8ca4501a', 'AUS.6.761', 'OMN.4.25', 'BWA.7', 'MEX.23.1231') # <- REGIONS NEAR 0 IMPACTS
#region_list = list('PAK.2.4.23')#, 'BFA.29', 'NER.1.3.4', 'DZA.1.16', 'SDN.6.15.72.223') # <- DELTABETA IMPACTS MUCH HIGHER
#region_list = list('USA.26.1540', 'USA.14.649') # <- PROJ HAS NEGATIVE IMPACTS, SIGN FLIP IN DB
#region_list = list('CAN.8.117.2359')#, 'RUS.65.1741.1858', 'CAN.8.117.2350', 'RUS.5.108.108') # <- Cold deaths underestimated
#region_list = list('PAK.2.4.23')

slug=''

#-----set args here-----
args = list(years=year,
            base_year=1993,
            rebase_year=2005,
            bound_to_hist=F,
            csvv.dir=csvv_dir,
            csvv.name=csvv_name,
            het.list=het_list,
            cov.dir=cov_dir,
            covarkey='region',
            list.names=c('climtas','loggdppc'),
            covar.names=c('climtas','loggdppc'),
            func=get.clipped.curve.mortality,
            get.covars=T,
            tas_value="tas",
            ncname="1.6",
            TT_upper_bound=62,
            TT_lower_bound=-62,
            TT_step=1,
            do.clipping=T,
            goodmoney.clipping=T, 
            do.diffclip=T,
            full_db=F, # this should be F for projection approximations, T for plotting curves
            do_global=F,
            delta.beta=F, # TRUE IF YOU WANT PLOTS
            return.db=T,
            rel.20 = T,
            save.plot=F
            )

#==============================================================================#
# 2. Run Function ----

# create output directory 
out = glue("{out_dir}/{rcp}/{gcm}/{iam}/{ssp}")
dir.create(out, recursive=T)
message(glue("[Running delta beta: ] {rcp} {gcm} {iam} {ssp}\n"))

for (age in age_list){
  
  agegroup = ifelse(age==3, "oldest", ifelse(age==2, "older", "young"))
  
  message(glue("[age group: ] {agegroup} \n"))
  
  df = lapply(region_list, get_all_db_tables, age, args)
  df = do.call(rbind, df) %>% as.data.frame() %>% select(region, bin, effect_fa)
  #df = df %>% filter(bin == "hot" | bin == "cold" | bin == "Total")
  
  file_name = glue("mortality-delta_beta-fulladapt-{year}-{agegroup}-old_db.csv")
  output = glue("{out}/{file_name}")
  
  message('\n[ saving ]\n')
  message(glue("Saving: {file_name}\nOutput directory: {out}\n"))
  #cat(df %>% format_tsv())
  write.csv(df, output, row.names=FALSE)
  message("\n[ saved ]\n")
  
}

message(glue("[ all age groups completed ]"))


