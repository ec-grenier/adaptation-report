#=================================================================================#
# Step 0: Extract covariates
#
# Author: Elliot Grenier (egrenier@uchicago.edu)
# 
# Description:
#'   
#'   Pulls long run income and climate covariates needed for delta beta
#'   calculations. Takes long-run income (13 half bartlett kernel) and long-run 
#'   average temperature (30 year average) from mortality projections
#'   
# How to run:
#'
#'  Run from command line `Rscript 0_extract_covariates.R` or in the IDE of your
#'  chooosing.
#  
#=================================================================================#

#=================================================================================#
# packages and paths ----

packages = c("glue", "dplyr", "readr")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

# projection system allcalcs file parent directories
root = "/project/cil/gcp/outputs/mortality/impacts-darwin/single/single-aug2025"
out_dir = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/covars"
allcalcs = "tests.configs.mortality.allmodels-allcalcs-Agespec_interaction_response-oldest.csv" # all age group allcalcs files are the same
adjust = F

# specifications we need covariates for
specs = list(
  list(rcp = "rcp45", gcm = "ACCESS1-0", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "BNU-ESM", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "CanESM2", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "CSIRO-Mk3-6-0", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "IPSL-CM5A-LR", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "IPSL-CM5A-MR", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "MIROC-ESM", iam="low", ssp="SSP2"),
  list(rcp = "rcp45", gcm = "MIROC-ESM-CHEM", iam="low", ssp="SSP2"),
  list(rcp = "rcp85", gcm = "GFDL-ESM2M", iam="low", ssp="SSP2"),
  list(rcp = "rcp85", gcm = "inmcm4", iam="low", ssp="SSP2")
)

#=================================================================================#
# Extract ----

for (spec in specs){
  
  ssp = spec$ssp
  iam = spec$iam
  rcp = spec$rcp
  gcm = spec$gcm
  
  message("[Running: ] ", ssp, ", ", iam, ", ", rcp, ", ", gcm)

  # get loggdppc and climtas from mortality
  clim_inc = suppressMessages(read_csv(glue("{root}/{rcp}/{gcm}/{iam}/{ssp}/{allcalcs}"), skip = 21, show_col_types = FALSE))
  clim_inc = clim_inc %>% select(region, 2, loggdppc, climtas) %>% rename(year = `year...2`)

  
  # Save out
  out = glue("{out_dir}/{rcp}/{gcm}/{iam}/{ssp}")
  dir.create(out, recursive=T, showWarnings = F)
  
  # This is if we want to test adjusting long run covariates affects mortality estimates
  # Load in age group population to get population weighted loggdppc average
  if (adjust){
    pop = read_csv('/project/cil/home_dirs/egrenier/cil-comms/memo_fact_check/data/misc/pop-SSP2-low.csv') %>% select(region, year, pop)
    wb = read_csv('/project/cil/gcp/inequality/other_data/oecd_worldbank_regions/oecd-worldbank-regions.csv') %>% rename(iso = region.key) %>% select(iso, worldbank)
    clim_inc = clim_inc %>% left_join(pop) #%>% mutate(iso = substr(region, 1, 3)) %>% left_join(wb)
    
    # get pop weighted average income
    clim_inc = clim_inc %>%
      group_by(year) %>%
      mutate(inc_adj = weighted.mean(loggdppc, pop, na.rm = TRUE)) %>%
      ungroup()
    
    clim_inc = clim_inc %>% mutate(loggdppc = if_else(worldbank == "low" & !is.na(worldbank), inc_adj, loggdppc))
    clim_inc = clim_inc %>% mutate(loggdppc = if_else(loggdppc < inc_adj & !is.na(inc_adj), inc_adj, loggdppc))
    clim_inc = clim_inc %>% select(region, year, loggdppc, climtas) %>% as.data.frame()
  }
  
  
  message(glue("[writing: ] {rcp}/{gcm}/{iam}/{ssp}/mortality-econ_clim.csv"))
  write.csv(clim_inc, glue("{out}/mortality-econ_clim.csv"), row.names = F)
  message("---- saved ----\n")
  
}
