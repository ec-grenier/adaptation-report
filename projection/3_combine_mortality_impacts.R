#==============================================================================#
#' 
#' 
#' Combine mortality delta beta impacts
#' 
#' 
#==============================================================================#

#==============================================================================#
# 0. Source, set packages and paths -----------
packages = c("glue", "tidyverse", "data.table", "reticulate", "scales")
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

#==============================================================================#
# 1. Define inputs -----------
input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data'

pop = fread(glue('{input}/misc/pop-SSP2-low.csv')) 
years = c(2040:2059)

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


#==============================================================================#
# 1. Combine impacts -----------

combine_impacts = function(pop, spec, year){
  
  rcp = spec$rcp
  gcm = spec$gcm
  iam = spec$iam
  ssp = spec$ssp
  
  pop = pop %>% filter(year==!!year)
  
  impacts = fread("/project/cil/gcp/regions/hierarchy-flat.csv") %>% select(`region-key`)
  colnames(impacts)[1] = 'region'
  
  ages = c("oldest","older","young")
  for(age in ages){  
    
    cols = c("region", paste0(age, "_pop"), paste0(age, "_share"))
    pop_subset = pop[ , ..cols]
    
    deaths = fread(glue('{input}/deltabeta/{rcp}/{gcm}/low/SSP2/mortality-delta_beta-fulladapt-{year}-{age}-001_bins.csv')) %>%
      mutate(bin = recode(bin,
                          "Total" = paste0("deaths","_",age),
                          "hot" = paste0("hot","_",age),
                          "cold" = paste0("cold","_",age))) %>%
      pivot_wider(names_from = bin,
                  values_from = effect_fa)
    
    impacts = impacts %>% left_join(deaths, by="region") %>% left_join(pop_subset, by="region")
  }
  
  # Combine impacts
  impacts = impacts %>%
    mutate(deaths = deaths_young * young_share + deaths_older * older_share + deaths_oldest * oldest_share,
           hot = hot_young * young_share + hot_older * older_share + hot_oldest * oldest_share,
           cold = cold_young * young_share + cold_older * older_share + cold_oldest * oldest_share) %>%
    select(region, deaths, hot, cold) 
  
  output = glue('{input}/deltabeta/{rcp}/{gcm}/low/SSP2/mortality-delta_beta-fulladapt-{year}-combined-001_bins.csv')
  message(output)
  write.csv(impacts, output, row.names=F)
}

#==============================================================================#
# 2. Call func -----------

for (s in specs) {
  for (year in years){
    combine_impacts(pop, s, year)
  }
}
