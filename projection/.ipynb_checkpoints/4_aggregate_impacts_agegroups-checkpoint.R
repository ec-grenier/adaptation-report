#==============================================================================#
#' 
#' 
#' Aggregate mortality delta beta impacts
#' 
#' to run, change agegroup manually and run
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
agegroup = "young"
input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data'
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

pop = fread(glue('{input}/misc/pop-SSP2-low.csv')) %>% select(region, year, paste0(agegroup,"_pop"))

#==============================================================================#
# 1. Functions -----------

# function to help load in data and stack impacts 
stack_impacts = function(spec, year, type, levels=F, pop=NULL){
  
  rcp = spec$rcp
  gcm = spec$gcm
  iam = spec$iam
  ssp = spec$ssp
  
  # Combine impacts
  df = fread(glue("{input}/deltabeta/{rcp}/{gcm}/{iam}/{ssp}/mortality-delta_beta-fulladapt-{year}-{agegroup}.csv"))
  
  if (type == "total"){
    df = df %>% filter(bin == "Total") %>% select(region, effect_fa) %>% rename(value = effect_fa)
  } else if (type == "hot"){
    df = df %>% filter(bin == "hot") %>% select(region, effect_fa) %>% rename(value = effect_fa)
  } else if (type == "cold"){
    df = df %>% filter(bin == "cold") %>% select(region, effect_fa) %>% rename(value = effect_fa)
  } else {
    return("Incorrect type: accepts total, hot, cold")
  }
  
  if (levels){
    pop = pop %>% filter(year == !!year) %>% select(region, paste0(agegroup,'_pop'))
    colnames(pop)[2] = 'pop'
    df = suppressMessages(df %>% left_join(pop))
    
    df = df %>% mutate(value = value / 100000 * pop)
  }
  return(df)
}

# function to get a mean, q25, q50, q75
get_quantiles = function(df) {
  
  df %>%
    group_by(region) %>%
    summarise(mean = mean(value, na.rm = TRUE),
              q25  = quantile(value, 0.25, na.rm = TRUE),
              q50  = quantile(value, 0.50, na.rm = TRUE),
              q75  = quantile(value, 0.75, na.rm = TRUE),
              .groups = "drop")
      
}

#==============================================================================#
# 2. Aggregate impacts (RATES) -----------

total = data.frame()
for (s in specs) {
  for (year in years){
    total = rbind(total, stack_impacts(spec=s, year=year, type="total"))
  }
}
total = get_quantiles(total) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_total = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-rates-3_c-midc-SSP2-low-deltabeta_total.csv')
print(output_total)
write.csv(total, output_total, row.names=F)

hot = data.frame()
for (s in specs) {
  for (year in years){
    hot = rbind(hot, stack_impacts(spec=s, year=year, type="hot"))
  }
}
hot = get_quantiles(hot) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_hot = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot.csv')
print(output_hot)
write.csv(hot, output_hot, row.names=F)

cold = data.frame()
for (s in specs) {
  for (year in years){
    cold = rbind(cold, stack_impacts(s, year, "cold"))
  }
}
cold = get_quantiles(cold) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_cold = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-rates-3_c-midc-SSP2-low-cold.csv')
print(output_cold)
write.csv(cold, output_cold, row.names=F)

#==============================================================================#
# 2. Aggregate impacts (RATES) -----------

total = data.frame()
for (s in specs) {
  for (year in years){
    total = rbind(total, stack_impacts(spec=s, year=year, type="total", levels=T, pop=pop))
  }
}
total = get_quantiles(total) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_total = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-levels-3_c-midc-SSP2-low-deltabeta_total.csv')
print(output_total)
write.csv(total, output_total, row.names=F)

hot = data.frame()
for (s in specs) {
  for (year in years){
    hot = rbind(hot, stack_impacts(spec=s, year=year, type="hot", levels =T, pop=pop))
  }
}
hot = get_quantiles(hot) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_hot = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-levels-3_c-midc-SSP2-low-hot.csv')
print(output_hot)
write.csv(hot, output_hot, row.names=F)

cold = data.frame()
for (s in specs) {
  for (year in years){
    cold = rbind(cold, stack_impacts(spec=s, year=year, type="cold", levels=T, pop=pop))
  }
}
cold = get_quantiles(cold) %>% mutate(year ="2040-2059") %>% select(region, year, everything())

output_cold = glue('{input}/analysis_ready/mortality/{agegroup}-fulladapt-ir_level-levels-3_c-midc-SSP2-low-cold.csv')
print(output_cold)
write.csv(cold, output_cold, row.names=F)
