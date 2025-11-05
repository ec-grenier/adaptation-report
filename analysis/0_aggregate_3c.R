#==============================================================================#
#'
#'
#' Aggregate 3C impacts, create distribution
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

library(glue)
library(tidyr)
library(dplyr)
library(multidplyr)
library(data.table)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/helper_functions/region_tools_ineq.R')

input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/extracted'
sector = 'mortality'
scn = 'incadapt' # only fulladapt, noadapt or costs
category = 'combined'
unit = 'rates'
gwl_bin = '3_c'
period = 'midc'
spatial = 'ir_level' # 'ir_level' or 'aggregated'
allyears = '' # '-all_years' or '' 

calc_quantiles = function(values_data){
  cluster = new_cluster(5)
  cluster_library(cluster, "dplyr")
  
  edf_data = values_data %>% 
    dplyr::select(region, year, value) %>%
    group_by(region, year) %>% 
    partition(cluster) %>% 
    summarise(mean = mean(value, na.rm=T),
              q99 = quantile(value, 0.99, na.rm=T),
              q95 = quantile(value, 0.95, na.rm=T),
              q90 = quantile(value, 0.90, na.rm=T),
              q75 = quantile(value, 0.75, na.rm=T),
              q66 = quantile(value, 0.66, na.rm=T),
              q50 = quantile(value, 0.50, na.rm=T),
              q33 = quantile(value, 0.33, na.rm=T),
              q25 = quantile(value, 0.25, na.rm=T),
              q10 = quantile(value, 0.10, na.rm=T),
              q5 = quantile(value, 0.05, na.rm=T),
              q1 = quantile(value, 0.01, na.rm=T)
              ) %>% 
    collect()
  
  return(edf_data)
}

# ssp, iam will not change
raw_gwl_data_a = read.csv(glue('{input}/{sector}/rcp45-SSP2-{gwl_bin}_{period}_a-no_surrogate-{scn}-{category}-low-{spatial}{allyears}-{unit}-valuescsv.csv'))
raw_gwl_data_b = read.csv(glue('{input}/{sector}/rcp85-SSP2-{gwl_bin}_{period}_b-no_surrogate-{scn}-{category}-low-{spatial}{allyears}-{unit}-valuescsv.csv'))

raw_gwl_data = rbind(raw_gwl_data_a, raw_gwl_data_b)

# Convert units by sector
if(sector == 'mortality' & unit == 'rates'){ raw_gwl_data = raw_gwl_data %>% mutate(value = value*100000) } # deaths per 100,000

if(sector == 'labor' & unit == 'min'){ raw_gwl_data = raw_gwl_data %>% mutate(value = value*250/60) } # 250 work days per year-same assumption as paper, in hours

if(sector == 'energy' & unit == 'rates'){ raw_gwl_data = raw_gwl_data %>% mutate(value = value*277.78) } # conversion to kWh if emily chooses

if(sector == 'agriculture' & unit == 'delta_cals'){ raw_gwl_data = raw_gwl_data %>% mutate(value = as.numeric(value) / 1e12) %>% filter(!is.na(value))} # conversion to trillions of calories

# get mean and quantile distribution
raw_gwl_data = calc_quantiles(raw_gwl_data) %>% 
  pivot_longer(., mean:names(.)[ncol(.)], names_to = 'quantile', values_to = 'value') %>% 
  pivot_wider(names_from = quantile, values_from = value) %>% 
  select(region, year, mean, q1, q5, q10, q25, q33, q50, q66, q75, q90, q95, q99)

if (spatial == "aggregated"){
  raw_gwl_data = raw_gwl_data %>% filter(nchar(region) <= 3)
}

print(glue('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{allyears}.csv'))
# write out
write.csv(raw_gwl_data, 
          glue('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{allyears}.csv'), 
          row.names=F)
