#==============================================================================#
#' 
#' 
#' In text stats, add sections upon stats requests
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

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

#==============================================================================#
# 1. What is GMST relative to pre-indstrial for all models? -----------

GMT_all = read.csv("/project/cil/norgay/Global_ACP/MORTALITY/release_2020/data/4_damage_function/GMST/GMTanom_all_temp_2001_2010_smooth.csv")
GMT_86_05_avg = GMT_all %>% 
  filter( year >= 1986 & year <= 2005) %>% 
  group_by(gcm, rcp) %>% 
  summarise(mean_temp_86_05 = mean(temp)) %>% 
  filter(!is.na(mean_temp_86_05)) #dropping surrogate_GFDL-ESM2G_06 which doesn't appear to have any data for RCP 4.5

GMT_all = GMT_all %>% 
  inner_join(GMT_86_05_avg, by = c("gcm" = "gcm", "rcp" = "rcp")) %>% 
  mutate(rebased_temp = temp - mean_temp_86_05) %>% 
  select(-temp, -mean_temp_86_05) %>% 
  rename(temp = rebased_temp)

# Finding anomaly relative to preindustrial
# Using an adjustment of 0.61 C (uncertainty 0.55 to 0.67) from the IPCC AR5
# see section TS.2.2.1 https://www.ipcc.ch/site/assets/uploads/2018/02/WG1AR5_TS_FINAL.pdf
GMT_all = GMT_all %>% 
  mutate(temp_rel_pre_in = temp + 0.61)

specs = list(
  list(rcp = "rcp45", gcm = "ACCESS1-0"),
  list(rcp = "rcp45", gcm = "BNU-ESM"),
  list(rcp = "rcp45", gcm = "CanESM2"),
  list(rcp = "rcp45", gcm = "CSIRO-Mk3-6-0"),
  list(rcp = "rcp45", gcm = "IPSL-CM5A-LR"),
  list(rcp = "rcp45", gcm = "IPSL-CM5A-MR"),
  list(rcp = "rcp45", gcm = "MIROC-ESM"),
  list(rcp = "rcp45", gcm = "MIROC-ESM-CHEM"),
  list(rcp = "rcp85", gcm = "GFDL-ESM2M"),
  list(rcp = "rcp85", gcm = "inmcm4")
)

specs_df = bind_rows(specs)
models_3c = GMT_all %>% semi_join(specs_df, by = c("rcp", "gcm")) %>% filter(year==2050)

print(round(mean(models_3c$temp_rel_pre_in), 2))

#==============================================================================#
# 2. 10% of global pop lives in regions contributing 80% of the increase in mortality? -----------

pop = get_dscim_econ_vars("pop", "all", "low", "SSP2", 2050) 

impacts = read.csv('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-levels-3_c-midc-SSP2-low-hot.csv') %>% select(region, q50)
impacts = impacts %>% filter(!is.na(q50)) %>% arrange(q50)
impacts = impacts %>% mutate(cum_damages = cumsum(q50),
                             cum_pct = cum_damages / sum(q50, na.rm = TRUE) * 100)

top75 = impacts %>% filter(cum_pct >= 25)
top75 = top75 %>% left_join(pop)
top75_pop = sum(top75$pop)

top50 = impacts %>% filter(cum_pct >= 50)
top50 = top50 %>% left_join(pop)
top50_pop = sum(top50$pop)

top80 = impacts %>% filter(cum_pct >= 20)
top80 = top80 %>% left_join(pop)
top80_pop = sum(top80$pop)

top90 = impacts %>% filter(cum_pct >= 10)
top90 = top90 %>% left_join(pop)
top90_pop = sum(top90$pop)

total_pop = sum(pop$pop)

pct_exposed75 = top75_pop/total_pop * 100
pct_exposed50 = top50_pop/total_pop * 100
pct_exposed80 = top80_pop/total_pop * 100
pct_exposed90 = top90_pop/total_pop * 100

#==============================================================================#
# 3. Number of IRs/population that live in low or lower-middle income countries? -----------

wb_regions = read_csv("/project/cil/gcp/regional_scc/data/misc/worldbank-regions.csv")

pop = get_dscim_econ_vars("pop", "all", "low", "SSP2", 2050) %>% mutate(ISO = substr(region, 1, 3))
pop = pop %>% left_join(wb_regions)

# count of IRs by worldbank regions
pop %>% count(worldbank)
sum(pop$pop[pop$worldbank == "low"], na.rm = TRUE)
sum(pop$pop[pop$worldbank == "lower-middle"], na.rm = TRUE)
sum(pop$pop[pop$worldbank == "low"], na.rm = TRUE) + sum(pop$pop[pop$worldbank == "lower-middle"], na.rm = TRUE)

