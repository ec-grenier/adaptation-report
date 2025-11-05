#==============================================================================#
#' 
#' 
#' Fix hot day impacts
#' 
#' 
#==============================================================================#

#==============================================================================#
# 0. Source, set packages and paths -----------
packages = c("glue", "tidyverse", "data.table", "reticulate", "scales", "fixest")
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

#==============================================================================#
# 1. Load data -----------

mc = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low.csv')
median = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-median.csv')
db = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-deltabeta_total.csv')
db_hot = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot.csv')
db_cold = fread('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-cold.csv')

mc = mc %>% select(region,q50)%>% rename(q50.mc = q50)
median = median %>% select(region, q50) %>% rename(q50.median = q50)
db = db %>% select(region, q50) %>% rename(q50.db = q50)
db_hot = db_hot %>% select(region, q50) %>% rename(q50.hot = q50)
db_cold = db_cold %>% select(region, q50) %>% rename(q50.cold = q50)

#==============================================================================#
# 2. Regressions (doesn't work) -----------

reg_df = median %>% left_join(db)
reg_mc_df = mc %>% left_join(db)
lm = feols(q50.median ~ q50.db , reg_df)
summary(lm)

mc_lm = feols(q50.mc ~ q50.db, reg_mc_df)
etable(mc_lm)
rm(reg_df,reg_mc_df,lm,mc_lm)
#==============================================================================#
# 3. machete -----

mc_df = mc %>% left_join(db_hot) %>% left_join(db_cold)

# test = mc_df %>% mutate(q50.hot = ifelse(q50.hot > -0.5 & q50.hot < 0 , 0, q50.hot))
# test2 = mc_df %>% filter(q50.hot < 0)
# test3 = mc_df %>% filter(q50.hot > 0 & q50.hot < 0.5)

mc_df = mc_df %>% mutate(q50.hot = ifelse(abs(q50.hot) < 0.5, 0, q50.hot))
fix = mc_df %>% filter(q50.hot<0)
fix = fix %>% mutate(fixed = q50.mc - q50.cold)
fix = fix %>% select(region, fixed) %>% rename(q50 = fixed)

fine = mc_df %>% filter(!(region %in% fix$region))
fine = fine %>% select(region, q50.hot) %>% rename(q50 = q50.hot)

fixed = rbind(fine,fix)

fixed = fixed %>% left_join(mc) %>% left_join(db_hot) %>% left_join(db_cold)

final = fixed %>% select(region, q50)

write.csv(final,'/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot-fixed.csv', row.names=F)

#==============================================================================#
# 3. levels -----

rates = read_csv('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-rates-3_c-midc-SSP2-low-hot-fixed.csv')

pop = fread(glue('{input}/misc/pop-SSP2-low.csv')) %>% select(region, year, pop) %>% filter(year == 2050)

rates = rates %>% left_join(pop)
levels = rates %>%  mutate(q50 = q50 * pop / 100000 ) %>% select(region, q50) 

write.csv(levels,'/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready/mortality/combined-fulladapt-ir_level-levels-3_c-midc-SSP2-low-hot-fixed.csv', row.names=F)

