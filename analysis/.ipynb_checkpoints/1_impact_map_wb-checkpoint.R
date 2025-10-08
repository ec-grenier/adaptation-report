#==============================================================================#
#'
#'
#' 
#'
#'
#==============================================================================#

#==============================================================================#
# Load in the required packages, installing them if necessary 
library(tidyverse)
library(multidplyr)
library(glue)
library(sf)
library(scales)
library(rnaturalearth)
library(ncdf4)
library(reticulate)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/helper_functions/mapping_ineq.R')
source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/get_dscim_econ_vars_ineq.R')

input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps"
output='/project/cil/home_dirs/egrenier/agriculture/outputs/'
#==============================================================================#
# set parameters

sector = 'mortality'
scn = 'fulladapt'
category = 'combined'
unit = 'levels'
gwl_bin = '3_c'
period = 'midc'
spatial = 'ir_level'

exclude = c("high", "upper-middle") # high or upper-middle or both

# redfine these manually
lb = -2500
ub = -lb

title = glue("Mid century {sector} {category} age group impacts (3C warming)")
colorbar_title = "Deaths"

prefix = ifelse(length(exclude) == 1, "wb_no_high", "wb_no_high_um") # um short for upper-middle
slug = ifelse(!is.null(top), glue('-top_{top}'),'')


#==============================================================================#
# load data

map.df = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')

wb_regions = read_csv("/project/cil/gcp/regional_scc/data/misc/worldbank-regions.csv")

print(glue("Reading: {input}/{sector}-{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2.csv"))
impacts = read.csv(glue("{input}/{sector}-{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2.csv")) %>%
  select(region, q50)

# impacts = read.csv('/project/cil/home_dirs/egrenier/agriculture/calories_3c_fulladaptcosts.csv') %>% filter(gwl_bin=="3_c") %>% select(region, value) %>% mutate(value = value / 1e9 * -1)
# test = read.csv('/project/cil/home_dirs/egrenier/agriculture/calories_3c_fulladaptcosts.csv') %>% filter(gwl_bin=="3_c") 
# title = glue("End of century agriculture impacts for six staple crops (3C warming)")
# colorbar_title = "Billions of calories"
# lb = -10000
# ub = -lb
#==============================================================================#
# data cleaning

impacts = impacts %>% mutate(ISO = substr(region, 1, 3)) %>% left_join(wb_regions)
impacts = impacts %>% mutate(q50 = case_when(worldbank %in% exclude ~ NA, TRUE ~ q50))

rescale_val = c(c(1, 1/2, 1/4, 1/8, 1/15, 1/30, 1/100, 1/500, 1/5000, 10E-5)*lb,
                0, c(10E-5, 1/5000, 1/500, 1/100, 1/30, 1/15, 1/8, 1/4, 1/2, 1)*ub)

if (sector == 'coastal'){
  color_scheme = 'seq'
} else if (sector=='labor'){
  color_scheme = 'rev'
} else {
  color_scheme = 'div'
}
breaks_labels_val = floor(c(seq(lb,0,length=4)[1:3],0,seq(0,ub,length=4)[2:4]))

breaks_labels_val = floor(c(seq(lb,0,length=5)[1:4],0,seq(0,ub,length=5)[2:5]))

print('Starting plot')
impact.map = join.plot.map(map.df = map.df,
                           df = impacts,
                           df.key = 'region',
                           plot.var = 'q50',
                           topcode = TRUE,
                           topcode.lb = lb,
                           topcode.ub = ub,
                           color.scheme = color_scheme,
                           colorbar.title = colorbar_title,
                           map.title = title,
                           rescale_val = rescale_val,
                           breaks_labels_val = breaks_labels_val,
                           plot.lakes = F)


print(impact.map)
message(glue("Saving: {output}/{prefix}-{sector}-{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}.pdf"))
ggsave(glue("{output}/{prefix}-{sector}-{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}.pdf"), impact.map)
