#==============================================================================#
#'
#'
#' Create impact maps for IR damages, greying out rich countries
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
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps"

#==============================================================================#

# redfine these manually
lb = -15
ub = -lb

title = glue("Fulladaptcosts damages (% of end century GDP)")
colorbar_title = "Percent of IR GDP"


#==============================================================================#
# load data

map.df = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')

impacts = read.csv("/project/cil/gcp/inequality/impacts-mealy/extraction_AMEL_global_rebased/SSP3-3_c_endc-global-AMEL_m0_vly-low-ir_level-damages_pctgdp-edfcsv.csv") %>% select(region, mean)

#impacts = combine_gwl_data(
#  input           = "/project/cil/gcp/inequality/impacts-mealy/",
#  sector_basename = "extraction_AMEL_noadapt_rebased",
#  spatial_level   = "ir",
 # gwl             = "3_c",
#  ssp             = "SSP3",
#  scn             = "noadapt",
#  category        = "AMEL_m0_vly",
#  iam             = "low",
#  unit            = "damages_pctgdp",
 # timeperiod      = "endc"
#) %>%
#select(region, mean)

#==============================================================================#
# data cleaning

rescale_val = c(c(1, 1/2, 1/4, 1/8, 1/15, 1/30, 1/100, 1/500, 1/5000, 10E-5)*lb,
                0, c(10E-5, 1/5000, 1/500, 1/100, 1/30, 1/15, 1/8, 1/4, 1/2, 1)*ub)

color_scheme = 'div'

# select number of breaks (super janky but watevs)
#breaks_labels_val = floor(c(seq(lb,0,length=3)[1:2],0,seq(0,ub,length=3)[2:3]))
breaks_labels_val = floor(c(seq(lb,0,length=4)[1:3],0,seq(0,ub,length=4)[2:4]))
#breaks_labels_val = floor(c(seq(lb,0,length=5)[1:4],0,seq(0,ub,length=5)[2:5]))
#breaks_labels_val = floor(c(seq(lb,0,length=6)[1:5],0,seq(0,ub,length=6)[2:6]))

message('Starting plot')
impact.map = join.plot.map(map.df = map.df,
                           df = impacts,
                           df.key = 'region',
                           plot.var = 'mean',
                           topcode = TRUE,
                           topcode.lb = lb,
                           topcode.ub = ub,
                           color.scheme = color_scheme,
                           colorbar.title = colorbar_title,
                           map.title = title,
                           rescale_val = rescale_val,
                           breaks_labels_val = breaks_labels_val,
                           plot.lakes = F)

message(glue("Saving: /project/cil/home_dirs/egrenier/damages_3c_global_pct_gdp.pdf"))
ggsave(glue("/project/cil/home_dirs/egrenier/damages_3c_global_pct_gdp.pdf"), impact.map)
