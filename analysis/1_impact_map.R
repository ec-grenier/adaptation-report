#==============================================================================#
#'
#'
#' Create impact maps across different levels of spatial aggregation
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
# set parameters

sector = 'mortality'
scn = 'noadapt'
category = 'combined'
unit = 'rates'
gwl_bin = '3_c'
period = 'midc'
spatial = 'ir_level' # 'aggregated' or 'ir_level'
top = NULL # accepts values in (0,1) or NULL -- ONLY RUN THIS WITH IR_LEVEL
cont = F # T or F
db=""
db_slug=""
lb = -150
ub = -lb
slug=''
title = glue("Mid century {sector} {category} age group impacts (3C warming{db_slug})")
colorbar_title = ifelse(unit == "rates", "Deaths/100,000", "Deaths")

#slug = ifelse(!is.null(top), glue('-top_{top}'),'')

#==============================================================================#
# load data

map.df = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')
print(glue("Reading: {input}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{db}.csv"))

if (spatial == 'aggregated' & unit == 'levels'){
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low{db}.csv")) %>%
    select(region, q50)
} else {
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{db}.csv")) %>%
    select(region, q50)
}

#==============================================================================#
# data cleaning

if (!is.null(top)){
  impacts = impacts %>% mutate(q50 = ifelse(q50 >= quantile(q50, probs = 1 - top, na.rm = TRUE), q50, NA))
}

# Sum IR dmg to country if levels, pick just countries if rates
# then assign the damages to all IRs in a country 
if (!(cont) & spatial == 'aggregated'){
  if (unit == 'levels'){
    impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>% summarise(q50 = sum(q50, na.rm = TRUE))
  }  else {
    impacts = impacts %>% filter(nchar(region) <= 3 & region != "")
  }
  #
  irs = map.df %>% st_drop_geometry() %>% select(region=hierid) %>% mutate(iso = substr(region, 1, 3))
  impacts = irs %>% left_join(impacts, by = c("iso"="region")) %>% select(region, q50)
}

if (cont){
  
  irs = map.df %>% st_drop_geometry() %>% select(region=hierid) %>% mutate(iso = substr(region, 1, 3))
  
  continents = read.csv('/project/cil/gcp/regions/continents2.csv') %>% 
    select(alpha.3, region, sub.region) %>% 
    rename(country = alpha.3,
           continent = region,
           sub_region = sub.region) %>%
    mutate(country = ifelse(country=='SXM', 'SMX', country),
           continent = ifelse(country=='ATA', 'Antarctica', continent),
           sub_region = ifelse(country=='ATA', 'Antarctica', sub_region),
           sub_region = ifelse(country=='MEX', 'Northern America', sub_region)) %>%
    bind_rows(data.frame(country = "KO-", continent = "Europe"),
              data.frame(country = "CA-", continent = "Asia")) %>%
    mutate(continent = ifelse(continent == "Americas", sub_region, continent))
  
  if (unit == "levels"){
    
    impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>% summarise(q50 = sum(q50, na.rm = TRUE))
    
    impacts = impacts %>% left_join(continents, by = c('region'='country')) %>% #
      filter(!is.na(continent)) %>% 
      group_by(continent) %>% 
      summarise(q50 = sum(q50, na.rm = TRUE)) 

  } else {
    
    # population weighted average mortality rate
    impacts = impacts %>% filter(nchar(region) <= 3 & region != "")
    pop = get_dscim_econ_vars('pop', 'iso', 'low', 'SSP2', 2050) %>% select(region, pop)
    impacts = impacts %>% left_join(pop) %>% left_join(continents, by=c("region"="country"))
    impacts = impacts %>%
      group_by(continent) %>%
      summarise(q50 = weighted.mean(q50, w = pop, na.rm = TRUE)) %>%
      filter(!is.na(q50))

  }
  
  impacts = continents %>% left_join(irs, by = c("country"="iso")) %>% left_join(impacts)
  
}

rescale_val = c(c(1, 1/2, 1/4, 1/8, 1/15, 1/30, 1/100, 1/500, 1/5000, 10E-5)*lb,
                0, c(10E-5, 1/5000, 1/500, 1/100, 1/30, 1/15, 1/8, 1/4, 1/2, 1)*ub)

if (sector == 'coastal'){
  color_scheme = 'seq'
} else if (sector=='labor'){
  color_scheme = 'rev'
} else {
  color_scheme = 'div'
}
# select number of breaks (super janky but watevs)
#breaks_labels_val = floor(c(seq(lb,0,length=3)[1:2],0,seq(0,ub,length=3)[2:3]))
breaks_labels_val = floor(c(seq(lb,0,length=4)[1:3],0,seq(0,ub,length=4)[2:4]))
#breaks_labels_val = floor(c(seq(lb,0,length=5)[1:4],0,seq(0,ub,length=5)[2:5]))
#breaks_labels_val = floor(c(seq(lb,0,length=6)[1:5],0,seq(0,ub,length=6)[2:6]))

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

if (!(cont) & spatial == 'aggregated'){
  country.shp = ne_countries(scale="large", returnclass="sf") %>% filter(sov_a3 != 'ATA') %>% st_transform("ESRI:54030") 
  impact.map = impact.map + geom_sf(data=country.shp, fill=NA, color='grey15', linewidth=0.1)
} else if (cont){
  spatial = "continent"
    
  # get continent shapes -- need to use planar geometry to group countries into continent shapes.
  sf::sf_use_s2(FALSE)
  country.shp = ne_countries(scale="large", returnclass="sf") %>% filter(sov_a3 != 'ATA') %>% st_transform("ESRI:54030") 
  country.shp = country.shp %>% 
    mutate(continent = case_when(sov_a3 %in% c("PAN", "GTM", "NIC", "HND", "SLV", "BLZ", "CRI") ~ "South America", TRUE ~ continent)) %>%
    mutate(continent = case_when(sov_a3 %in% c("MEX") ~ "North America", TRUE ~ continent))
  cont.shp = country.shp %>% st_set_geometry("geometry") %>% group_by(continent) %>% summarize(.groups = "drop")
  sf::sf_use_s2(TRUE)
  
  impact.map = impact.map + geom_sf(data=cont.shp, fill=NA, color='grey15', linewidth=0.1)
}

#print(impact.map)
message(glue("Saving: {output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{db}{slug}.pdf"))
ggsave(glue("{output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{db}{slug}.pdf"), impact.map)


