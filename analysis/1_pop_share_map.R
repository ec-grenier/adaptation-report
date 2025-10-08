#==============================================================================#
#'
#'
#' Create age group population share maps
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
library(data.table)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/helper_functions/mapping_ineq.R')
source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

input = '/project/cil/gcp/regional_scc/data/deltabeta/input/covariates'
output = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps"

#==============================================================================#
# set parameters

DEFAULT_CRS = glue("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                   " +datum=WGS84 +units=m +no_defs")

agegroup = "oldest"
year = 2025 #or 2025
ub = 50
title = glue("{agegroup} age group population share {year}")
colorbar_title = "Population share (%)"

slug= "-alt"
#==============================================================================#
# load data

map.df = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')
map.df = map.df %>% 
  st_transform(DEFAULT_CRS) %>% 
  filter(!(hierid %in% c("CA-", "USA.23.1273", "USA.14.642",
                         "USA.50.3082", "USA.50.3083", "USA.23.1275",
                         "USA.15.740", "USA.24.1355", "USA.33.1855",
                         "USA.36.2089", "USA.23.1272", "UGA.32.80.484",
                         "UGA.31.79.483.2760", "UGA.32.80.484.2761",
                         "TZA.13.59.1169", "TZA.5.26.564", "TZA.17.86.1759",
                         "ATA", "PER.8.71.705", "PER.7.67.677",
                         "ARM.7", "USA.23.1274", "TZA.8.37.779")))

print(glue("Reading population data"))
pop = fread(glue("{input}/econ_clim-CCSM4-rcp45-SSP2-low.csv")) %>% filter(year == !!year) %>% select(region, all_of(paste0(agegroup, "_share")))

pop_new = fread(glue("{input}/econ_clim-CCSM4-rcp45-SSP2-low.csv")) %>% 
  filter(year == 2050) %>%
  select(region, young_share, older_share, oldest_share) %>% 
  rename(young_share_2050 = young_share, older_share_2050=older_share, oldest_share_2050 = oldest_share)

pop_old = fread(glue("{input}/econ_clim-CCSM4-rcp45-SSP2-low.csv")) %>% 
  filter(year == 2025) %>%
  select(region, young_share, older_share, oldest_share) %>% 
  rename(young_share_2025 = young_share, older_share_2025=older_share, oldest_share_2025 = oldest_share)

pop_map = pop_new %>% left_join(pop_old)
pop = pop_map %>% mutate(iso = substr(region, 1, 3))
pop = pop %>%
  group_by(iso) %>%
  summarise(across(starts_with("young_share"), ~ first(na.omit(.x))),
            across(starts_with("older_share"), ~ first(na.omit(.x))),
            across(starts_with("oldest_share"), ~ first(na.omit(.x)))) %>%
  ungroup()

pop$diff_oldest = pop$oldest_share_2050 - pop$oldest_share_2025
pop$diff_older = pop$older_share_2050 - pop$older_share_2025
pop$diff_young = pop$young_share_2050 - pop$young_share_2025

colnames(pop)[2] = 'share'
pop[,2] = pop[,2]*100 
#==============================================================================#
# data cleaning

color_scheme = 'seq'
breaks_labels_val = c(0,seq(0,ub,length=5)[2:5])
plot.df = map.df %>% left_join(pop, by=c("hierid"="region"))
limits_val = c(0, ub)

color.values = rev(c("#c92116", "#ec603f", "#fd9b64",
                     "#fdc370", "#fee69b","#fef7d1", "#f0f7d9"))

# FIX AESTHETICS
print('Starting plot') 
p = ggplot(data = plot.df) +
  geom_sf(aes(fill=share), lwd = 0.05, color = NA) +
  theme_bw() +     
  theme(plot.title = element_text(hjust=0.5, size = 10), 
        plot.caption = element_text(hjust=0.5, size = 7), 
        legend.title = element_text(hjust=0.5, size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        axis.title= element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank()) +   
  labs(title = title) +
  scale_fill_gradientn(
    colors = color.values,
    na.value = "grey85",
    limits = limits_val,
    breaks = breaks_labels_val,
    labels = breaks_labels_val,
    guide = guide_colorbar(
      title = colorbar_title,
      direction = "horizontal",
      barheight = unit(4, "mm"),
      barwidth = unit(100, units = "mm"), 
      draw.ulim = FALSE,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  )


#print(p)

message(glue("Saving: {output}/other/{year}-{agegroup}_share-map{slug}.pdf"))
ggsave(glue("{output}/other/{year}-{agegroup}_share-map{slug}.pdf"), p)
