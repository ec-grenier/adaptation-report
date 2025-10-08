#==============================================================================#
#'
#'
#' map dmg by 
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

library(tidyverse)
library(multidplyr)
library(glue)
library(sf)
library(scales)
library(rnaturalearth)
library(ncdf4)
library(reticulate)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/get_dscim_econ_vars_ineq.R')

#==============================================================================#
# define parameters ----

input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
sector = 'mortality'
scn = 'fulladapt'
category = 'oldest'
unit = 'rates'
gwl_bin = '3_c'
period = 'midc'
spatial = 'ir_level'
ntiles = 5

DEFAULT_CRS = glue("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                   " +datum=WGS84 +units=m +no_defs")

title = glue("Mid century {sector} {category} age group rankings (3C warming)")

# read in map, set CRS, remove lakes
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
                                 
message(glue("Reading: {input}/{sector}-{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2.csv"))
impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2.csv")) %>%
  select(region, q50)

impacts = impacts %>% mutate(group = ntile(q50, ntiles))

#pop = get_dscim_econ_vars(units = 'pop', regions='all', iam = 'low', ssp = 'SSP2', year=2050) %>% select(region, pop)

palette = colorRampPalette(rev(c("#c92116", "#ec603f", "#fd9b64", "#fdc370", "#fee69b", "#fef7d1", "#f0f7d9")))(ntiles)
plot.df = map.df %>% left_join(impacts, by=c('hierid'='region'))

p = ggplot(data = plot.df) +
  geom_sf(aes(fill=as.numeric(group)), lwd = 0.05, color = NA) +
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
  scale_fill_gradientn(colors = palette, 
                       na.value = "grey85", 
                       guide = guide_colorbar(title = glue("Impact regions ranked by damages ({ntiles} groups, low to high)"), 
                                              direction = "horizontal", barheight = unit(4, units = "mm"), 
                                              barwidth = unit(100, units = "mm"), 
                                              draw.ulim = F, 
                                              title.position = 'top', 
                                              title.hjust = 0.5, 
                                              label.hjust = 0.5))
#print(p)
ggsave(glue("/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps/{sector}/IR_ranked-{scn}-{category}-{period}-{gwl_bin}-{ntiles}_bins.pdf"), p)
