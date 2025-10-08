#==============================================================================#
#'
#'
#' Create age group population share maps
#'
#'
#==============================================================================#

#==============================================================================#
# Load in the required packages, installing them if necessary 

packages = c("glue", "sf", "tidyverse", "data.table", "reticulate", "scales", "ncdf4", "parallel")
invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/helper_functions/mapping_ineq.R')
source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/misc'
output = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps"

# load in full dataset
mmt = fread(glue('{input}/collected_mmt.csv'))

#==============================================================================#
# set parameters

DEFAULT_CRS = glue("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84",
                   " +datum=WGS84 +units=m +no_defs")

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

# testing 
# mmt2 = fread('/project/cil/battuta-shares-S3-archive/gcp/outputs/mortality/impacts-darwin/montecarlo/batch10/rcp45/ACCESS1-0/low/SSP2/Agespec_interaction_response-oldest-polymins.csv') %>%
#   rename(mmt = analytic)

#==============================================================================#
# plot func

plot_mmt = function(mmt, agegroup, year){

  ub = 30
  title = glue("{agegroup} Median Minimum Mortality Temp {year}")
  colorbar_title = "MMT"
  
  age = ifelse(agegroup == "oldest", 3, ifelse(agegroup == "older", 2, 1))
  mmt2 = mmt %>% filter(age == !!age, year == !!year) %>% group_by(region) %>% summarize(mmt = median(mmt))
  
  color_scheme = 'seq'
  breaks_labels_val = c(seq(10,ub,length=3))
  plot.df = map.df %>% left_join(mmt2, by=c("hierid"="region"))
  limits_val = c(10, ub)
  
  color.values = rev(c("#c92116", "#ec603f", "#fd9b64",
                       "#fdc370", "#fee69b","#fef7d1", "#f0f7d9"))
  
  # FIX AESTHETICS
  print('Starting plot') 
  p = ggplot(data = plot.df) +
    geom_sf(aes(fill=mmt), lwd = 0.05, color = NA) +
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
  
  message(glue("Saving: {output}/other/{year}-{agegroup}_mmt-map.pdf"))
  ggsave(glue("{output}/other/{year}-{agegroup}_mmt-map.pdf"), p)
  return(p)
}

#==============================================================================#
# run in parallel

years = 2040:2059
agegroups = c("young", "older", "oldest")
specs = expand.grid(year = years, agegroup = agegroups, stringsAsFactors = FALSE)

mcmapply(
  FUN = function(year, agegroup) {
    plot_mmt(mmt, agegroup, year)
    NULL   # suppress return values (we only care about saving the plots)
  },
  year = specs$year,
  agegroup = specs$agegroup,
  mc.cores = 10
)
