#==============================================================================#
#'
#'
#' Create continent specific maps
#'
#'
#==============================================================================#

#==============================================================================#
# Load in the required packages, installing them if necessary 

packages = c("glue", "dplyr", "ggplot2", "sf", "scales", "rnaturalearth", "ncdf4", "reticulate", "readr")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/helper_functions/mapping_ineq.R')
source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

input = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = "/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/maps"

#==============================================================================#
# set parameters

sector = 'mortality'
scn = 'fulladapt'
category = 'combined'
unit = 'rates'
gwl_bin = '3_c'
period = 'midc'
spatial = 'ir_level' # 'aggregated' or 'ir_level'
slug="-hot-fixed" #
title_slug=", Hot days" #
title = glue("Mid century {sector} {category} age group impacts (3C warming{title_slug})")
colorbar_title = ifelse(unit == "rates", "Deaths/100,000", "Deaths")

continent_dict = list( # ADD BOUNDS 
  "Asia" = list(
    code = "asia", 
    crs = "+proj=laea +lat_0=45 +lon_0=100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    ub.mc = 150,
    ub.hot = 75
  ),
  "Europe" = list(
    code = "eu", 
    crs = "+proj=laea +lat_0=52 +lon_0=10 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    ub.mc = 100,
    ub.hot = 20
  ),
  "Africa" = list(
    code = "afr", 
    crs = "+proj=laea +lat_0=5 +lon_0=20 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    ub.mc = 80,
    ub.hot = 75
  ),
  "Oceania" = list(
    code = "oce", 
    crs = "+proj=laea +lat_0=-15 +lon_0=135 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", # MC bounds = -50,50, Hot bounds ?
    ub.mc = 35,
    ub.hot = 40
  ),
  "Latin America and the Caribbean" = list(
    code = "lac", 
    crs = "+proj=laea +lat_0=-15 +lon_0=-60 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", # MC bounds = -50,50, Hot bounds ?,
    ub.mc = 30,
    ub.hot = 20
  ),
  "Antarctica" = list(
    code = "ant", 
    crs = "+proj=laea +lat_0=-90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    ub.mc = 150,
    ub.hot = 50
  ),
  "Northern America" = list(
    code = "na", 
    crs = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    ub.mc = 150,
    ub.hot = 40
  )
)

#==============================================================================#
# load and clean data

map.df = st_read('/project/cil/sacagawea_shares/gcp/regions/world_combo_201710_mockup/agglomerated-world-new-simp100.shp')
country.shp = ne_countries(scale="large", returnclass="sf") %>% filter(sov_a3 != 'ATA') %>% st_transform("ESRI:54030") 
cont = read_csv('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/misc/continent-to-IR.csv')

print(glue("Reading: {input}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{slug}.csv"))
if (spatial == 'aggregated' & unit == 'levels'){
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low{slug}.csv")) %>%
    select(region, q50)
} else {
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low{slug}.csv")) %>%
    select(region, q50)
}

# Sum IR dmg to country if levels, pick just countries if rates
# then assign the damages to all IRs in a country 
if (spatial == 'aggregated'){
  if (unit == 'levels'){
    impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>% summarise(q50 = sum(q50, na.rm = TRUE))
  }  else {
    impacts = impacts %>% filter(nchar(region) <= 3 & region != "")
  }
  
  irs = map.df %>% st_drop_geometry() %>% select(region=hierid) %>% mutate(iso = substr(region, 1, 3))
  impacts = irs %>% left_join(impacts, by = c("iso"="region")) %>% select(region, q50)
}

impacts = impacts %>% left_join(cont)

# names = read_csv('/project/cil/gcp/regions/hierarchy.csv', skip=31, show_col_types = FALSE) %>% select(`region-key`, name) %>% rename(region=`region-key`)
# names = names %>% filter(nchar(region) <= 3 & region != "") %>% rename(iso = region)
# country_subset = country_subset %>% left_join(names)


fra_mpoly = country.shp %>% filter(adm0_a3 == "FRA")
country.shp = country.shp %>% filter(adm0_a3 != "FRA")
fra_poly = fra_mpoly %>% st_cast("POLYGON")
fra_poly = fra_poly %>% mutate(area_sqkm = as.numeric(st_area(.)) / 1e6)
fra = fra_poly %>% arrange(desc(area_sqkm)) %>% slice(1,3) %>% select(-area_sqkm)
country.shp = rbind(country.shp, fra)

rm(fra_mpoly, fra_poly, fra)

country.shp = country.shp %>% select(admin, adm0_a3, sovereignt) %>% rename(name=admin, iso=adm0_a3)
country.shp = country.shp %>%
  mutate(iso = case_when(iso == "PSX" ~ "PSE",
                         iso == "SXM" ~ "SMX",
                         iso == "KOS" ~ "KO-",
                         iso == "SDS" ~ "SSD",
                         iso == "ALD" ~ "ALA",
                         iso == "PGA" ~ "SP-",
                         iso == "CLP" ~ "CL-",
                         iso %in% c("CNM", "CYN") ~ "CYP",
                         iso == "SAH" ~ "MAR",
                         iso == "BRT" ~ "SDN",
                         iso == "USG" ~ "USA",
                         iso == "BRI" ~ "BRA",
                         iso == "SPI" ~ "CHL",
                         iso %in% c("IOA", "CSI", "ATC") ~ "AUS",
                         iso == "BJN" ~ "COL",
                         TRUE ~ iso)
         ) %>%
  filter(!iso %in% c("KAB", "SCR", "SER")) %>%
  group_by(iso) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

cont_iso = cont %>% distinct(iso, .keep_all = TRUE)
country.shp = country.shp %>% left_join(cont_iso) %>% select(-region)

#==============================================================================#
# Plot map


for (c in names(continent_dict)){

  if (c == "Antarctica") next #skip antarctica
  
  code = continent_dict[[c]]$code
  DEFAULT_CRS = continent_dict[[c]]$crs
  
  impacts_subset = impacts %>% filter(continent == c)
  country_subset = country.shp %>% filter(continent == c)
  country_subset = country_subset %>% st_transform(DEFAULT_CRS)
  
  if (slug == ""){
    ub = continent_dict[[c]]$ub.mc
    lb = -ub
    breaks_labels_val = floor(c(seq(lb,0,length=4)[1:3],0,seq(0,ub,length=4)[2:4]))
  } else {
    ub = continent_dict[[c]]$ub.hot
    lb = -ub
    breaks_labels_val = floor(c(seq(lb,0,length=3)[1:2],0,seq(0,ub,length=3)[2:3]))
  }

  rescale_val = c(c(1, 1/2, 1/3, 1/4, 1/8, 1/15, 1/50, 1/100, 1/500, 10E-5)*lb,
                  0, c(10E-5, 1/500, 1/100, 1/50, 1/15, 1/8, 1/4, 1/3, 1/2, 1)*ub)

  print('Starting plot')
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

  shp_plot = map.df %>% filter(hierid %in% c(impacts_subset$region))
  shp_plot = shp_plot %>% left_join(impacts_subset, by = c('hierid'='region'))

  minval = round(min(shp_plot$q50, na.rm = T), digits = 2)
  maxval = round(max(shp_plot$q50, na.rm = T), digits = 2)
  avgval = round(mean(shp_plot$q50, na.rm = T), digits = 2)
  caption_val = glue("Min: {minval}   Avg: {avgval}   Max: {maxval}")

  shp_plot$q50 = squish(shp_plot$q50, c(lb,ub))
  na.df = dplyr::filter(shp_plot, is.na(q50))

  limits_val = c(lb, ub)

  color.values = rev(c("#d7191c", "#fec980", "#ffedaa", "grey95", "#e7f8f8", "#9dcfe4", "#2c7bb6"))
  na.color = "grey85"

  p = ggplot(data = shp_plot) +
    geom_sf(aes(fill=q50), lwd = 0.05, color = NA) + #color = NA removes the borders
    geom_sf(data = na.df, fill = na.color, color = NA) +
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
    labs(title = title, caption = caption_val) +
    scale_fill_gradientn(colors = color.values,
                         values=rescale(rescale_val),
                         na.value = na.color,
                         limits = limits_val, #center color scale so white is at 0
                         breaks = breaks_labels_val,
                         labels = breaks_labels_val, #set freq of tick labels
                         guide = guide_colorbar(title = colorbar_title,
                                                direction = "horizontal",
                                                barheight = unit(4, units = "mm"),
                                                barwidth = unit(100, units = "mm"),
                                                draw.ulim = F,
                                                title.position = 'top',
                                                title.hjust = 0.5,
                                                label.hjust = 0.5))

  p = p + geom_sf(data=country_subset, fill=NA, color='grey15', linewidth=0.1) 

  print(p)
  message(glue("Saving: {output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}-{code}.pdf"))
  ggsave(glue("{output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}-{code}.pdf"), p)
}

#==============================================================================#
# Plot map with grey NA areas (then edit in illustrator)

# for (c in names(continent_dict)){
#   
#   if (c == "Antarctica") next #skip antarctica
#   
#   DEFAULT_CRS = glue("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#   code = continent_dict[[c]]$code
#   
#   # impacts_subset = impacts %>% filter(continent == c)
#   
#   impacts_subset = impacts %>% mutate(q50 = ifelse(continent != c, NA, q50))
#   country_subset = country.shp %>% filter(continent == c)
#   
#   if (slug == ""){
#     ub = 150
#     lb = -ub
#     breaks_labels_val = floor(c(seq(lb,0,length=4)[1:3],0,seq(0,ub,length=4)[2:4]))
#   } else {
#     ub = 50
#     lb = -ub
#     breaks_labels_val = floor(c(seq(lb,0,length=3)[1:2],0,seq(0,ub,length=3)[2:3]))
#   }
#   
#   color_scheme = "div"
#   rescale_val = c(c(1, 1/2, 1/3, 1/4, 1/8, 1/15, 1/50, 1/100, 1/500, 10E-5)*lb,
#                   0, c(10E-5, 1/500, 1/100, 1/50, 1/15, 1/8, 1/4, 1/3, 1/2, 1)*ub)
#   
#   p = join.plot.map(map.df = map.df,
#                     df = impacts_subset,
#                     df.key = 'region',
#                     plot.var = 'q50',
#                     topcode = TRUE,
#                     topcode.lb = lb,
#                     topcode.ub = ub,
#                     color.scheme = color_scheme,
#                     colorbar.title = colorbar_title,
#                     map.title = title,
#                     rescale_val = rescale_val,
#                     breaks_labels_val = breaks_labels_val,
#                     plot.lakes = F)
#   
#   
#   p = p + geom_sf(data=country_subset, fill=NA, color='grey15', linewidth=0.1)
#   
#   print(p)
#   message(glue("Saving: {output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}-{code}.pdf"))
#   ggsave(glue("{output}/{sector}/{scn}-{category}-{period}-{gwl_bin}-{spatial}-{unit}{slug}-{code}.pdf"), p)
#   
# }

# RE-ALIGNING COUNTRY BOUNDARIES ()
# country.shp$country = names$iso, PSX = PSE, SXM = SMX, KOS = KO-, SDS = SSD, merge CNM, CYN, CYP (call CYP), merge SAH with MAR (call MAR),
# merge BRT with SDN, remove KAB, merge USG and USA (Call USA), merge BRI with BRA (call BRA), merge SPI with CHL (call CHL),
# ALD=ALA, merge IOA & CSI & ATC with AUS (call AUS), PGA=SP-, CLP=CL-, merge BJN and COL (call col),
# remove SCR & SER

