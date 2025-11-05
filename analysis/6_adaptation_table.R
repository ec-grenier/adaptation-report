#==============================================================================#
#'
#'
#' work in progress -- defining a bunch of data outputs to be used in release
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

packages = c("glue", "tidyverse", "multidplyr", "data.table", "reticulate")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

# I/O doesn't change across specs
input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/data_release'

#==============================================================================#
# set parameters ----

collect_data = function(sector, category, scn, unit, gwl_bin, period, spatial, spec){ 
  
  message(glue('---- Running {sector} {category} {scn} impacts ({gwl_bin}, {period}) ----'))
  message(glue('---- Units: {unit}'))
  message(glue('---- Spec: {spec}'))
  message(glue('---- Spatial: {spatial}'))
  message(glue("Reading: {input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv"))
  
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>%
    select(region, q50)
  
  if (scn == "fulladapt"){impacts = impacts %>% filter(nchar(region) <= 3)}
  
  if (spec == 'country'){
    
    impacts = impacts %>% filter(region != "")
    
    # get country names
    names = read_csv('/project/cil/gcp/regions/hierarchy.csv', skip=31, show_col_types = FALSE) %>% select(`region-key`, name) %>% rename(region=`region-key`)
    names = names %>% filter(nchar(region) <= 3 & region != "") 
    impacts = impacts %>% filter(nchar(region) == 3) %>% left_join(names)
    
    # arrange in descending order
    ranked = impacts %>% select(name, region, q50) %>% arrange(desc(q50)) %>% filter(!is.na(q50))
    
  } else if (spec == 'cont'){
    
    world = impacts %>% filter(region == "")
    impacts = impacts %>% filter(region != "")
    
    continents = read_csv('/project/cil/gcp/regions/continents2.csv', show_col_types = FALSE) %>% 
      select(`alpha-3`, region, `sub-region`) %>% 
      rename(iso = `alpha-3`,
             continent = region,
             sub_region = `sub-region`) %>%
      mutate(iso = ifelse(iso=='SXM', 'SMX', iso),
             continent = ifelse(iso=='ATA', 'Antarctica', continent),
             sub_region = ifelse(iso=='ATA', 'Antarctica', sub_region),
             continent = ifelse(iso=='RUS', 'Asia', continent)) %>%
      bind_rows(data.frame(iso = "KO-", continent = "Europe"),
                data.frame(iso = "CA-", continent = "Asia")) %>%
      mutate(continent = ifelse(continent == "Americas", sub_region, continent)) %>%
      filter(continent != "Antarctica") %>%
      select(iso, continent)
    
    pop = get_dscim_econ_vars('pop', 'iso', 'low', 'SSP2', 2050) %>% select(region, pop)
    
    impacts = impacts %>% left_join(pop) %>% left_join(continents, by=c("region"="iso"))
    impacts = impacts %>%
      group_by(continent) %>%
      summarise(q50 = weighted.mean(q50, w = pop, na.rm = TRUE)) %>%
      filter(!is.na(q50))
    
    ranked = impacts %>% arrange(desc(q50)) %>% rename(region = continent)
    
    world = world %>% mutate(region = "global")
    ranked = rbind(world,ranked)
    
  }
 
  return(ranked) 
}

#==============================================================================#
# Run scenarios (Country-level) ----

country_fulladapt = collect_data(sector="mortality", 
                                 category="combined", 
                                 scn="fulladapt", 
                                 unit="rates",
                                 gwl_bin="3_c", 
                                 period="midc",
                                 spatial="aggregated", 
                                 spec="country") %>%
  rename(fa.q50 = q50)
  
country_incadapt = collect_data(sector="mortality", 
                                 category="combined", 
                                 scn="incadapt", 
                                 unit="rates",
                                 gwl_bin="3_c", 
                                 period="midc",
                                 spatial="aggregated", 
                                 spec="country") %>%
  rename(ia.q50 = q50)

country_noadapt = collect_data(sector="mortality", 
                                category="combined", 
                                scn="noadapt", 
                                unit="rates",
                                gwl_bin="3_c", 
                                period="midc",
                                spatial="aggregated", 
                                spec="country") %>%
  rename(na.q50 = q50)

country_ranked = country_noadapt %>% left_join(country_incadapt) %>% left_join(country_fulladapt)

country_ranked = country_ranked %>% mutate(double_speed = ia.q50 + 2*(fa.q50 - ia.q50),
                                           half_speed = ia.q50 + 0.5*(fa.q50 - ia.q50))

write.csv(country_ranked, '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/data_release/mortality/combined-country-adapt-comparison-SSP2-low-3_c-midc.csv', row.names=F)

#==============================================================================#
# Run scenarios (continent level) ----

cont_fulladapt = collect_data(sector="mortality", 
                                 category="combined", 
                                 scn="fulladapt", 
                                 unit="rates",
                                 gwl_bin="3_c", 
                                 period="midc",
                                 spatial="aggregated", 
                                 spec="cont") %>%
  rename(fa.q50 = q50)

cont_incadapt = collect_data(sector="mortality", 
                                category="combined", 
                                scn="incadapt", 
                                unit="rates",
                                gwl_bin="3_c", 
                                period="midc",
                                spatial="aggregated", 
                                spec="cont") %>%
  rename(ia.q50 = q50)

cont_noadapt = collect_data(sector="mortality", 
                               category="combined", 
                               scn="noadapt", 
                               unit="rates",
                               gwl_bin="3_c", 
                               period="midc",
                               spatial="aggregated", 
                               spec="cont") %>%
  rename(na.q50 = q50)


cont_ranked = cont_noadapt %>% left_join(cont_incadapt) %>% left_join(cont_fulladapt)

cont_ranked = cont_ranked %>% mutate(double_speed = ia.q50 + 2*(fa.q50 - ia.q50),
                                     half_speed = ia.q50 + 0.5*(fa.q50 - ia.q50))

write.csv(cont_ranked, '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/data_release/mortality/combined-continent-adapt-comparison-SSP2-low-3_c-midc.csv', row.names=F)
