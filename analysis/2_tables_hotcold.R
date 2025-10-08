#==============================================================================#
#'
#'
#' work in progress -- defining a bunch of data outputs to be used in release
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

packages = c("glue", "dplyr", "readr", "data.table", "reticulate")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
rm(packages)

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

# I/O doesn't change across specs
input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/data_release'

#==============================================================================#
# set parameters ----

define_ranking = function(df1, df2){
  
  df1 = df1 %>% arrange(q50) %>% mutate(index_old = row_number()) %>% select(region, index_old)
  df2 = df2 %>% arrange(q50) %>% mutate(index = row_number())
  
  df2 = df2 %>% left_join(df1, by="region") %>% 
    mutate(delta = index_old - index) %>% 
    mutate(delta = if_else(delta > 0, paste0("+", as.character(delta)), as.character(delta))) %>%
    mutate(delta = if_else(delta == 0, "--" , delta))
  
  return(df2)
}

run_table = function(sector, category, scn, unit, gwl_bin, period, spatial, ranking){
  message(glue('---- Running {sector} {category} {scn} impacts ({gwl_bin}, {period}) ----'))
  message(glue('---- Units: {unit}'))
  message(glue('---- Ranking: {ranking}'))
  message(glue('---- Spatial: {spatial}'))
  
  message(glue("Reading: {input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv"))
  
  impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>% 
    select(region, q50)
  
  impacts_cold = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low-cold.csv")) %>% 
    select(region, q50)
  
  continents = read_csv('/project/cil/gcp/regions/continents2.csv', show_col_types = FALSE) %>% 
    select(`alpha-3`, region, `sub-region`) %>% 
    rename(iso = `alpha-3`,
           continent = region,
           sub_region = `sub-region`) %>%
    mutate(iso = ifelse(iso=='SXM', 'SMX', iso),
           continent = ifelse(iso=='ATA', 'Antarctica', continent),
           sub_region = ifelse(iso=='ATA', 'Antarctica', sub_region)) %>%
    bind_rows(data.frame(iso = "KO-", continent = "Europe"),
              data.frame(iso = "CA-", continent = "Asia")) %>%
    mutate(continent = ifelse(continent == "Americas", sub_region, continent)) %>%
    filter(continent != "Antarctica") %>%
    select(iso, continent)
  
  continents_list = unique(continents$continent)
  
  continent_code = c(
    "Asia" = "asia",
    "Europe" = "eu",
    "Africa" = "afr",
    "Oceania" = "oce",
    "Latin America and the Caribbean" = "lac",
    "Antarctica" = "ant",
    "Northern America" = "na"
  )
  
  
  if (ranking == 'city'){
    
    cities = read.csv('/project/cil/gcp/regions/500k_cities.csv') %>% select(city, region = Region_ID)
    cities = cities %>% mutate(iso = substr(region, 1, 3)) %>% distinct(region, .keep_all = TRUE)
    
    # To get country names in the table
    names = read_csv('/project/cil/gcp/regions/hierarchy.csv', skip=31, show_col_types = FALSE) %>% select(`region-key`, name) %>% rename(region=`region-key`)
    names = names %>% filter(nchar(region) <= 3 & region != "") %>% rename(iso = region)
    
    cities = cities %>% left_join(names) %>% mutate(city = glue("{city}, {name}"))
    
    cities_ranked = cities %>% left_join(impacts)
    cities_ranked_cold = cities %>% left_join(impacts_cold)
    
    cities_ranked_cold = define_ranking(cities_ranked, cities_ranked_cold) %>% select(city, region, q50, delta)
    
    # save out full ranking
    message(glue('Saving: cities_ranked-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold.csv \n in {output}/{sector}'))
    write.csv(cities_ranked_cold, glue('{output}/{sector}/cities_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold.csv'), row.names=F)
    
    # print top 25
    txt = cities_ranked_cold %>% select(city, q50, delta) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
    cat(txt)
    
    within_cont = cities %>% left_join(impacts) %>% left_join(continents)
    within_cont_cold = cities %>% left_join(impacts_cold) %>% left_join(continents)
    
    for (c in continents_list){
      
      within_cont_sub = within_cont %>% filter(continent == c)
      within_cont_cold_sub = within_cont_cold %>% filter(continent == c)
      
      ranked = define_ranking(within_cont_sub, within_cont_cold_sub) %>% select(city, region, q50, delta)
      
      print(glue("\n City ranking for continent: {c}"))
      txt = ranked %>% select(city, q50, delta) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
      cat(txt)
      
      cont = continent_code[c] # get continent code 
      
      message(glue('Saving: cities_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold-{cont}.csv \n in {output}/{sector}'))
      write.csv(ranked, glue('{output}/{sector}/cities_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold-{cont}.csv'), row.names=F)
    }
    
  } else if (ranking == 'ir'){
    
    # arrange in descending order
    ir_ranked_cold = define_ranking(impacts, impacts_cold)
    
    names = read.csv('/project/cil/gcp/regions/hierarchy-flat.csv') %>% select(region.key, name) %>% rename(region=region.key)
    
    ir_ranked_cold = ir_ranked_cold %>% left_join(names) %>% select(name, region, q50, delta)
    
    # print top 25
    txt = ir_ranked_cold %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
    cat(txt)
    # save out full ranking
    message(glue('Saving: impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold.csv \n in {output}/{sector}'))
    write.csv(ir_ranked_cold, glue('{output}/{sector}/impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold.csv'), row.names=F)
    
    within_cont = impacts %>% mutate(iso = substr(region,1,3)) %>% left_join(continents)
    within_cont_cold = impacts_cold %>% mutate(iso = substr(region,1,3)) %>% left_join(continents)
    
    for (c in continents_list){
      
      within_cont_sub = within_cont %>% filter(continent == c)
      within_cont_cold_sub = within_cont_cold %>% filter(continent == c)
      
      ranked = define_ranking(within_cont_sub, within_cont_cold_sub) %>% left_join(names) %>% select(name, region, q50, delta)
      
      print(glue("\n City ranking for continent: {c}"))
      txt = ranked %>% select(name, region, q50, delta) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
      cat(txt)
      
      cont = continent_code[c] # get continent code 
      
      message(glue('Saving: impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold-{cont}.csv \n in {output}/{sector}'))
      write.csv(ranked, glue('{output}/{sector}/impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}-cold-{cont}.csv'), row.names=F)
    }
  }
}

#==============================================================================#
# Run scenarios ----

# this will run all combos
run_table(sector="mortality",
          category="combined",
          scn="fulladapt",
          unit="rates",
          gwl_bin="3_c",
          period="midc",
          spatial="ir_level",
          ranking="city")

run_table(sector="mortality",
          category="combined",
          scn="fulladapt",
          unit="levels",
          gwl_bin="3_c",
          period="midc",
          spatial="ir_level",
          ranking="city")

run_table(sector="mortality",
          category="combined",
          scn="fulladapt",
          unit="rates",
          gwl_bin="3_c",
          period="midc",
          spatial="ir_level",
          ranking="ir")
