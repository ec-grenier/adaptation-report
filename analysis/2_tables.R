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

run_table = function(sector, category, scn, unit, gwl_bin, period, spatial, ranking){ 
    
    message(glue('---- Running {sector} {category} {scn} impacts ({gwl_bin}, {period}) ----'))
    message(glue('---- Units: {unit}'))
    message(glue('---- Ranking: {ranking}'))
    message(glue('---- Spatial: {spatial}'))
    message(glue("Reading: {input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv"))
    if (spatial == 'aggregated' & unit == 'levels'){
        impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>% 
            select(region, q25, q50, q75)
    } else {
        impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>%
            select(region, q25, q50, q75)
    }

    if (spatial == 'aggregated'){
        if (unit == 'levels'){
            impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>%
                summarise(q25 = sum(q25, na.rm = TRUE),
                          q50 = sum(q50, na.rm = TRUE),
                          q75 = sum(q75, na.rm = TRUE))
                    
          } else {
            impacts = impacts %>% filter(nchar(region) <= 3 & region != "")
          }
    } 

    if (ranking == 'city'){
        
        cities = read.csv('/project/cil/gcp/regions/500k_cities.csv') %>% select(city, region = Region_ID)
        cities = cities %>% mutate(iso = substr(region, 1, 3))

        names = read_csv('/project/cil/gcp/regions/hierarchy.csv', skip=31, show_col_types = FALSE) %>% select(`region-key`, name) %>% rename(region=`region-key`)
        names = names %>% filter(nchar(region) <= 3 & region != "") %>% rename(iso = region)

        cities = cities %>% left_join(names) %>% mutate(city = glue("{city}, {name}"))
        
        cities_ranked = cities %>% left_join(impacts) %>% arrange(desc(q50))
    
        # save out full ranking
        message(glue('Saving: cities_ranked-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv \n in {output}/{sector}'))
        write.csv(cities_ranked, glue('{output}/{sector}/cities_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv'), row.names=F)

        # print top 25
        txt = cities_ranked %>% select(city, q50) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
        cat(txt)
  
    } else if (ranking == 'ir'){

        # arrange in descending order
        ir_ranked = impacts %>% arrange(desc(q50))
  
        names = read.csv('/project/cil/gcp/regions/hierarchy-flat.csv') %>% select(region.key, name) %>% rename(region=region.key)
  
        ir_ranked = ir_ranked %>% left_join(names) %>% select(name, region, q25, q50, q75)
  
        # save out full ranking
        message(glue('Saving: impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv \n in {output}/{sector}'))
        write.csv(ir_ranked, glue('{output}/{sector}/impact_regions_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv'), row.names=F)
        
        txt = ir_ranked %>% select(name, q50) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
        cat(txt)
  
    } else if (ranking == 'country'){

        # get country names
        names = read_csv('/project/cil/gcp/regions/hierarchy.csv', skip=31, show_col_types = FALSE) %>% select(`region-key`, name) %>% rename(region=`region-key`)
        names = names %>% filter(nchar(region) <= 3 & region != "") 
        impacts = impacts %>% filter(nchar(region) == 3) %>% left_join(names)
        
        # arrange in descending order
        country_ranked = impacts %>% select(name, region, q25, q50, q75) %>% arrange(desc(q50)) %>% filter(!is.na(q50))

        # save out full ranking
        message(glue('Saving: countries_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv \n in {output}/{sector}'))
        write.csv(country_ranked, glue('{output}/{sector}/countries_ranked-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv'), row.names=F)

        # print top 25
        txt = country_ranked %>% select(name, q50) %>% slice_head(n = 25) %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
        cat(txt)

    } else if (ranking == 'cont'){

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
        
        if (unit == "levels"){
    
            if (spatial == 'aggregated'){
                stop("Invalid combination, please run continent levels ranking with spatial = 'ir_level'")
            }

            impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>% summarise(q50 = sum(q50, na.rm = TRUE))
    
            impacts = impacts %>% left_join(continents, by = c('region'='country')) %>% 
              filter(!is.na(continent)) %>% 
              group_by(continent) %>% 
              summarise(q50 = sum(q50, na.rm = TRUE)) 

        } else {

            if (spatial == 'ir_level'){
                stop("Invalid combination, please run continent levels ranking with spatial = 'aggregated'")
            }
            
            # population weighted average mortality rate
            impacts = impacts %>% filter(nchar(region) <= 3 & region != "")
            pop = get_dscim_econ_vars('pop', 'iso', 'low', 'SSP2', 2050) %>% select(region, pop)
            impacts = impacts %>% left_join(pop) %>% left_join(continents, by=c("region"="country"))
            impacts = impacts %>%
              group_by(continent) %>%
              summarise(q50 = weighted.mean(q50, w = pop, na.rm = TRUE)) %>%
              filter(!is.na(q50))
        }
    
        cont_ranked = impacts %>% arrange(desc(continent))

        message(glue('Saving: continent_impacts-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv \n in {output}/{sector}'))
        write.csv(cont_ranked, glue('{output}/{sector}/continent_impacts-{sector}-{category}-{scn}-{unit}-{gwl_bin}-{period}.csv'), row.names=F) 

        txt = cont_ranked %>% mutate(q50 = sprintf("%.1f", q50)) %>% format_tsv()
        cat(txt) 
    }
}

#==============================================================================#
# Run scenarios ----

# this will run all combos
for (cat in c('combined', 'oldest', 'older', 'young')){
    for (u in c('rates', 'levels')){
         run_table(sector="mortality", 
                   category=cat, 
                   scn="fulladapt", 
                   unit=u,
                   gwl_bin="3_c", 
                   period="midc", 
                   spatial="ir_level",   # "aggregated" only in combination with "country" ranking or "cont" with "rates"
                   ranking="ir")         # 'ir' or 'city' or 'country' or 'cont'
    }
}

for (cat in c('combined', 'oldest', 'older', 'young')){
    for (u in c('rates', 'levels')){
         run_table(sector="mortality", 
                   category=cat, 
                   scn="fulladapt", 
                   unit=u,
                   gwl_bin="3_c", 
                   period="midc", 
                   spatial="ir_level", 
                   ranking="city") 
    }
}

for (cat in c('combined', 'oldest', 'older', 'young')){
    for (u in c('rates', 'levels')){
         run_table(sector="mortality", 
                   category=cat, 
                   scn="fulladapt", 
                   unit=u,
                   gwl_bin="3_c", 
                   period="midc", 
                   spatial="aggregated", 
                   ranking="country") 
    }
}

for (cat in c('combined', 'oldest', 'older', 'young')){
    for (u in c('levels')){
         run_table(sector="mortality", 
                   category=cat, 
                   scn="fulladapt", 
                   unit=u,
                   gwl_bin="3_c", 
                   period="midc", 
                   spatial="ir_level", 
                   ranking="cont") 
    }
}

for (cat in c('combined', 'oldest', 'older', 'young')){
    for (u in c('rates')){
         run_table(sector="mortality", 
                   category=cat, 
                   scn="fulladapt", 
                   unit=u,
                   gwl_bin="3_c", 
                   period="midc", 
                   spatial="aggregated", 
                   ranking="cont") 
    }
}

for (cat in c('combined')){
  for (u in c('rates')){
    run_table(sector="mortality", 
              category=cat, 
              scn="fulladapt", 
              unit=u,
              gwl_bin="3_c", 
              period="midc", 
              spatial="aggregated", 
              ranking="country") 
  }
}

