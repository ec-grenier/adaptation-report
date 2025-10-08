#==============================================================================#
#'
#'
#' work in progress -- decile plots
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

packages <- c("glue", "tidyverse", "multidplyr", "data.table", "reticulate", "purrr")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

# I/O doesn't change across specs
input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/decile'

#==============================================================================#
# plotting function ----


decile_plot = function(sector, category, scn, unit, gwl_bin, period, spatial){
  
  ylab = ifelse(unit == "rates", "Deaths per 100,000", "Deaths")
  type = ifelse(spatial == "ir_level", "IR", "country")
  plot_title = glue("{sector} {category} {scn} impacts, {period}, {gwl_bin} ({type} level)")
  
  message(glue("Plotting: {sector} {category} {scn} impacts, {period}, {gwl_bin} ({type} level)"))
  
  if (spatial == 'aggregated' & unit == 'levels'){
    
    impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>% 
      select(region, mean, q25, q50, q75)
    
    impacts = impacts %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>%
      summarise(mean = sum(mean, na.rm = TRUE),
                q25 = sum(q25, na.rm = TRUE),
                q50 = sum(q50, na.rm = TRUE),
                q75 = sum(q75, na.rm = TRUE))
    
  } else {
    
    impacts = read.csv(glue("{input}/{sector}/{category}-{scn}-{spatial}-{unit}-{gwl_bin}-{period}-SSP2-low.csv")) %>%
      select(region, mean, q25, q50, q75)
    
    if (spatial == "aggregated"){
      
      impacts = impacts  %>% filter(nchar(region) <= 3 & region != "")
    
    }
    
  }
  
  if (spatial == "ir_level"){
    
    gdppc = get_dscim_econ_vars(c('gdppc', 'pop'), 'all', 'low', 'SSP2', 2050) %>% select(region, pop, gdppc)
    
  } else {
    
    gdppc = get_dscim_econ_vars(c('gdppc', 'pop'), 'iso', 'low', 'SSP2', 2050) %>% select(region, pop, gdppc)
    
  }

  impacts = impacts %>% left_join(gdppc)
  
  if (spatial == "ir_level"){
    impacts = impacts %>%
      filter(!is.na(gdppc)) %>%
      arrange(gdppc) %>% 
      mutate(cum_pop = cumsum(pop) / sum(pop),
             income_decile = cut(cum_pop,
                                 breaks = seq(0, 1, 0.1),
                                 labels = 1:10,
                                 include.lowest = TRUE))
  } else {
    impacts = impacts  %>%
      filter(!is.na(gdppc)) %>% 
      mutate(income_decile = ntile(gdppc, 10))
  }
  
  decile_summary = impacts %>%
    group_by(income_decile) %>%
    summarise(q25 = weighted.mean(q25, pop),
              q50 = weighted.mean(q50, pop),
              q75 = weighted.mean(q75, pop),
              .groups = "drop")

  # define white bar to be 0.005 of the average interquartile range
  val = decile_summary %>% mutate(iqr = q75 - q25) %>% summarise(val = mean(iqr, na.rm = TRUE) * 0.005) %>% pull(val)
  
  p = ggplot(decile_summary, aes(x = factor(income_decile))) +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    geom_linerange(aes(ymin = q25, ymax = q75), color = "#d0475d", size = 18) +
    geom_linerange(aes(ymin = q50-val, ymax = q50+val), color = "white", size = 18) +
    labs(x = "Global income decile",
         y = ylab,
         title = plot_title) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size=14),
          axis.title = element_text(color = "black", size=16),
          plot.title = element_text(color = "black", hjust = 0.5),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.8),
          axis.ticks.length = unit(6, "pt"),
          panel.grid = element_blank())
  
  ggsave(glue('{output}/{sector}/income_deciles-bar_plot-{type}-{scn}-{category}-{unit}-{period}-{gwl_bin}.jpg'), p)
}


#==============================================================================#
# call2plot ----


params = crossing(age = c("combined", "oldest", "older", "young"),
                  u = c("rates", "levels"),
                  space = c("ir_level", "aggregated"))
  

pmap(params, ~ decile_plot(sector = "mortality",
                           category = ..1,
                           scn = "fulladapt",
                           unit = ..2,
                           gwl_bin = "3_c",
                           period = "midc",

