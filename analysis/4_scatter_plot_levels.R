#==============================================================================#
#'
#'
#' work in progress -- scatter plots
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

packages = c("glue", "tidyverse", "multidplyr", "data.table", "reticulate", "scales")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

# I/O doesn't change across specs
input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/scatter'

#==============================================================================#
# plotting function ----


country_scatter_plot = function(sector, category, scn, gwl_bin, period, zoom=NULL, slug=''){
  
  ylab = "Deaths per 100,000"
  xlab = "2025 Country GDP per capita (2019 $ USD PPP)"
  plot_title = glue("Country level {sector} {category} {scn} impacts, {period}, {gwl_bin}")
  subtitle=''
  message(glue("[Plotting: ] {sector} {category} {scn} impacts, {period}, {gwl_bin}"))
  
  # get levels by summing impacts from ir to iso
  levels = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-levels-{gwl_bin}-{period}-SSP2.csv")) %>% select(region, q50)
  levels = levels %>% mutate(region = substr(region, 1, 3)) %>% group_by(region) %>% summarise(q50 = sum(q50, na.rm = TRUE))
  levels = levels %>% rename(levels.q50 = q50)
  
  # get country aggregated rates
  rates = read.csv(glue("{input}/{sector}/{category}-{scn}-aggregated-rates-{gwl_bin}-{period}-SSP2.csv")) %>%
    filter(nchar(region) <= 3 & region != "") %>% 
    select(region, q50) %>% 
    rename(rates.q50 = q50)

  # get income
  gdppc = get_dscim_econ_vars('gdppc', 'iso', 'low', 'SSP2', 2025) %>% ungroup() %>% select(region, gdppc)

  # plot data
  impacts = rates %>% left_join(levels) %>% left_join(gdppc) %>% filter(!is.na(gdppc))
  
  if (!is.null(zoom)) {
    impacts = impacts %>%
      filter(rates.q50 >= quantile(rates.q50, zoom/100, na.rm = TRUE),
             rates.q50 <= quantile(rates.q50, 1 - zoom/100, na.rm = TRUE))
    subtitle=glue("Top and bottom {zoom}% truncated")
    slug=glue('{zoom}pct_trunc')
  }
  
  impacts = impacts %>%
    mutate(type = ifelse(levels.q50 >= 0, "Lives lost", "Lives saved"),
           size_val = abs(levels.q50))
  
  p = ggplot(impacts, aes(x = gdppc, y = rates.q50)) +
    geom_hline(yintercept=0, linetype="dashed", color="black") +
    geom_point(aes(size = size_val, color = type),
               shape = 21, fill = NA, stroke = 0.7) +
    scale_color_manual(values = c("Lives lost" = "#d0475d", "Lives saved" = "blue")) +
    scale_size_continuous(name = "Mortality impacts (people/year)") +
    scale_x_continuous(labels = comma) +
    labs(x = xlab,
         y = ylab,
         title = plot_title,
         subtitle=subtitle) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size=14),
          axis.title = element_text(color = "black", size=16),
          plot.title = element_text(color = "black", hjust = 0.5),
          plot.subtitle = element_text(color = "black", hjust = 0.5),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.8),
          axis.ticks.length = unit(6, "pt"),
          panel.grid = element_blank())
  
  print(p)
  message(glue('[Saving: ] country_scatter-{scn}-{category}-{period}-{gwl_bin}{slug}.jpg'))
  ggsave(glue('{output}/{sector}/country_scatter-{scn}-{category}-{period}-{gwl_bin}{slug}.jpg'), p, height=5, width=8)
  message(" ---- DONE ---- \n ")
}


#==============================================================================#
# call2plot ----

for (age in c("combined", "oldest", "older", "young")){
  country_scatter_plot(sector = "mortality",
                       category = age,
                       scn = "fulladapt",
                       gwl_bin = "3_c",
                       period = "midc",
                       zoom=NULL) 
}

