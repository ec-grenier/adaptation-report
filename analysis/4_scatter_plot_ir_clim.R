#==============================================================================#
#'
#'
#' work in progress -- scatter plots
#'
#'
#==============================================================================#

#==============================================================================#
# load packages and paths ----

packages <- c("glue", "tidyverse", "multidplyr", "data.table", "reticulate", "scales")

invisible(lapply(packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))

source('/project/cil/home_dirs/egrenier/repos/inequality/4_figures_and_tables/load_utils.R')
source('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/code/analysis/utils/get_dscim_econ_vars_ineq.R')

# I/O doesn't change across specs
input ='/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/data/analysis_ready'
output = '/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/scatter'

#==============================================================================#
# data ----

climtasmax = fread("/project/cil/gcp/climate/_spatial_data/impactregions/weather_data/csv_daily/GMDF_tmax_temp_and_spline_avg_year.csv") %>% 
  rename(temp = value.x,
         region = hierid) %>%
  group_by(region) %>%
  summarize(climtasmax = mean(temp))

#==============================================================================#
# plotting function ----


ir_scatter_plot = function(sector, category, scn, gwl_bin, period, zoom=NULL, slug=''){
  
  ylab = "Deaths per 100,000"
  xlab = "Countries ranked by Historical Average of Daily Max Temp"
  plot_title = glue("Impact Region {sector} {category} {scn} impacts, {period}, {gwl_bin}")
  subtitle=''
  message(glue("[Plotting: ] {sector} {category} {scn} impacts, {period}, {gwl_bin}"))
  
  # get country aggregated rates
  rates = read.csv(glue("{input}/{sector}/{category}-{scn}-ir_level-rates-{gwl_bin}-{period}-SSP2-low-cold.csv")) %>%
    select(region, q50) %>% 
    rename(rates.q50 = q50)

  # plot data
  impacts = rates %>% left_join(climtasmax) %>% left_join(levels)
  
  if (!is.null(zoom)) {
    impacts = impacts %>%
      filter(rates.q50 >= quantile(rates.q50, zoom/100, na.rm = TRUE),
             rates.q50 <= quantile(rates.q50, 1 - zoom/100, na.rm = TRUE))
    subtitle=glue("Top and bottom {zoom}% truncated")
    slug=glue('{zoom}pct_trunc')
  }
  
  p = ggplot(impacts, aes(x = climtasmax, y = rates.q50)) +
    geom_hline(yintercept=0, linetype="dashed", color="black") +
    geom_point(size=0.5,  color="blue")+
    labs(x = xlab,
         y = ylab,
         title = plot_title,
         subtitle=subtitle) +
    theme_minimal() +
    theme(axis.text = element_text(color = "black", size=12),
          axis.title = element_text(color = "black", size=12),
          plot.title = element_text(color = "black", hjust = 0.5),
          plot.subtitle = element_text(color = "black", hjust = 0.5),
          axis.line = element_line(color = "black"),
          axis.ticks = element_line(color = "black", linewidth = 0.8),
          axis.ticks.length = unit(6, "pt"),
          panel.grid = element_blank())
  
  print(p)
  message(glue('[Saving: ] country_scatter-{scn}-{category}-{period}-{gwl_bin}{slug}.jpg'))
  ggsave(glue('{output}/{sector}/ir_rates_temp_scatter-{scn}-{category}-{period}-{gwl_bin}{slug}.jpg'), p, height=5, width=4)
  message(" ---- DONE ---- \n ")
}


#==============================================================================#
# call2plot ----

for (age in c("combined", "oldest", "older", "young")){
  ir_scatter_plot(sector = "mortality",
                  category = age,
                  scn = "fulladapt",
                  gwl_bin = "3_c",
                  period = "midc",
                  zoom=NULL,
                  slug='-cold') 
  
}

