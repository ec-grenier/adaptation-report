#' This function was designed to be very similar to the old get_econvar function
#' except instead of calling a netcdf file it calls a zarr file from the DSCIM system.
#' The zarr file from DSCIM is the source of truth for our economic data (GDP and
#' population). 
#' You input what econ data you want along with the regions (can do aggregated),
#' IAMs, SSPs and years you want it for, and it returns a dataframe with that information.
#' Note this function calls python under the hood and therefore is DEPENDENT on the
#' reticulate package and having a conda environment available to activate 
#' Inputs
#' [units] = the economic variables you want returned options are 'gdp', 'gdppc' 'pop', 
#'    'pop0to4', 'pop5to64', 'pop65plus', you can input as a vector if you want 
#'    multiple variables. Note if you ask for gdppc for an aggregated region under
#'    the hood the function pulls gdp and pop and recalculated gdppc. The GDP and 
#'    GDP pc values are in 2019 dollars.
#' [regions] = a list of regions you want the data for, also accepts 'all' if you want
#'    all IRs, 'iso' for country level, and 'global' for a single global value
#' [iam] = The IAM scenario options are 'low' or 'high'
#' [ssp] = The SSP scenario optiosn are 'SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5'
#' [year_list] = a vector of years you want the data for can be between 2010 and 2099

get_dscim_econ_vars = function(
  units,
  regions,
  iam,
  ssp,
  year_list){
  
  # Convert the regions input which maybe a string into a list of regions based on the heirarchy file
  region_list = return_region_list(regions)
  # Determining the resolution of the region list (ie is it IR or ISO level or a mix)
  resolution_list = check_resolution(region_list)
  # Having a bool flag for if there are any aggregated regions 
  is_aggregated = !is.null(resolution_list$aggregated)
  
  # creating varlist as the list of variables that will actually be pulled from the zarr file
  # for aggregated regions gdppc needs to be recalculated so GDPPC is not called and pop and gdp are added if needed
  if(is_aggregated & 'gdppc' %in% units) {
    varlist = units[! units == 'gdppc']
    if(! 'pop' %in% units){ varlist = c(varlist, 'pop')}
    if(! 'gdp' %in% units){ varlist = c(varlist, 'gdp')}
  } else {varlist = units}
  
  if(iam == 'low'){
    iam_model = 'IIASA GDP'
  } else if(iam == 'high'){
    iam_model = 'OECD Env-Growth'
  }
  
  use_condaenv("/project/cil/home_dirs/egrenier/envs/r-reticulate-ineq")
  source_python("/project/cil/home_dirs/egrenier/repos/inequality/python_functions/get_dscim_econ_vars.py")
  econ_vars_raw = open_dscim_econ_vars(ssp_in = ssp, model_in = iam_model, year_list = year_list,
                                       econ_vars = varlist)
  
  # Creating a df with the relationship between any aggregated regions in the region list and their IRs
  if (is_aggregated) {
    region_list_IR_level = get_children(region_list)
    
    region_list_IR_level = enframe(region_list_IR_level) %>%  
      unnest_longer(col=value,keep_empty = TRUE) %>% 
      mutate(value=if_else(is.na(value),name,value)) %>% 
      rename(parent = name,
             region = value)}
  
  econ_vars_raw = econ_vars_raw %>% 
    mutate(model = case_when(model == 'IIASA GDP' ~ 'low',
                             model == 'OECD Env-Growth' ~ 'high'))
  
  econ_vars_out = if ("all" %in% regions) {
    econ_vars_raw
  } else if ("global" %in% regions) {
    econ_vars_raw %>% 
      group_by(year, model) %>% 
      summarise_at(varlist, sum) %>% 
      {if ("gdppc" %in% units) mutate(., gdppc = gdp/pop) else .} %>% 
      mutate(region = 'global') %>% 
      select(all_of(c('year', 'region','model',units)))
  } else if (is_aggregated) {
    econ_vars_raw %>% 
      inner_join(region_list_IR_level, by = c('region' = 'region')) %>% 
      group_by(parent, year, model) %>% 
      summarise_at(varlist, sum) %>% 
      {if ("gdppc" %in% units) mutate(., gdppc = gdp/pop) else .} %>% 
      rename(region = parent) %>% 
      select(all_of(c('year', 'region','model',units)))
  } else {
    econ_vars_raw %>% 
      filter(region %in% region_list)
  }
  
  
}
