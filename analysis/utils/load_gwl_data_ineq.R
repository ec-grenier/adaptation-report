#' This function combines multiple extractions into a single flat file.
#' It is used by all or almost all the inequality functions to pull GWL data in the
#' correct format. It can also combine all gwl groups OR all adaption scenarios together
#' into a single file holding all other variables constant. (It can technically it 
#' can combine all GWLs AND all adaption scenarios but I am pretty sure that will 
#' break the plot functions so don't do that)
# Note on using this for Ag, inputting 'all_crop' and 
# 'extraction_agriculture_rebased' results in the function pulling the true 
# extractions and combining them into a single file broken out by crop. Inputting 
# 'all_crop' and 'extraction_agriculture_rebased_all_crop' tells the function to 
# pull the pseudo-extraction where all the crops are aggregated into a single value 
# for each IR, see combine_all_crop_ag_ineq.R for more info.
# Another note on Ag, inputting the true extraction ie 'extraction_agriculture_rebased'
# Results an adjustment for CO2 fertilization being added. See ag_help_tools_ineq.R for details
# pseudo extractions also have the CO2 fertilization applied because they are generated 
# via this function, therefore we do not want reapply the adjustment a second time when a 
# pseudo extraction is called. 
# Input 
# input = directory path to all extraction data, default is set to object DB
# spatial_level = what spatial level extraction you wish to pull and combine
  # 'ir' pulls ir level data
  # 'iso' pulls an aggregate extraction and then returns GWLs at the ISO level
    # 'iso' is only used in one function cumulative_damages_v_emissions
  # 'gobal' pulls an aggregate extraction and then returns GWLs at the global level
    # we don't use this anywhere it's just theortically possible given the data and how the code is written
    # use at your own risk
# sector_basename = the specific directory that contains the extractions for the 
  # sector in question. 
  # input and sector_basename are separate input so that way sector_basename can 
  # act as a flag to direct the function to correctly handle the pseudo-extractions
  # in the ag sector. The main difference is these "extraction" are already pre pivoted
# gwl = list of global warming levels to be combined, options are '1_5_c', '2_c', '3_c', '4_c' and 'all' which returns all for GWLs
# ssp = ssp for the scenario of interest, options SSP2 - SSP4, default SSP3
# Note it breaks on SSP1 for GWL bins tied to RCP 8.5 and SSP5 for GWL bins tied to RCP 4.5 
# scn = adaption scenario for the scenario of interest, options are "fulladapt", "incadapt", "noadapt" for energy & labor; "fulladaptcosts" for AMEL; "costs", "fulladapt", "fulladaptcosts", "incadapt", "noadapt", or 'all' which combines all scn together for mortality
# see RA manual for more discussion on what these levels mean
# category = sector specific category, options are "OTHERIND_electricity", "OTHERIND_other_energy" for energy; "young", "older", "oldest", "combined" for mortality; "allrisk", "highrisk", "lowrisk" for labor; "AMEL_m0_vly", "AMEL_m3_vly", "AMEL_m3_vsl" for AMEL
# iam = income IAM level for the scenario, options are low or high, default low
# unit = sector specific input for the units the data should be in, options are "impactpc" for energy; "rates" or "levels" for mortality; "min" for labor
# timeperiod = if the scenario is end of century (2080-2099) or midcentury (2040-2059) options are endc or midc respectively, default end of century
# It does account for the the multi rcps in 1.5C mid century bucket problem

combine_gwl_data = function(
    input,
    sector_basename,
    spatial_level = 'ir',
    gwl,
    ssp='SSP3',
    scn,
    category,
    iam='low',
    unit,
    timeperiod='endc') {
  
  if ('all' %in% gwl) {
    gwl_list = c('1_5_c', '2_c', '3_c', '4_c', '5_c')} else {
      gwl_list = gwl}

  # Energy & labor sector does not have a fulladaptcosts scenario
  # using category as proxy for sector since passing sector through would necessitate a lot of changes
  if ('all' %in% scn & category %in% c('OTHERIND_electricity', 'OTHERIND_other_energy', 'allrisk', 'highrisk', 'lowrisk')) {
      scn_list = c('fulladapt', 'incadapt', 'noadapt')
    } else if('all' %in% scn & category %in% c('AMEL_m0_vly', 'AMEL_m3_vly', 'AMEL_m3_vsl')){
      scn_list = c('fulladaptcosts')
    } else if ('all' %in% scn & category %in% c('facts')){
      scn_list = c('fulladaptcosts', 'noadapt')
    } else if('all' %in% scn){
      scn_list = c('fulladapt', 'fulladaptcosts', 'incadapt', 'noadapt')
    } else {
      scn_list = scn} 
  
  # Setting put values to loop and combine across multiple categories
  # Only used when combining all the crops together into a pseudo-extraction see
  # documentation above and combine_all_crop_ag_ineq.R for more details 
  if(category == 'all_crop' & sector_basename == 'extraction_agriculture_rebased') {
    category_list = c('cassava', 'corn', 'rice', 'sorghum', 'soy', 'wheat_combined')
  } else if(category == 'wheat') {
    category_list = 'wheat_combined'
  } else {category_list = category}
  
  # Hard coding the unit to be logyield for ag since all extractions are in this unit
  # Plot functions may be fed a different unit which they will adjust. 
  # It was easier to change the unit here than have every plot function handle to unit values.
  if(category %in% c('cassava', 'corn', 'rice', 'sorghum', 'soy', 'wheat', 'all_crop')){
    unit = 'logyield'
  }
  
  iam_list = iam
  
  if(spatial_level == 'ir') {
    spatial_file = 'ir_level'
  } else {spatial_file = 'aggregated'}
  
  return_data = data.frame()
  
  for(iam in iam_list){
    for(category_bin in category_list){
      for (scn_bin in scn_list){
        for(gwl_bin in gwl_list){
          
          print(glue('Pulling data for {scn_bin} at {gwl_bin}'))
          print(sector_basename)
          if (!(sector_basename %in% c('extraction_coastal_rebased', 'extraction_CAMEL_rebased'))){
            # If you are in a mixed RCP gwl bin and not Ag All crop pull full valuescsv for both rcps 
            if (sector_basename != 'extraction_agriculture_rebased_all_crop' &
                ((gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'midc') |
                 (gwl_bin == '3_c' & timeperiod == 'endc'))) {
              print(glue('{input}/{sector_basename}/rcp45-{ssp}-{gwl_bin}_{timeperiod}_a-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-valuescsv.csv'))
              raw_gwl_data_a = read.csv(glue('{input}/{sector_basename}/rcp45-{ssp}-{gwl_bin}_{timeperiod}_a-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-valuescsv.csv'))
              print(glue('{input}/{sector_basename}/rcp85-{ssp}-{gwl_bin}_{timeperiod}_b-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-valuescsv.csv'))
              raw_gwl_data_b = read.csv(glue('{input}/{sector_basename}/rcp85-{ssp}-{gwl_bin}_{timeperiod}_b-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-valuescsv.csv'))
              
              rcp = list('rcp45', 'rcp85')
              
              #Ag data across all crops is already precombined so there is no need to call the rcp's seperately
            } else if(sector_basename == 'extraction_agriculture_rebased_all_crop' &
                      ((gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'midc') |
                       (gwl_bin == '3_c' & timeperiod == 'endc'))) {
              
              raw_gwl_data = read.csv(glue('{input}/{sector_basename}/rcp45_rcp85-{ssp}-{gwl_bin}_{timeperiod}-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-edfcsv.csv'))

                            
            } else if (gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'endc') {
              print(glue('{input}/{sector_basename}/rcp45-{ssp}-{gwl_bin}_{timeperiod}-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-edfcsv.csv'))
              raw_gwl_data = read.csv(glue('{input}/{sector_basename}/rcp45-{ssp}-{gwl_bin}_{timeperiod}-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-edfcsv.csv'))
              
              print("rcp45")
              rcp = 'rcp45'
              
            } else if((gwl_bin == '3_c' & timeperiod == 'midc') |
                      (gwl_bin %in% c('4_c', '5_c') & timeperiod == 'endc')) {
              
              raw_gwl_data = read.csv(glue('{input}/{sector_basename}/rcp85-{ssp}-{gwl_bin}_{timeperiod}-no_surrogate-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-edfcsv.csv'))
              
              print("rcp85")
              rcp = 'rcp85'
              
            } else if (gwl_bin %in% c('4_c', '5_c') & timeperiod == 'midc') {
              break #assumes there are no 4 or 5 c GWL bins in the midcentury, brittle but effective
            }
            head(raw_gwl_data)
            # If not pulling IR level data then select the correct aggregated regions from 
            # the aggregated file
            if(spatial_level != 'ir' & 
               ((gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'midc') | 
                (gwl_bin == '3_c' & timeperiod == 'endc'))
            ) {
              
              region_list = return_region_list(spatial_level)
              
              raw_gwl_data_a = raw_gwl_data_a %>%
                filter(region %in% region_list) %>% 
                # For aggregated level extractions these IRs come in with NA values because they have zero pop
                # Filter them out so that mean and quantile work
                # Yes technically I should be calling the pop data and filtering out any IR with zero pop
                # But that is slow and I am lazy so hard coding it is
                filter(!(region %in% c('ATA', 'ATF', 'BVT', 'CL-', 'HMD', 'IOT', 'SGS', 'SP-') & is.na(value)))
              
              raw_gwl_data_b = raw_gwl_data_b %>%
                filter(region %in% region_list) %>% 
                filter(!(region %in% c('ATA', 'ATF', 'BVT', 'CL-', 'HMD', 'IOT', 'SGS', 'SP-') & is.na(value)))
              
            } else if(spatial_level != 'ir') {
              
              region_list = return_region_list(spatial_level)
              
              raw_gwl_data = raw_gwl_data %>%
                filter(region %in% region_list) %>% 
                filter(!(region %in% c('ATA', 'ATF', 'BVT', 'CL-', 'HMD', 'IOT', 'SGS', 'SP-') & is.na(mean)))
              
            }
            
            # For true ag extraction the CO2 fertilization must be added 
            # See the ag_help_tools_ineq.R for more info on why we do this
            # Pseudo-extractions already have CO2 fert added so they skip this step
            if(sector_basename == 'extraction_agriculture_rebased') {
              
              # Formatting the raw_gwl_data correctly to be handed over to the CO2 fert function
              if((gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'midc') |
                 (gwl_bin == '3_c' & timeperiod == 'endc')){
                
                raw_gwl_list = list(raw_gwl_data_a, raw_gwl_data_b)
                
              } else if ((gwl_bin %in% c('1_5_c', '2_c', '4_c', '5_c') & timeperiod == 'endc') | 
                         (gwl_bin == '3_c' & timeperiod == 'midc')){
                
                raw_gwl_data = raw_gwl_data %>%
                  pivot_longer(., mean:names(.)[ncol(.)], names_to = 'quantile', values_to = 'value')
                
                raw_gwl_list = list(raw_gwl_data)
                
              }
              
              if(timeperiod == 'midc'){
                start_year = 2040
                end_year = 2059
              } else if(timeperiod == 'endc'){
                start_year = 2080
                end_year = 2099
              }
              
              # Applying CO2 fertilization adjustment
              # This function is set up so it can handle either one or two sets of data
              # This means both single and mixed RCP GWL bins can run through the function easily
              raw_gwl_data = map2(raw_gwl_list, rcp, CO2_fertilization, 
                                  crop = category_bin, start_year = start_year, end_year = end_year)
              
              # Collapsing the data back into a dataframe since it is returned as a list
              raw_gwl_data = bind_rows(raw_gwl_data) }
            
            # Aggregating the mixed RCP bins to find the mean and quantiles 
            if(sector_basename != 'extraction_agriculture_rebased_all_crop' &
               ((gwl_bin %in% c('1_5_c', '2_c') & timeperiod == 'midc') |
                (gwl_bin == '3_c' & timeperiod == 'endc'))) {
              
              if(sector_basename != 'extraction_agriculture_rebased'){
                
                # Mixed RCP GWL Bins need to be formatted into a single file before aggregation
                # Combining the files for sectors that do not go through CO2 Fert
                raw_gwl_data = rbind(raw_gwl_data_a, raw_gwl_data_b)
                
              }
              
              raw_gwl_data = calc_quantiles(raw_gwl_data) %>% 
                pivot_longer(., mean:names(.)[ncol(.)], names_to = 'quantile', values_to = 'value')
            }
            
          } else if (sector_basename %in% c('extraction_coastal_rebased', 'extraction_CAMEL_rebased')){
            # Because coastal and CAMEL are already valued aggregates of sorts 
            # their extractions don't use an RCP flag so they got to skip all the crazy logic nonsense above
            if (gwl_bin %in% c('4_c', '5_c') & timeperiod == 'midc') {
              break
            } else{
              raw_gwl_data = read.csv(glue('{input}/{sector_basename}/{ssp}-{gwl_bin}_{timeperiod}-{scn_bin}-{category_bin}-{iam}-{spatial_file}-{unit}-edfcsv.csv'))
            }
          }
          
          # Coercing the scn & gwl lists to be correctly ordered so when the factor is set it recognizes the correct order 
          gwl_levels = intersect(c('1_5_c', '2_c', '3_c', '4_c', '5_c'), gwl_list)
          scn_levels = intersect(c('noadapt', 'incadapt', 'fulladapt', 'fulladaptcosts'), scn_list)
          
          raw_gwl_data = raw_gwl_data %>% 
            mutate(gwl_bin = factor(gwl_bin, levels = gwl_levels, ordered = TRUE),
                   scn = factor(scn_bin, levels = scn_levels, ordered = TRUE)) %>% 
            # Ag all crop pseudo-extractions, ag CO2 fertilization, and mixed RCP bins
            # are pivoted earlier, this command pivots the remaining sectors and GWL bins
            # Sorry this is such messy broken up logic
            {if ((!(sector_basename %in% c('extraction_agriculture_rebased', 'extraction_agriculture_rebased_all_crop')) &
                  ((gwl_bin %in% c('1_5_c', '2_c', '4_c', '5_c') & timeperiod == 'endc') | 
                   (gwl_bin == '3_c' & timeperiod == 'midc'))) | sector_basename %in% c('extraction_coastal_rebased', 'extraction_CAMEL_rebased'))
              pivot_longer(., mean:names(.)[ncol(.)-2], names_to = 'quantile', values_to = 'value') else .} %>%
            # adding a crop column to ag outputs only
            {if (category_bin %in% c('cassava', 'corn', 'rice', 'sorghum', 'soy', 'wheat_combined'))
              mutate(., crop = category_bin) else .} %>% 
            # In argiculture and labor the raw bad outcomes are negative instead of positive
            # So we flip the values for labor so that way bad outcomes are positive and match the other sectors 
            # The ag values need to be flipped inside each plot function because it must happend after any weighted means are taken
            {if (sector_basename %in% c('extraction_labor_rebased'))
              mutate(., value = value*-1) else .} %>%
            {if (length(iam_list) > 1) 
              mutate(., iam = iam) else .}
          
          return_data = rbind(return_data, raw_gwl_data)
        }  
      }
    }
  }
  
  
  
  # For mortality when the unit is rates
  # Turning the rate into per 100K people
  if(unit == 'rates'){
    return_data = return_data %>% 
      mutate(value = value*100000)
  }
  
  if(sector_basename == 'extraction_agriculture_rebased') {
    
    calories = get_calories()
    area = get_cropped_area(crop_in = category)
    
    return_data = return_data %>% 
      mutate(crop = ifelse(crop == 'wheat_combined', 'wheat', crop)) %>% 
      left_join(calories) %>% 
      left_join(area) %>% 
      mutate(cal_area = cals*area) %>% 
      select(-cals, -area)
    
  }
  
  return_data = return_data %>% 
    # We started to do this on a sector/plot level but we realized we should
    # filter all abnormal values out for everything
    filter(!is.na(value) & !(value %in% c('Inf','-Inf')))
  
  return(return_data)
}


# This function takes a data set in the form of a valuescsv and recalculates the mean and quantiles
# This function was developed to solve the issue that quantiles.py can not produce an edfcsv for a group of models
# where some of the models need to be evaluated at one rcp and another subset at a different rcp
# as was the case in the global warming level 1.5C mid century bin
# input values_data = data.frame in the format of the valuescsv output from quantiles.py
# output data.frame in the format of edfcsv from quantiles.py
# calculates the mean and the following quantiles: 95%, 90%, 75%, 66%, 50%, 33%, 25%, 10%, 5%
# These quantile correspond to IPCC likelihoods see inequality documentation for more info on them 
calc_quantiles = function(
    values_data
){
  #This calculation was slow (taking ~30s per run) so I needed to parallelize it
  #After parallelization it runs closer to 6s which is better but there is room for improvement 
  cluster <- new_cluster(1)
  cluster_library(cluster, "dplyr")
  
  edf_data = values_data %>% 
    dplyr::select(region, year, value) %>%
    group_by(region, year) %>% 
    partition(cluster) %>% 
    summarise(mean = mean(value),
              q95 = quantile(value, 0.95),
              q90 = quantile(value, 0.90),
              q75 = quantile(value, 0.75),
              q66 = quantile(value, 0.66),
              q50 = quantile(value, 0.50),
              q33 = quantile(value, 0.33),
              q25 = quantile(value, 0.25),
              q10 = quantile(value, 0.10),
              q5 = quantile(value, 0.05)) %>% 
    collect()
  
  return(edf_data)
}
