#------------------------------------------------------------------------------------------
# Mortality Delta Beta Wrapper (adapted from https://gitlab.com/ClimateImpactLab/Impacts/gcp-labor/-/blob/master/3_projection/deltabetas/double_delta_beta.R)

# Author: Elliot Grenier (egrenier@uchicago.edu)
# Date Created: Jan 16 2025
# Last Modified: Jan 21 2025

# Description:
#
#   This script wraps the yellow purple package found here: https://gitlab.com/ClimateImpactLab/Impacts/post-projection-tools/-/blob/master/response_function/yellow_purple_package.R
#   to run delta betas for all regions to get deaths/100,000 in a given year as a result
#   of climate change, splitting IR-level mortality predictions by deaths in days warmer than 20C and 
#   deaths on days below 20C. Can also be used to plot delta-beta curves
#
#   To Run: depending on purpose, run through mortality_db_script.R or make_curve_hist_plot.R
#------------------------------------------------------------------------------------------

# 0. Load packages -----------------------
if(!require("pacman")){install.packages(("pacman"))}
pacman::p_load(ggplot2, 
               dplyr,
               readr)

# 1. Delta Beta Functions ----------------------

get.clipped.curve.mortality.mmt <- function(csvv, climtas, loggdppc, age, year, region, adapt='full', base_year=2010, TT_lower_bound=-23, TT_upper_bound=45, TT_step = 1, model="poly", minpoly_constraint=30, do.clipping=T, goodmoney.clipping=T, do.diffclip=T, get.covariates=F, covars=NULL, covar.names=NULL, list.names=NULL, CI='central', ...) {
  
  TT=seq(TT_lower_bound, TT_upper_bound, TT_step/2)
  years = c(base_year,year)
  
  if (get.covariates) {
    if (is.null(covars)) {
      stop("need to provide covariates dataframe -- try load.covariates(...)")
    } else {
      cov.list = get.covariates(covars=covars, region=region, years=years, covar.names=covar.names, list.names=list.names)
      climtas = cov.list$climtas
      loggdppc = cov.list$loggdppc
      
      if (length(climtas) != length(years) & length(loggdppc) != length(years)  ) {
        stop("covariates are incorrect size!")
      }
    }
  }
  yy0 <- get.curve.mortality(TT=TT, csvv=csvv, climtas=climtas[1], loggdppc=loggdppc[1], age=age) 
  
  ii.min <- which.min(yy0[TT >= 10 & TT <= minpoly_constraint]) + which(TT == 10) - 1
  MMT = TT[ii.min]
  message(paste0('Region: ',region,'; Age: ',age,'; MMT: ',TT[ii.min]))
  return(as.numeric(MMT))
}


standard_delta_beta = function(region, curve_ds, binclim, year, base_year, rebase_year, rnd.digits = 2, drop_zero_bins = T, rel.mmt = T, mmt=NULL, full_db = F, bin=NULL, het.list) {

  dims = dimnames(binclim)[[1]]
  FA_effect_y = curve_ds[dims,paste0(year),region]*binclim[dims,paste(year),] #  beta FA*clim 2099
  FA_effect_clim = curve_ds[dims,paste0(year),region]*binclim[dims,paste(rebase_year),] #  beta FA*clim 2005
  IA_effect_y = curve_ds[dims,paste0(year,'_IA'),region]*binclim[dims,paste(year),] # beta IA*clim 2099
  IA_effect_by = curve_ds[dims,paste0(year,'_IA'),region]*binclim[dims,paste(base_year),]  #  beta IA*clim 1993
  IA_effect_clim = curve_ds[dims,paste0(year,'_IA'),region]*binclim[dims,paste(rebase_year),] # beta IA*clim 2005
  NA_effect_y = curve_ds[dims,paste0(base_year,'_NA'),region]*binclim[dims,paste(year),] #  beta NA*clim 2099
  NA_effect_by = curve_ds[dims,paste0(base_year,'_NA'),region]*binclim[dims,paste(rebase_year),] #beta NA*clim 2005
  
  #rebase
  FA_effect_y = FA_effect_y - FA_effect_clim
  IA_effect_by = IA_effect_by - IA_effect_clim
  
  # Alternative formula for diagnostic purposes
  # term_1 = curve_ds[dims,paste0(year),region]*(binclim[dims,paste(year),] - binclim[dims,paste(base_year),])
  # term_2 = (curve_ds[dims,paste0(year),region] - curve_ds[dims,paste0(year,'_IA'),region])*(binclim[dims,paste(base_year),] - binclim[dims,paste(rebase_year),])
  
  if (is.null(bin)) {
    diff = mean(diff(as.numeric(dimnames(binclim)[[1]])))/2
    bin = paste0('(',as.numeric(dimnames(binclim)[[1]])-diff,',',as.numeric(dimnames(binclim)[[1]])+diff,']')		
  }
  deltabeta = data.frame( list(
    bin=bin,
    T_y = binclim[dims,paste(year),],
    T_by = binclim[dims,paste(base_year),],
    T_rb = binclim[dims,paste(rebase_year),],
    T_diff = binclim[dims,paste(year),] - binclim[dims,paste(base_year),],
    beta_fa = curve_ds[dims,paste0(year),region],
    beta_ia = curve_ds[dims,paste0(year,'_IA'),region],
    beta_na = curve_ds[dims,paste0(base_year,'_NA'),region],
    effect_fa = FA_effect_y - IA_effect_by,
    effect_ia = IA_effect_y - IA_effect_by,
    effect_na = NA_effect_y - NA_effect_by
    #effect_fa_alt = term_1 + term_2 # Use for diagnostics, should be exactly the same result as effect_fa
  ) , stringsAsFactors=F
  )
  
  # rounding
  deltabeta[,colnames(deltabeta)[grepl('T_',colnames(deltabeta))]] = round(deltabeta[,colnames(deltabeta)[grepl('T_',colnames(deltabeta))]], digits = 0 )
  deltabeta[,colnames(deltabeta)[grepl('beta_',colnames(deltabeta))]] = round(deltabeta[,colnames(deltabeta)[grepl('beta_',colnames(deltabeta))]], digits = rnd.digits )
  deltabeta[,colnames(deltabeta)[grepl('effect_',colnames(deltabeta))]] = round(deltabeta[,colnames(deltabeta)[grepl('effect_',colnames(deltabeta))]], digits = rnd.digits )
  
  # message(mmt)
  # message(class(mmt))
  under_mmt = round( apply(deltabeta[which(as.numeric(rownames(deltabeta)) < mmt),(ncol(deltabeta)-2):ncol(deltabeta)],2,sum), digits = 2)
  over_mmt = round( apply(deltabeta[which(as.numeric(rownames(deltabeta)) > mmt),(ncol(deltabeta)-2):ncol(deltabeta)],2,sum), digits = 2)
  total = round( apply(deltabeta[,(ncol(deltabeta)-2):ncol(deltabeta)],2,sum), digits = 2)
  
  if (drop_zero_bins == T) {
    deltabeta = dplyr::filter(deltabeta,(T_y > 0 | T_by > 0 | T_y < 0 | T_by < 0))
  }
  if (rel.mmt==T) {
    df = bind_rows(under_mmt, over_mmt, total) %>% 
      data.frame(bin=as.character(c('cold', 'hot', 'Total')), stringsAsFactors=F)
    
  } else {
    df = bind_rows( total) %>% 
      data.frame(bin=as.character(c('Total')), stringsAsFactors=F) 
  }

  # full db gives us mortality by 1 degree temp bin
  if (full_db == TRUE){
    
    db_table = bind_rows(deltabeta,df) %>%
      mutate_all(as.character) %>%
      mutate_all(~ if_else(is.na(.x),'',.x))  %>% 
      mutate(region = region)
    
  } else {
    
    db_table = df %>% mutate(region = region)
    
  }
  
  return(db_table)
}

get_db_table = function(csvv, region, clim, covars, age, args, ...) {
  
  for(i in 1:length(args)) {
    assign(x = names(args)[i], value = args[[i]])
  }
  
  message(region)
  
  all_years = sort(c(years, base_year, rebase_year))
  
  covar_base_year = ifelse(base_year<2016, 2016, base_year)
  covar_years = sort(c(years, covar_base_year))
  
  if(!is.null(covars)){
    #message('Subsetting covariates...')
    covars=get.covariates(covars=covars, region=region,years=covar_years, 
                          covar.names=covar.names, list.names=list.names) 
  }else{ covars=NULL }

  #message("Binning climate...")
  binclim = mapply_bin_clim(regions=region, years=all_years, clim=clim,
                            TT_lower_bound=TT_lower_bound, TT_upper_bound=TT_upper_bound, TT_step=TT_step)
  # Response functions
  #message('drawing response functions...')
  #message('---full adapt')
  curve_ds = mapply_curve(csvv=csvv, climtas=covars$climtas, loggdppc=covars$loggdppc,
                          age=age, year=years, regions=region, base_year=base_year, func=func, het.list=het.list,
                          TT_lower_bound=TT_lower_bound, TT_upper_bound=TT_upper_bound, TT_step=TT_step,
                          do.clipping=do.clipping, goodmoney.clipping=goodmoney.clipping, do.diffclip=do.diffclip)
  
  #message('---no adapt')
  curve_ds = mapply_curve(csvv=csvv, climtas=covars$climtas, loggdppc=covars$loggdppc,
                          age=age, year=years, regions=region, base_year=base_year, func=func, het.list=het.list,
                          TT_lower_bound=TT_lower_bound, TT_upper_bound=TT_upper_bound, TT_step=TT_step,
                          curve_ds=curve_ds, adapt='no', do.clipping=do.clipping,
                          goodmoney.clipping=goodmoney.clipping, do.diffclip=do.diffclip)
  
  #message('---income adapt')
  curve_ds = mapply_curve(csvv=csvv, climtas=covars$climtas, loggdppc=covars$loggdppc,
                          age=age, year=years, regions=region, base_year=base_year, func=func, het.list=het.list,
                          TT_lower_bound=TT_lower_bound, TT_upper_bound=TT_upper_bound, TT_step=TT_step,
                          curve_ds=curve_ds, adapt='income', do.clipping=do.clipping,
                          goodmoney.clipping=goodmoney.clipping, do.diffclip=do.diffclip)
  
  #message("got the curve")
  mmt = get.clipped.curve.mortality.mmt(csvv=csvv, climtas=covars$climtas, loggdppc=covars$loggdppc,
                                        age=age, year=years, region=region, base_year=base_year, het.list=het.list,
                                        TT_lower_bound=TT_lower_bound, TT_upper_bound=TT_upper_bound, TT_step=TT_step,
                                        curve_ds=curve_ds, do.clipping=do.clipping,
                                        goodmoney.clipping=goodmoney.clipping, do.diffclip=do.diffclip)
                     
  df = standard_delta_beta(region=region, curve_ds=curve_ds, year=years, binclim=binclim,
                           base_year=base_year, rebase_year=rebase_year, rnd.digits=5, het.list=het.list,
                           full_db=full_db, drop_zero_bins=F, rel.mmt=T, mmt=mmt, bin=NULL)
  
  if (delta.beta==T) {
    message('delta-beta...')
    years_sub = sort(c(years, base_year))
    bounds = mapply_bound_to_hist(regions=region, years=years_sub, binclim=binclim)
    yrs = names(bounds[[1]])
    bound2 <- list()
    bound2[[names(bounds)[1]]] <- list(
      setNames(list(bounds[[1]][[1]]), paste0(yrs[1], "_NA")),
      setNames(list(bounds[[1]][[2]]), yrs[2]),
      setNames(list(bounds[[1]][[2]]), paste0(yrs[2], "_IA"))
    )
    bound2[[names(bounds)[1]]] <- unlist(bound2[[names(bounds)[1]]], recursive = FALSE)
    
    curve_plots = mapply_plot_curve(regions=region, curve_ds=curve_ds, bounds=bound2, years=years, base_year=base_year, ...)
    hist_plots = mapply_plot_hist(regions=region, binclim=binclim, ...)
    yp = mapply_yellow_purple(regions=region, curve_plots=curve_plots, hist_plots=hist_plots, ...)

    if(save.plot){
      yp = yp[[1]]
      save_plot(plot = yp, 
                filename = glue('/project/cil/home_dirs/egrenier/cil-comms/adaptation_report/output/yellow_purple/yp_{region}_{age}_{rcp}_{gcm}_{base_year}_{rebase_year}{slug}.jpg'),
                base_height = 10,
                base_width = 10)
    }
  }
  
  return(df)
}



# Loop over a list of regions, and then bind them into one dataframe and save it as a csv on sac 
get_all_db_tables = function(region_list, age, args) {
  
  for(i in 1:length(args)) {
    assign(x = names(args)[i], value = args[[i]])
  }
  
  all_years = sort(c(years, base_year, rebase_year))
  
  # Was getting an error with global covars. This fixes without changing results (all covars pre-2015 should be the same)
  covar_base_year = ifelse(base_year<2016, 2016, base_year)  # This will load in 2015 covars
  covar_years = sort(c(years, covar_base_year))
  
  clim_df = mapply_extract_climate_data(years=all_years, tas_value=tas_value, 
                                        ncname = ncname, rcp=rcp, gcm=gcm)
  
  # message('loading csvv...')
  if (!is.null(csvv.name)){
    csvv = read.csvv(filepath = paste0(csvv.dir,csvv.name), het.list=het.list, age=age)
  } else { 
    message('warning: csvv empty')
    csvv = NULL 
  }
  
  # Load covariates and hold in memory
  if (get.covars == T) {
    message('loading covariates...')
    covars = load.covariates(regions=region_list, years=covar_years, 
                             cov.dir=cov.dir, covarkey=covarkey, covar.names=covar.names)
  } else { 
    message('warning: covars empty')
    covars=NULL 
  }
  
  # Returns list
  df = lapply(region_list, get_db_table, csvv=csvv, clim=clim_df, covars=covars, age=age, args) %>%
    rbindlist()
  
  return(df)
}

