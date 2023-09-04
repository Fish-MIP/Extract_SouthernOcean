
# Camilla Novaglio, 16/02/2023
# This file contains all functions used to extract data from netCDF for the Southern Ocean. 

# Extract data from tcblog10 netCDF ----

extract_antarctica<-function(netcdf, file = "new"){
  
  # # trial
  # a<-combinations %>% filter(identifier == "apecosm_ipsl_historical")
  # netcdf = a$netcdf_name
  # # netcdf = "zoomss_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"  
  # file = "new"
  
  # # list of files 
  # [1] "apecosm_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_monthly_1850_2014.nc"          
  # [2] "apecosm_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_monthly_1850_2014.nc"       
  # [3] "boats_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_monthly_1950_2014.nc"            
  # [4] "boats_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_monthly_1950_2014.nc"         
  # [5] "dbpm_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_monthly_1850_2014.nc"             
  # [6] "dbpm_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_monthly_1850_2014.nc"          
  # [7] "ecotroph_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"          
  # [8] "ecotroph_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"       
  # [9] "macroecological_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"   
  # [10] "macroecological_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"
  # [11] "zoomss_gfdl-esm4_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc"            
  # [12] "zoomss_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_annual_1950_2014.nc" 
  

  if(file.exists(file.path(dir, netcdf))){
  
  ########## extract info from netcdf name
  model = sub("\\_.*", "", netcdf)
  
  if(str_detect(netcdf, "gfdl", negate = FALSE)){
    esm = "gfdl-esm4"
  }else if (str_detect(netcdf, "ipsl", negate = FALSE)){
    esm = "ipsl-cm6a-lr"
  }
  
  # WARNING - add in EC? 
  if(str_detect(netcdf, "monthly", negate = FALSE)){
    time_step = "monthly"
  }else if (str_detect(netcdf, "annual", negate = FALSE)){
    time_step = "annual"
  }
  
  if(str_detect(netcdf, "historical", negate = FALSE)){
    scenario = "historical"
  }else if (str_detect(netcdf, "ssp126", negate = FALSE)){
    scenario = "ssp1"
  }else if (str_detect(netcdf, "ssp585", negate = FALSE)){
    scenario = "ssp5"
  }else if (str_detect(netcdf, "picontrol|2100", negate = FALSE)) {
    scenario = "picontrol_fut"
  } else if (str_detect(netcdf, "picontrol|2014", negate = FALSE)) {
    scenario = "picontrol_hist"}
  
  # extract info from netcdf description: 
  nc_data <- nc_open(file.path(dir, netcdf))
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F) # if verbose =t the process information is printed. def = F
  
  
  # ###### WARNING -
  # # problem with time vector in inputs - check outputs too 
  # t_units<-ncatt_get(nc_data, "time", "units")$value
  # t_problem<-ncvar_get(nc_data, "time")
  # t_problem[1]
  # t_extract_problem<-as.character(nc.get.time.series(nc_data))
  # t_extract_problem[1]
  # t_extract_problem[length(t_extract_problem)]
  # # here OK 
  
  t <- as.character(nc.get.time.series(nc_data))
  
  # this is only to FIX zoom size bins names 
  if(model != "zoomss" & file == "new"){
    bins<-ncvar_get(nc_data, "bins")
  }else if (model != "zoomss" & file == "old"){ # this is only to check DBPM old files (not in DKRZ)
    bins<-ncvar_get(nc_data, "size")
  }else if (model == "zoomss"){
    bins<-c(1:6)}
  
  t_units<-ncatt_get(nc_data, "time", "units")$value
  b_units<-ncatt_get(nc_data, "tcblog10", "units")$value
  
  nc_close(nc_data)
  
  # print warnings 
  
  stLon<-lon[1]
  enLon<-lon[length(lon)]
  stLat<-lat[1]
  enLat<-lat[length(lat)]
  stTime<-t[1]
  enTime<-t[length(t)]
  
  if(stLon != -179.5){
    warning(paste(model, esm, scenario, "incorrect starting Lon", sep = " "), immediate. = TRUE)
  }
  if(enLon != 179.5){
    warning(paste(model, esm, scenario, "incorrect ending Lon", sep = " "), immediate. = TRUE)
  }
  if(stLat != 89.5){
    warning(paste(model, esm, scenario, "incorrect starting Lat", sep = " "), immediate. = TRUE)
  }
  if(enLat != -89.5){
    warning(paste(model, esm, scenario, "incorrect ending Lat", sep = " "), immediate. = TRUE)
  }
  if(scenario == "historical" & !stTime %in% c("1950-01-01","1850-01-01")){ # some model include 100 years more 
    warning(paste(model, esm, scenario, "incorrect starting time", sep = " "), immediate. = TRUE)
  }
  if(scenario != "historical" & !stTime %in% c("2015-01-01")){
    warning(paste(model, esm, scenario, "incorrect starting time", sep = " "), immediate. = TRUE)
  }
  if(scenario == "historical" & !enTime %in% c("2014-12-01", "2014-01-01")){ # models can be monthly or annual 
    warning(paste(model, esm, scenario, "incorrect ending time", sep = " "), immediate. = TRUE)
  }
  if(scenario != "historical" & !enTime %in% c("2100-12-01", "2100-01-01")){ # models can be monthly or annual 
    warning(paste(model, esm, scenario, "incorrect ending time", sep = " "), immediate. = TRUE)
  }
  if(t_units != "days since 1601-1-1 00:00:00"){
    warning(paste(model, esm, scenario, "incorrect time units", sep = " "), immediate. = TRUE)
  }
  if(bins[1] != 1 & bins[6] != 6){
    warning(paste(model, esm, scenario, "incorrect bins names", sep = " "), immediate. = TRUE)
  }
  if(bins[length(bins)] != 6){
    warning(paste(model, esm, scenario, "incorrect bins dimension", sep = " "), immediate. = TRUE)
  }
  if(b_units != "g m-2"){
    warning(paste(model, esm, scenario, "incorrect biomass units", sep = " "), immediate. = TRUE)
  }
 
  # extract data as raster object: 
  brick_data<-list()
 
  for (i in 1:length(bins)){
    brick_data[[i]]<-brick(file.path(dir, netcdf), level = i)
    print(dim(brick_data[[i]]))
  }
  
  # # CHECK
  # dim(brick_data[[1]])
  # plot(brick_data[[1]][[1004]])
  # plot(brick_data[[4]][[dim(brick_data[[2]])[3]]])
  
  ##### WARNING - extent problem for dbpm and zoom ipsl: different than for other models 
  # in general, extent is different (shifted by 0.5 deg) from what is specified in lat and lon
  # extent(brick_data[[1]])

  ########## 1 - calculate maps of annual means 
  
  # remove 1850-1950 as not all models have them
  indices<-t
  
  if(scenario %in% c("historical","picontrol_hist")){
    indices_subset<-indices[indices>="1950-01-01"]
    indices_position<-match(indices_subset,indices)
    brick_data_subset<-lapply(brick_data, FUN = function(x) raster::subset(x, indices_position))
  }else if (scenario %in% c("ssp1","ssp5","picontrol_fut")){
    brick_data_subset<-brick_data
    } else if(scenario == "picontrol_whole"){
    # This is only for DBPM checking - extract the future part of picontrol in files that are not on DKRZ
    indices_subset<-indices[indices>="2015-01-01"]
    indices_position<-match(indices_subset,indices)
    brick_data_subset<-lapply(brick_data, FUN = function(x) raster::subset(x, indices_position))
  }
  
  # calculate annual means
  # WARNING - try median instead 
  
  
  
  # https://gis.stackexchange.com/questions/257090/calculating-and-displaying-mean-annual-precipitation-from-cru-data
  # create vector to serve as index
  
  if(scenario %in% c("historical","picontrol_hist", "picontrol_whole")){ 
    indices2<-as.Date(indices_subset)
  }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
    indices2<-as.Date(t)}
  
  indices2<-format(indices2, format = "%Y")
  indices2<-as.numeric(indices2)
  tic()
  brick_data_annual<-lapply(brick_data_subset, FUN = function(x) stackApply(x, indices=indices2, fun=mean))
  toc() # 4.4. min historical 
  
  # rm(mean) # when error 
  
  # # CHECK 
  # dim(brick_data_annual[[1]])
  # plot(brick_data_annual[[1]][[15]])
  
  # THis is only for DBPM checking - extract the future part of picontrol 
  if(scenario == "picontrol_whole"){scenario = "picontrol_fut"}

  ########## 2 - calculate mean tcb over period and save for maps
  
  # subset - choose a period for maps
  if(scenario %in% c("historical","picontrol_hist")){
    indices_subset3<-unique(indices2)
    indices_subset3<-indices_subset3[indices_subset3>=1990 & indices_subset3<=1999] # Time period asked by Philip Boyd
  }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
    indices_subset3<-unique(indices2)
    indices_subset3<-indices_subset3[indices_subset3>2090 & indices_subset3<=2099] # Future not considered here 
  }
  
  indices_position3<-match(indices_subset3,unique(indices2))
  brick_data_annual_subset<-lapply(brick_data_annual, FUN = function(x) raster::subset(x, indices_position3))
  # # CHECK
  # dim(brick_data_annual_subset[[1]])
  # extent(brick_data_annual_subset[[1]])
  # plot(brick_data_annual_subset[[5]][[10]])
  
  # mean
  # WARNING - try median instead 
  
  
  
  indices4<-rep(1, length(indices_subset3))
  brick_data_annual_subset_mean<-lapply(brick_data_annual_subset, FUN = function(x) stackApply(x, indices=indices4, fun=mean))
  # # CHECK 
  # dim(brick_data_annual_subset_mean[[1]])
  # plot(brick_data_annual_subset_mean[[6]])
  
  ###### Just trial to understand stackapply 
  # dim(brick_data_annual_subset[[1]]) # this is a list of raster stack - so you apply to each list (size) a mean function across layers (years)
  # # so this below does not make sense....
  # brick_data_annual_subset_mean<-stackApply(brick_data_annual_subset, FUN = function(x) mean(x, indices=indices4))
  
  # CHECK if all bins are full
  missing_bins <- lapply(brick_data_annual_subset_mean, FUN = function(x) minValue(x))
  bins<-which(!is.na(missing_bins)) 
  
  # warning on empty bins
  if(length(bins) < 6){
    warning(paste(model, esm, scenario, "some bins are empty", sep = " "), immediate. = TRUE)
  }
  
  # defining layers according to bins (boats has 6 layers with 2 being empty)
  if (model == "macroecological"){
    layers<-c(1:5)
  }else{
    layers<-c(1:6)
  }
  
  # WARNING - dealing with different extent for Zoom and DBPM IPSL before extracting data  
  if(stLon != -179.5){
    
    # ## CHECK 
    # brick_data_annual_subset_mean
    # plot(brick_data_annual_subset_mean[[1]])
    
    # New extent 
    bb <- extent(-180, 180, -90, 90) 
    # trial<-setExtent(brick_data_annual_subset_mean[[1]], bb, keepres=FALSE) 
    # WARNING keepres=FALSE changes the resolution (but not in Zoom??), the alternative = TRUE changes 
    # the number of columns 
    # OK for now but needs more checking  
    
    for(i in 1:6){
      brick_data_annual_subset_mean[[i]]<-setExtent(brick_data_annual_subset_mean[[i]], bb, keepres=FALSE) 
    }
    
    # ## CHECK 
    # brick_data_annual_subset_mean
    # plot(brick_data_annual_subset_mean[[1]])
  }
  
  # SELECT SO polygon:  
  
  # # # USING LME 
  # # library(sf)
  # # shape<-st_read("/home/ubuntu/extract_SouthernOcean/Input/LME66/LMEs66.shp")
  # # ant<-shape[47,] # WARNING - shouldn't SO be 61? 
  # 
  # # USING World_Seas_IHO_v3
  # library(sf)
  # shape<-st_read("/rd/gem/private/users/camillan/Extract_SouthernOcean_Data/Input/World_Seas_IHO_v3/World_Seas_IHO_v3.shp")
  # ant<-shape[63,] # Southern Ocean
  # 
  # # loop through sizes (could use stackapply)
  # brick_data_annual_subset_mean_ant<-list()
  # 
  # for(i in layers){
  #   # OK here for BOATS - the bins become 4 after line above on bins but layers are 6 with 2 NULL ones.
  #   # macroecological is more problematic as it has 5 bins and layers... so not used here for now
  # 
  #   # i = 2
  #   trial<-brick_data_annual_subset_mean[[i]]
  # 
  #   crs(trial) <- crs(shape)
  #   temp<-crop(trial, extent(ant))
  #   trial2<-mask(temp, ant) # NOTE - for EcoTroph, 
  #   # here you remove some coastal outliers that are not inside the ant poligon.
  #   # because you do not remove these values with the option below based on lat band
  #   # the 2 plots from the 2 appraoch look very different but they are not, 
  #   # once removed these outliers using the appraoch below, the plots are the same 
  # 
  #   brick_data_annual_subset_mean_ant[[i]]<-trial2
  #   
  # }
  # 
  # # dim(brick_data_annual_subset_mean_ant[[2]])
  # # plot(brick_data_annual_subset_mean_ant[[2]])
  # # extent(brick_data_annual_subset_mean_ant[[2]])
  
  #### OR cut values of lat < e.g.-40 for comparison with Yang et al. 2022

  brick_data_annual_subset_mean_ant<-list()

  # this version of the code does not work with BOATS size classes 1 and 6 (empty)
  # so need to define a different layer for BOATS

  if (model == "boats"){
    layers<-c(2:5)
  }else{
    layers<-layers
  }

  for(i in layers){

    # i = 2
      trial<-brick_data_annual_subset_mean[[i]] # here is already 
      trial<-as.data.frame(rasterToPoints(trial))

      #### WARNING
      # the below results in different final extent between IPSL and GFDL
      # trial<-filter(trial, y <= -35)
      # class      : Extent
      # xmin       : -180
      # xmax       : 180
      # ymin       : -79
      # ymax       : -35
      #
      # class      : Extent
      # xmin       : -180
      # xmax       : 180
      # ymin       : -78
      # ymax       : -35
      # pick the minimum common denominator and cut lower lat.
      # This does not happen when using polygons (above) as the lower part of the polygon of is land

      trial<-filter(trial, y <= -40, y >= -78)
      
      # ## for ecotroph checking: 
      # trial<-filter(trial, y <= -60, y >= -78)
      # # If outliers are removed (see above for explanation) 
      # # plots from the two approaches are the same. 
      # # no need to delete them, just to se a meaningful range of values to consider in maps
      # mistmatches<-filter(trial, index_1 > 49.15919 | index_1 < 8.972864)
      # trial<-filter(trial, index_1 >= 8.972864 & index_1 <= 49.15919)
      
      brick_data_annual_subset_mean_ant[[i]]<-rasterFromXYZ(trial, crs = crs(brick_data_annual_subset_mean[[i]]))
  }

  # # same output as above when extracting using polygon
  # plot(brick_data_annual_subset_mean_ant[[2]])

  # unit conversion: from g m-2 to g C m-2 for better comparison with inputs   
  if(b_units == "g m-2"){
    brick_data_annual_subset_mean_ant = lapply(brick_data_annual_subset_mean_ant, FUN = function(x) x/10)
    b_units = "g C m-2"}
  
  rm(brick_data, brick_data_annual, brick_data_annual_subset, indices, indices2, indices_subset3, indices_position3, brick_data_annual_subset_mean, indices4) # remove objects that are not needed 
  
  return(brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant)
  
  rm(brick_data_annual_subset_mean_ant, trial)
  
  } # end of if(file.exist)
  
}

# Extract data from climate input netCDF ----

extract_antarctica_inputs<-function(netcdf){
  
  # trial
  # netcdf = "gfdl-esm4_r1i1p1f1_historical_phyc-vint_60arcmin_global_monthly_1850_2014.nc"

  if(file.exists(file.path(dir, netcdf))){
    
    ########## extract info from netcdf name
    model = sub("\\_.*", "", netcdf)
    
    if(str_detect(netcdf, "gfdl", negate = FALSE)){
      esm = "gfdl-esm4"
    }else if (str_detect(netcdf, "ipsl", negate = FALSE)){
      esm = "ipsl-cm6a-lr"
    }
    
    if(str_detect(netcdf, "monthly", negate = FALSE)){
      time_step = "monthly"
    }else if (str_detect(netcdf, "annual", negate = FALSE)){
      time_step = "annual"
    }
    
    if(str_detect(netcdf, "historical", negate = FALSE)){
      scenario = "historical"
    }else if (str_detect(netcdf, "ssp126", negate = FALSE)){
      scenario = "ssp1"
    }else if (str_detect(netcdf, "ssp585", negate = FALSE)){
      scenario = "ssp5"
    }else if (str_detect(netcdf, "picontrol|2100", negate = FALSE)) {
      scenario = "picontrol_fut"
    } else if (str_detect(netcdf, "picontrol|2014", negate = FALSE)) {
      scenario = "picontrol_hist"}
    
    # extract info from netcdf description: 
    nc_data <- nc_open(file.path(dir, netcdf))
    
    lon <- ncvar_get(nc_data, "lon")
    lat <- ncvar_get(nc_data, "lat", verbose = F)
    
    # ###### WARNING problem with time vector - can this be due to calendar 365_day?? 
    # t_units<-ncatt_get(nc_data, "time", "units")$value
    # t_problem<-ncvar_get(nc_data, "time")
    # t_problem[1]
    # length(t_problem)[1]
    # t_extract_problem<-as.character(nc.get.time.series(nc_data))
    # t_extract_problem[1]
    # t_extract_problem[length(t_extract_problem)]
    # # same as (but not with MEMs): 
    # trial<-as.character(nc.get.time.series(nc_data, correct.for.gregorian.julian = TRUE))
    # trial[1]
    # trial[length(trial)]
    
    # overwrite t: 
    library(lubridate) # https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
    t<- as.character(seq(ymd("1850-1-1"), ymd("2014-12-31"), by = "month"))
    
    # # CHECK
    # length(t) #1980
    # length(t_extract_problem) # 1980
    # length(t_problem) # 1980
    # t[1]
    # t[length(t)]
    
    if(str_detect(netcdf, "phyc", negate = FALSE)){
      variable = "phyc-vint"
    }else if (str_detect(netcdf, "zooc", negate = FALSE)){
      variable = "zooc-vint"
    }
    
    b_units<-ncatt_get(nc_data, variable, "units")$value
    
    nc_close(nc_data)
    
    # print warnings 
    
    stLon<-lon[1]
    enLon<-lon[length(lon)]
    stLat<-lat[1]
    enLat<-lat[length(lat)]
    stTime<-t[1]
    enTime<-t[length(t)]
    
    if(stLon != -179.5){
      warning(paste(model, esm, scenario, "incorrect starting Lon", sep = " "), immediate. = TRUE)
    }
    if(enLon != 179.5){
      warning(paste(model, esm, scenario, "incorrect ending Lon", sep = " "), immediate. = TRUE)
    }
    if(stLat != 89.5){
      warning(paste(model, esm, scenario, "incorrect starting Lat", sep = " "), immediate. = TRUE)
    }
    if(enLat != -89.5){
      warning(paste(model, esm, scenario, "incorrect ending Lat", sep = " "), immediate. = TRUE)
    }
    if(scenario == "historical" & !stTime %in% c("1950-01-01","1850-01-01")){ # some model include 100 years more 
      warning(paste(model, esm, scenario, "incorrect starting time", sep = " "), immediate. = TRUE)
    }
    if(scenario != "historical" & !stTime %in% c("2015-01-01")){
      warning(paste(model, esm, scenario, "incorrect starting time", sep = " "), immediate. = TRUE)
    }
    if(scenario == "historical" & !enTime %in% c("2014-12-01", "2014-01-01")){ # models can be monthly or annual 
      warning(paste(model, esm, scenario, "incorrect ending time", sep = " "), immediate. = TRUE)
    }
    if(scenario != "historical" & !enTime %in% c("2100-12-01", "2100-01-01")){ # models can be monthly or annual 
      warning(paste(model, esm, scenario, "incorrect ending time", sep = " "), immediate. = TRUE)
    }
    if(t_units != "months since 1601-1-1 00:00:00"){
      warning(paste(model, esm, scenario, "incorrect time units", sep = " "), immediate. = TRUE)
    }
    
    # extract data as raster object: 
    brick_data<-brick(file.path(dir, netcdf))
    
    # # CHECK
    # dim(brick_data)
    # plot(brick_data[[1980]])
    # extent(brick_data)
    
    ########## 1 - calculate maps of annual means 
    
    # remove 1850-1950 as not all models have them
    indices<-t
    
    if(scenario %in% c("historical")){
      indices_subset<-indices[indices>="1950-01-01"]
      indices_position<-match(indices_subset,indices)
      brick_data_subset<-raster::subset(brick_data, indices_position)
    }
    
    # calculate annual means
    # WARNING - try median instead 
    
    
    
    # https://gis.stackexchange.com/questions/257090/calculating-and-displaying-mean-annual-precipitation-from-cru-data
    # create vector to serve as index
    
    if(scenario %in% c("historical")){ 
      indices2<-as.Date(indices_subset)
    }
    
    indices2<-format(indices2, format = "%Y")
    indices2<-as.numeric(indices2)
    brick_data_annual<-stackApply(brick_data_subset, indices=indices2, fun=mean)
    
    # # CHECK
    # length(unique(indices2))
    # dim(brick_data_annual)
    # plot(brick_data_annual[[15]])
    
    ########## 2 - calculate mean over period and save for maps
    
    # subset - choose a period for maps
    if(scenario %in% c("historical")){
      indices_subset3<-unique(indices2)
      indices_subset3<-indices_subset3[indices_subset3>=1990 & indices_subset3<=1999] # WARNING this is what Phil asked for 
    }
    
    indices_position3<-match(indices_subset3,unique(indices2))
    brick_data_annual_subset<-raster::subset(brick_data_annual, indices_position3)
    
    # # CHECK
    # dim(brick_data_annual_subset)
    # extent(brick_data_annual_subset)
    # plot(brick_data_annual_subset[[10]])
    
    # mean
    # WARNING - try median instead - see plots 
    
    
    
    
    indices4<-rep(1, length(indices_subset3))
    brick_data_annual_subset_mean<-stackApply(brick_data_annual_subset, indices=indices4, fun=mean)
    
    # # CHECK
    # dim(brick_data_annual_subset_mean)
    # plot(brick_data_annual_subset_mean)
    
    # SELECT SO polygon:  
    
    # # USING World_Seas_IHO_v3
    # library(sf)
    # shape<-st_read("/rd/gem/private/users/camillan/Extract_SouthernOcean_Data/Input/World_Seas_IHO_v3/World_Seas_IHO_v3.shp")
    # ant<-shape[63,] # Southern Ocean  
    # brick_data_annual_subset_mean_ant<-list()
    # 
    # trial<-brick_data_annual_subset_mean
    #   
    # # this is wrong - something happens here ... 
    # # extent(trial) <- extent(shape)
    # # this does not do much .... 
    # crs(trial) <- crs(shape) 
    #   
    # # this seems to work but 
    # temp<-crop(trial, extent(ant))
    # trial2<-mask(temp, ant)
    # # plot(trial2)
    #   
    # brick_data_annual_subset_mean_ant<-trial2
    # 
    # # # CHECK
    # # dim(brick_data_annual_subset_mean_ant)
    # # plot(brick_data_annual_subset_mean_ant)
    # # extent(brick_data_annual_subset_mean_ant)
    # 
    
    #### OR cut values of lat <e.g. -50 for comparison with Yang et al. 2022
    
    trial<-brick_data_annual_subset_mean
    trial<-as.data.frame(rasterToPoints(trial))
    # trial<-filter(trial, y < -35)
    # see above for reason on selecting -78 too. 
    trial<-filter(trial, y <= -40, y >= -78)
      
    brick_data_annual_subset_mean_ant<-rasterFromXYZ(trial, crs = crs(brick_data_annual_subset_mean))
    # plot(brick_data_annual_subset_mean_ant)

    # Units: from mol m-2 to g C m-2
    if(b_units == "mol m-2"){
      brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant*12.01
      b_units == "g C m-2"}
    
    rm(brick_data, brick_data_annual, brick_data_annual_subset, indices, indices2, indices_subset3, indices_position3, brick_data_annual_subset_mean, indices4) # remove objects that are not needed 
    
    return(brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant)
    
    rm(brick_data_annual_subset_mean_ant,trial)
    
  } # end of if(file.exist)
  
}