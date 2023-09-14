
# Camilla Novaglio, 16/02/2023
# This file contains all functions used to extract data from netCDF for the Southern Ocean. 

# Extract data from tcblog10 netCDF ----

extract_antarctica<-function(netcdf, 
                             file = "new",
                             history_year1,
                             history_year2,
                             future_year1,
                             future_year2){
  
  # # trial
  # a<-combinations %>% filter(identifier == "boats_ipsl_historical")
  # netcdf = a$netcdf_name
  # file = "new"
  # history_year1=1990
  # history_year2=1999
  # future_year1=2090
  # future_year2= 2099
  
  if(file.exists(file.path(dir, netcdf))){
  
  ########## extract info from netcdf name ----
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
  print(nc_data)
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F) # if verbose =t the process information is printed. def = F
  
  # check function below re time vector
  t_units<-ncatt_get(nc_data, "time", "units")$value

  # get time vector based on file name
  if(str_detect(netcdf, "1850", negate = FALSE)){
    stTime = "1850-1-1"
    enTime = "2014-12-31"
  }else if (str_detect(netcdf, "1950", negate = FALSE)){
    stTime = "1950-1-1"
    enTime = "2014-12-31"
  }else if (str_detect(netcdf, "2015", negate = FALSE)){
    stTime = "2015-1-1"
    enTime = "2100-12-31"
  }
  
  if(time_step == "monthly"){
    time_step_vector = "month"
  }else if(time_step == "annual"){
    time_step_vector = "year"
  }
  
  library(lubridate) # https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
  t1<- as.character(seq(ymd(stTime), ymd(enTime), by = time_step_vector))
  print(paste("Start time from file name ",t1[1], sep = ""))
  print(paste("End time from file name ",t1[length(t1)], sep = ""))
  
  # get time vector based on built in function 
  t <- as.character(nc.get.time.series(nc_data))
  print(paste("Start time with built in function ",t[1], sep = ""))
  print(paste("End time with built in function ",t[length(t)], sep = ""))
  
  if((t1[1] != t[1]) | (t1[length(t1)] != t[length(t)])){
    warning(paste(model, esm, scenario, "incorrect time vector", sep = " "), immediate. = TRUE)
    ## trust the vector from file name (this function does not work with inputs)
    t<-t1
  }
  
  # this is only to FIX zoom size bins names 
  if(model != "zoomss" & file == "new"){
    bins<-ncvar_get(nc_data, "bins")
  }else if (model != "zoomss" & file == "old"){ # this is only to check DBPM old files (not in DKRZ)
    bins<-ncvar_get(nc_data, "size")
  }else if (model == "zoomss"){
    bins<-c(1:6)}
  
  t_units<-ncatt_get(nc_data, "time", "units")$value
  b_units<-ncatt_get(nc_data, "tcblog10", "units")$value
  
  
  ### TO ADD - missing values 
  missingValues<-ncatt_get(nc_data, "tcblog10", "missing_value")$value
  
  nc_close(nc_data)
  
  # print warnings 
  
  stLon<-lon[1]
  enLon<-lon[length(lon)]
  stLat<-lat[1]
  enLat<-lat[length(lat)]
  stTime<-t[1] # leave this as a double check on the time vector
  enTime<-t[length(t)]
  
  if(stLon != -179.5){
    warning(paste(model, esm, scenario, "incorrect starting Lon", sep = " "), immediate. = TRUE)
  }
  if(enLon != 179.5){
    warning(paste(model, esm, scenario, "incorrect ending Lon", sep = " "), immediate. = TRUE)
  }
  ## dbpm and zoom IPSL incorrect ln fixed below 
  
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
  ## macroecological 5 bins and boats 2-4 bins fixed below 
  
  if(b_units != "g m-2"){
    warning(paste(model, esm, scenario, "incorrect biomass units", sep = " "), immediate. = TRUE)
  }
 
  # extract data as raster object: 
  brick_data<-list()
 
  for (i in 1:length(bins)){
    brick_data[[i]]<-brick(file.path(dir, netcdf), level = i) # level: integer > 0 (default=1). To select the 'level' (4th dimension variable) to use, if the file has 4 dimensions, e.g. to create a RasterBrick of weather over time at a certain height.
    print(dim(brick_data[[i]]))
  }
  
  # # CHECK
  # dim(brick_data[[1]])
  # plot(brick_data[[1]][[1004]])
  # plot(brick_data[[4]][[dim(brick_data[[2]])[3]]])
  
  ### Data is specified in file names amready... but not for the inputs.
  # BOATS example
  # class      : RasterBrick 
  # dimensions : 180, 360, 64800, 780  (nrow, ncol, ncell, nlayers)
  # resolution : 1, 1  (x, y)
  # extent     : -180, 180, -90, 90  (xmin, xmax, ymin, ymax)
  # crs        : +proj=longlat +datum=WGS84 +no_defs 
  # source     : boats_ipsl-cm6a-lr_nobasd_historical_nat_default_tcblog10_global_monthly_1950_2014.nc 
  # names      : X1950.01.01, X1950.02.01, X1950.03.01, X1950.04.01, X1950.05.01, X1950.06.01, X1950.07.01, X1950.08.01, X1950.09.01, X1950.10.01, X1950.11.01, X1950.12.01, X1951.01.01, X1951.02.01, X1951.03.01, ... 
  # Date       : 1950-01-01, 2014-12-01 (min, max)
  # varname    : tcblog10 
  # level      : 1 
  
  ## TO ADD 
  # check if land is specified as missingValues or as NA
  # options(scipen=999)
  trial<-brick_data[[1]][[1]] # take first layer and first year
  if(is.null(trial[trial == missingValues]) == FALSE){ # if there are missingValues cells 
    warning(paste(model, esm, scenario, "missing values not as NAs", sep = " "), immediate. = TRUE)
    for (i in 1:length(brick_data)){
      brick_data[[i]][brick_data[[i]] == missingValues]<-NA
    }
  }
  # # back check 
  # plot(trial)
  # trial[is.na(trial)] <- missingValues
  # plot(trial)
  
  ### TO FINISH... 
  # # check for negative values in random layer/years
  # trial[trial < 0]
  # 
  # lengthLoop<-dim(brick_data[[1]])[3]
  # 
  # vals_summary<-vector()
  # 
  # for(i in 1:length(bins)){
  #   # i = 1
  #   data<-brick_data[[i]]
  #   for(j in 1:lengthLoop){
  #     # j = 1
  #     vals<-getValues(data[[j]])
  #     vals_summary[[j]]<-sum(!is.na(vals[vals<0]))
  #   }
  # }
  # 
  # # lapply refers to layers - i.e. bins 
  # # stackApply referst to 
  # trial<-lapply(brick_data, FUN = function(x) stackApply(x, indices=t, fun=sum(!is.na(x[x<0]))))
  # trial<-lapply(brick_data, FUN = function(x) stackApply(x, indices=t, fun=sum))
  # 
  # trial<-brick_data[[1]]
  # trial<-stackApply(trial, indices=t, fun=sum)
  ### end to add 

  ########## calculate maps of annual means ----
  
  # remove 1850-1950 as not all models have them
  indices<-t
  
  if(scenario %in% c("historical","picontrol_hist")){
    indices_subset<-indices[indices>="1950-01-01"]
    indices_position<-match(indices_subset,indices)
    # check
    # indices[indices_position[1]]  
    brick_data_subset<-lapply(brick_data, FUN = function(x) raster::subset(x, indices_position))
    # check 
    # dim(brick_data[[1]])[3]-dim(brick_data_subset[[1]])[3] # 100 0r 1200 (100*12)
    # names(brick_data_subset[[1]])[1] # 1950
    # names(brick_data_subset[[1]])[length(names(brick_data_subset[[1]]))] # 2014
  }else if (scenario %in% c("ssp1","ssp5","picontrol_fut")){
    brick_data_subset<-brick_data
    } else if(scenario == "picontrol_whole"){
    # This is only for DBPM checking - extract the future part of picontrol in files that are not on DKRZ
    indices_subset<-indices[indices>="2015-01-01"]
    indices_position<-match(indices_subset,indices)
    brick_data_subset<-lapply(brick_data, FUN = function(x) raster::subset(x, indices_position))
  }
  
  # calculate annual means
  # https://gis.stackexchange.com/questions/257090/calculating-and-displaying-mean-annual-precipitation-from-cru-data
  
  if(scenario %in% c("historical","picontrol_hist", "picontrol_whole")){ 
    indices2<-as.Date(indices_subset)
  }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
    indices2<-as.Date(t)}
  
  indices2<-format(indices2, format = "%Y")
  indices2<-as.numeric(indices2)
  tic()
  brick_data_annual<-lapply(brick_data_subset, FUN = function(x) stackApply(x, indices=indices2, fun=mean))
  toc() # 4.4. min historical monthly, 12 sec annual
  
  # # CHECK 
  # dim(brick_data_annual[[1]])
  # plot(brick_data_annual[[1]][[15]])
  # dim(brick_data_subset[[1]])[3]/12 # 65 for e.g. DBPM which is == dim(brick_data_annual[[1]])[3]
  # names(brick_data_annual[[1]])[1] # 1950, 1951 etc... 
  # names(brick_data_annual[[1]])[length(names(brick_data_annual[[1]]))] # 2014
  
  # This is only for DBPM checking - extract the future part of picontrol 
  if(scenario == "picontrol_whole"){scenario = "picontrol_fut"}

  ########## calculate mean over period and save for maps ----
  
  # subset - choose a period for maps
  if(scenario %in% c("historical","picontrol_hist")){
    indices_subset3<-unique(indices2)
    indices_subset3<-indices_subset3[indices_subset3>=history_year1 & indices_subset3<=history_year2] 
  }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
    indices_subset3<-unique(indices2)
    indices_subset3<-indices_subset3[indices_subset3>=future_year1 & indices_subset3<=future_year2] # TO ADD CHANGES in >=
  }
  
  indices_position3<-match(indices_subset3,unique(indices2)) # unique(indices2) = new time dimension after annual mean
  brick_data_annual_subset<-lapply(brick_data_annual, FUN = function(x) raster::subset(x, indices_position3))
  
  # # CHECK
  # dim(brick_data_annual_subset[[1]])
  # plot(brick_data_annual_subset[[5]][[10]])
  
  indices4<-rep(1, length(indices_subset3))
  brick_data_annual_subset_mean<-lapply(brick_data_annual_subset, FUN = function(x) stackApply(x, indices=indices4, fun=mean))
  
  # # CHECK 
  # dim(brick_data_annual_subset_mean[[1]])
  # plot(brick_data_annual_subset_mean[[6]])
  
  # # CHECK - understand stackapply 
  # dim(brick_data_annual_subset[[1]]) # this is a list of raster stack - so you apply to each list (size) a mean function across layers (years)
  # # so this below does not make sense as you need to go through the list first ...
  # brick_data_annual_subset_mean<-stackApply(brick_data_annual_subset, FUN = function(x) mean(x, indices=indices4))
  
  # CHECK if all bins are full
  missing_bins <- lapply(brick_data_annual_subset_mean, FUN = function(x) minValue(x))
  bins<-which(!is.na(missing_bins)) 
  
  # warning on empty bins
  if(length(bins) < 6){
    warning(paste(model, esm, scenario, "some bins are empty", sep = " "), immediate. = TRUE)
  }
  
  # defining layers according to bins (boats has 6 layers with 2 being empty)
  # macroecological has 5 layers 
  if (model == "macroecological"){
    layers<-c(1:5)
  }else{
    layers<-c(1:6)
  }
  
  ########## fix extent and select SO ----
  # for Zoom and DBPM IPSL before extracting data and averaging them   
  
  if(stLon != -179.5){
    
    # ## CHECK 
    # brick_data_annual_subset_mean
    # plot(brick_data_annual_subset_mean[[1]])
    # extent(brick_data_annual_subset_mean[[1]])
    
    ## DBPM & ZoomSS
    # # dim of extracted variables from the netcdf
    # lat # 89.5, -89.5
    # length(lat) # 180
    # lon # -180, 179 
    # length(lon) # 360 # this includes 0 while lat does not because it goes by 0.5 deg. 
    # # dim of raster (added 0.5 deg)
    # brick_data_annual_subset_mean[[1]]
    # # -90, 90 # to be 180 this then should not include 0 but jump from 1 to -1?
    # # -180.5, 179.5
    # # how are the equator and Greenw. considered?
    # trial0<-as.data.frame(rasterToPoints(brick_data_annual_subset_mean[[1]]))
    # unique(trial0$x)# from -180 to 179 with 0
    # unique(trial0$y)# from 89.5 to -78.5 without 0 (Antarctica land removed)
    
    # New extent 
    bb <- extent(-180, 180, -90, 90) 
    # trial<-setExtent(brick_data_annual_subset_mean[[1]], bb, keepres=TRUE)  
    # plot(trial)
    # trial2<-setExtent(brick_data_annual_subset_mean[[1]], bb, keepres=FALSE) 
    # plot(trial2)
    # keepers: logical. If TRUE, the resolution of the cells will stay the same after adjusting the bounding box (by adjusting the number of rows and columns). 
    # If FALSE, the number of rows and columns will stay the same, and the resolution will be adjusted.
    # snap: logical. If TRUE, the extent is adjusted so that the cells of the input and output RasterLayer are aligned
    # for DBPM and ZoomSS, no difference between approaches
    
    for(i in 1:6){
      brick_data_annual_subset_mean[[i]]<-setExtent(brick_data_annual_subset_mean[[i]], bb, keepres=FALSE) 
    }
    
    # ## CHECK 
    # brick_data_annual_subset_mean
    # plot(brick_data_annual_subset_mean[[1]])
    
    # # how are the equator and Greenw. considered?
    # # DBPM and zoomSS:
    # trial3<-as.data.frame(rasterToPoints(brick_data_annual_subset_mean[[1]]))
    # unique(trial3$x)# from -179.5 to 179.5 without 0
    # unique(trial3$y)# from 89.5 to -78.5 withot 0 (Antarctica land removed)
    # trial4<-filter(trial3, x %in% c(0.5))
    # length(unique(trial4$y)) # Greenw. includes values, some missing lon/lat due to land
    # trial4<-filter(trial3, x %in% c(-0.5))
    # unique(trial4$y) # Greenw. includes values, some missing lon/lat due to land
    # trial4<-filter(trial3, y %in% c(0.5))
    # unique(trial4$x) # the equator includes values, some missing lon/lat due to land
    # trial4<-filter(trial3, y %in% c(-0.5))
    # length(unique(trial4$x)) # the equator includes values
  
  }
  
  # SELECT SO polygon:  
  
  # # # USING LME 
  # # library(sf)
  # # shape<-st_read("/rd/gem/private/users/camillan/Extract_SouthernOcean_Data/Input/LME66/LMEs66.shp")
  # # ant<-shape[47,] # SO should be LME61! 
  # 
  # # USING World_Seas_IHO_v3
  # library(sf)
  # shape<-st_read("/rd/gem/private/users/camillan/Extract_SouthernOcean_Data/Input/World_Seas_IHO_v3/World_Seas_IHO_v3.shp")
  # ant<-shape[63,] # Southern Ocean
  # 
  # # loop through sizes (could use lapply)
  # https://gis.stackexchange.com/questions/385850/mask-and-crop-a-raster-to-multiple-polygons-in-r
  # brick_data_annual_subset_mean_ant<-list()
  # 
  # for(i in layers){
  #   # OK here for BOATS - the bins become 4 after line above on bins but layers are 6 with 2 NULL ones.
  #   # macroecological is more problematic as it has 5 bins and layers... so not used here for now
  # 
  #   # i = 2
  #   trial<-brick_data_annual_subset_mean[[i]]
  #
  # # CRS: same projections 
  #   crs(trial) <- crs(shape)
  # # crop(): subset to the common polygon extent (a bounding box)   
  #   temp<-crop(trial, extent(ant))
  # # mask(): Create a new Raster* object that has the same values as x, except for the cells that are NA (or other maskvalue) in a 'mask'
  #   trial2<-mask(temp, ant) 
  # # NOTE - for EcoTroph, 
  # # here you remove some coastal outliers that are not inside the SO poligon.
  # # because you do not remove these values with the option below based on lat band
  # # the 2 plots from the 2 approaches look different but they are not, 
  # # once removed these outliers using the approach below, the plots are the same 
  # 
  #   brick_data_annual_subset_mean_ant[[i]]<-trial2
  #   
  # }
  # 
  # # dim(brick_data_annual_subset_mean_ant[[2]])
  # # plot(brick_data_annual_subset_mean_ant[[2]])
  # # extent(brick_data_annual_subset_mean_ant[[2]])
  
  # OR cut values of lat < e.g.-40 for comparison with Yang et al. 2022

  # step1<-lapply(brick_data_annual_subset_mean, function(x) as.data.frame(rasterToPoints(x)))
  # step2<-lapply(step1, function(x) filter(x, y <= -40, y >= -78))
  # 
  # # transform back in raster
  # brick_data_annual_subset_mean_ant<-lapply(step2[sapply(step2, function(x) dim(x)[1]) > 0], function(x) rasterFromXYZ(x, crs = crs(brick_data_annual_subset_mean[[1]])))
  # 
  # # add back dimension 1 and 6 in boats 
  # # otherwise you need to change the averaging code 02
  # if(model == "boats"){
  #   brick_data_annual_subset_mean_ant<-append(brick_data_annual_subset_mean_ant, 
  #                                             brick_data_annual_subset_mean[[1]],
  #                                             after = 0)
  #   brick_data_annual_subset_mean_ant<-append(brick_data_annual_subset_mean_ant, 
  #                                             brick_data_annual_subset_mean[[6]],
  #                                             after = 5)
  # }
  # 
  # 

  bb <- as(extent(-180, 180, -78, -40), 'SpatialPolygons')
  crs(bb) <- crs(brick_data_annual_subset_mean[[1]])
  brick_data_annual_subset_mean_ant<-lapply(brick_data_annual_subset_mean, function(x) crop(x,bb))
  
  # # CHECK
  # plot(brick_data_annual_subset_mean[[2]])
  # plot(brick_data_annual_subset_mean_ant[[2]])

  ########## unit conversion ---- 
  # from g m-2 to g C m-2 for better comparison with inputs   
  if(b_units == "g m-2"){
    brick_data_annual_subset_mean_ant = lapply(brick_data_annual_subset_mean_ant, FUN = function(x) x/10)
    b_units = "g C m-2"}
  
  # # CHECK 
  # plot(brick_data_annual_subset_mean_ant[[2]])
  
  rm(brick_data, brick_data_annual, brick_data_annual_subset, indices, indices2, indices_subset3, indices_position3, brick_data_annual_subset_mean, indices4) # remove objects that are not needed 
  
  return(brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant)
  
  rm(brick_data_annual_subset_mean_ant, trial)
  
  } # end of if(file.exist)
  
}

# Extract data from climate input netCDF ----

extract_antarctica_inputs<-function(netcdf, 
                                    history_year1,
                                    history_year2,
                                    future_year1,
                                    future_year2){
  
  # # trial
  # netcdf = netcdf_phyto[1]
  # history_year1 =1990
  # history_year2 =1999
  # future_year1 =NA
  # future_year2= NA
  
  if(file.exists(file.path(dir, netcdf))){
    
    ########## extract info from netcdf name ----
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
    
    # check function below re time vector
    t_units<-ncatt_get(nc_data, "time", "units")$value
    
    # get time vector based on file name
    if(str_detect(netcdf, "1850", negate = FALSE)){
      stTime = "1850-1-1"
      enTime = "2014-12-31"
    }else if (str_detect(netcdf, "1950", negate = FALSE)){
      stTime = "1950-1-1"
      enTime = "2014-12-31"
    }else if (str_detect(netcdf, "2015", negate = FALSE)){
      stTime = "2015-1-1"
      enTime = "2100-12-31"
    }
    
    if(time_step == "monthly"){
      time_step_vector = "month"
    }else if(time_step == "annual"){
      time_step_vector = "year"
    }
    
    library(lubridate) # https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
    t1<- as.character(seq(ymd(stTime), ymd(enTime), by = time_step_vector))
    print(paste("Start time from file name ",t1[1], sep = ""))
    print(paste("End time from file name ",t1[length(t1)], sep = ""))
    
    # get time vector based on built in function 
    t <- as.character(nc.get.time.series(nc_data))
    print(paste("Start time with built in function ",t[1], sep = ""))
    print(paste("End time with built in function ",t[length(t)], sep = ""))
    
    if((t1[1] != t[1]) | (t1[length(t1)] != t[length(t)])){
      warning(paste(model, esm, scenario, "incorrect time vector", sep = " "), immediate. = TRUE)
      ## trust the vector from file name (this function does not work with inputs)
      t<-t1
    }
    
    if(str_detect(netcdf, "phyc-vint", negate = FALSE)){
      variable = "phyc-vint"
    }else if (str_detect(netcdf, "zooc-vint", negate = FALSE)){
      variable = "zooc-vint"
    }
    
    b_units<-ncatt_get(nc_data, variable, "units")$value
    missingValues<-ncatt_get(nc_data, variable, "missing_value")$value
    
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
    
    # check if land is specified as missingValues or as NA
    # options(scipen=999)
    trial<-brick_data[[1]] # take first year
    if(is.null(trial[trial == missingValues]) == FALSE){ # if there are missingValues cells 
      warning(paste(model, scenario, "missing values not as NAs", sep = " "), immediate. = TRUE)
      brick_data[brick_data == missingValues]<-NA
    }
    # # back check 
    # plot(trial)
    # trial[is.na(trial)] <- missingValues
    # plot(trial)
  
    ########## calculate maps of annual means ----
    
    # remove 1850-1950 as not all models have them
    indices<-t
    
    if(scenario %in% c("historical", "picontrol_hist")){
      indices_subset<-indices[indices>="1950-01-01"]
      # check
      # indices[indices_position[1]]  
      indices_position<-match(indices_subset,indices)
      brick_data_subset<-raster::subset(brick_data, indices_position)
      # check 
      # dim(brick_data)[3]-dim(brick_data_subset)[3] # 100 0r 1200 (100*12)
    }else if (scenario %in% c("ssp1","ssp5","picontrol_fut")){
      brick_data_subset<-brick_data
    }
    
    # https://gis.stackexchange.com/questions/257090/calculating-and-displaying-mean-annual-precipitation-from-cru-data
    # create vector to serve as index
    
    if(scenario %in% c("historical", "picontrol_hist")){ 
      indices2<-as.Date(indices_subset)
    }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
      indices2<-as.Date(t)}
    
    indices2<-format(indices2, format = "%Y")
    indices2<-as.numeric(indices2)
    brick_data_annual<-stackApply(brick_data_subset, indices=indices2, fun=mean)
    
    # # CHECK
    # length(unique(indices2)) # 65
    # dim(brick_data_annual)[3] # 65
    # plot(brick_data_annual[[15]])
    # dim(brick_data_subset)[3]/12 # 65 
    # now names have become dates here too 
    # names(brick_data_annual)[1] # 1950, 1951 etc... 
    # names(brick_data_annual)[length(names(brick_data_annual))] # 2014
    
    ########## calculate mean over period and save for maps ----
    
    # subset - choose a period for maps
    if(scenario %in% c("historical", "picontrol_hist")){
      indices_subset3<-unique(indices2)
      indices_subset3<-indices_subset3[indices_subset3>=history_year1 & indices_subset3<=history_year2]
    }else if(scenario %in% c("ssp1", "ssp5", "picontrol_fut")){
      indices_subset3<-unique(indices2)
      indices_subset3<-indices_subset3[indices_subset3>=future_year1 & indices_subset3<=future_year2]
    }
    
    indices_position3<-match(indices_subset3,unique(indices2))
    brick_data_annual_subset<-raster::subset(brick_data_annual, indices_position3)
    
    # # CHECK
    # dim(brick_data_annual_subset) # 10
    # extent(brick_data_annual_subset)
    # plot(brick_data_annual_subset[[10]])
    # names(brick_data_annual_subset)[1] # 1990 for history
    # names(brick_data_annual_subset)[length(names(brick_data_annual_subset))] # 1999
    
    indices4<-rep(1, length(indices_subset3))
    brick_data_annual_subset_mean<-stackApply(brick_data_annual_subset, indices=indices4, fun=mean)
    
    # # CHECK
    # dim(brick_data_annual_subset_mean)
    # plot(brick_data_annual_subset_mean)
    
    ########## select SO ----
    # cut values of lat <e.g. -50 for comparison with Yang et al. 2022
    
    # step1<-as.data.frame(rasterToPoints(brick_data_annual_subset_mean))
    # step2<-filter(step1, y <= -40, y >= -78)
    # brick_data_annual_subset_mean_ant<-rasterFromXYZ(step2, crs = crs(brick_data_annual_subset_mean))
    
    bb <- as(extent(-180, 180, -78, -40), 'SpatialPolygons')
    crs(bb) <- crs(brick_data_annual_subset_mean)
    brick_data_annual_subset_mean_ant<-crop(brick_data_annual_subset_mean,bb)
    
    # CHECK 
    # plot(brick_data_annual_subset_mean)
    # plot(brick_data_annual_subset_mean_ant) # compare to plot above to check - OK

    ########## unit conversion ----
    # Units: from mol m-2 to g C m-2
    if(b_units == "mol m-2"){
      brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant*12
      b_units == "g C m-2"}
    
    rm(brick_data, brick_data_annual, brick_data_annual_subset, indices, indices2, indices_subset3, indices_position3, brick_data_annual_subset_mean, indices4) # remove objects that are not needed 
    
    return(brick_data_annual_subset_mean_ant = brick_data_annual_subset_mean_ant)
    
    rm(brick_data_annual_subset_mean_ant,trial)
    
  } # end of if(file.exist)
  
}
