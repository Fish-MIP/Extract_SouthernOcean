
# Camilla Novaglio, 30/04/2023
# This file contains all functions used to plot data for SO 

# Plot map of values ----
plot_SO<-function(wmap_df = antmap_df, 
                  data_to_plot = data,
                  proj = circumpolarCRS, 
                  legend_name = legend_name,
                  min = min, 
                  max = max,
                  data_type = "modelled", 
                  color_scale = color_scale, 
                  theme_map = my_theme_map, 
                  scale = "raw", 
                  breaks_vector_n = 10){
  
  # # trial
  # # wmap_df = antmap_df1
  # wmap_df = antmap_df
  # data_to_plot = phyt_raster
  # proj = circumpolarCRS
  # legend_name = legend_name
  # min = min_Fig0
  # max = max_Fig0_phyt #phyt_raster@data@max
  # data_type = "phyto"
  # color_scale = color_scale
  # theme_map= "my_theme_map"
  # scale = scale_Fig0
  # breaks_vector_n = breaks_vector_n
  
  # convert rasterLayer to sf object and consider circumpolar projections within function
  data_to_plot <- rasterToPolygons(data_to_plot) # convert Rasterlayer to spatial polygon dataframe 
  data_to_plot <- st_as_sf(data_to_plot) # convert dataframe to sf object 
  data_to_plot <- st_transform(data_to_plot, crs = st_crs(circumpolarCRS)) # convert sf object to circumpolar Projection
  
  
  if(scale == "log"){
    # head(data_to_plot)
    data_to_plot$index_1<-log(data_to_plot$index_1)
    # head(data_to_plot) - OK
    min = log(min) 
    max = log(max)
  }  
    
  if(data_type == "zoo"){
    breaks_vector = c(0,0.5,1,2,3,5,10,25)
    labels_vector = c(0,0.5,1,2,3,5,10,25)
  }else if (data_type == "phyto"){
    breaks_vector = c(0,0.5,1.2,2.5,3.7,5.0,6.2,8.7)
    labels_vector = c(0,0.5,1.2,2.5,3.7,5.0,6.2,8.7)
  }else{
    breaks_vector = seq(min, max, length.out = breaks_vector_n)
    labels_vector = format(round(seq(min, max, length.out = breaks_vector_n), digits = 1), nsmall = 1)
    }
  
  # define grid 
  x_lines <- seq(-120,180, by = 60) # longitude considered (lines) 
  y_lines <- seq(-40, -78, by = -10) # latitude (circles) 
  
  # convert in coordinares from dd to WGS84 Antarctic Polar Stereographic for geom_text
  # https://www.pgc.umn.edu/apps/convert/
  ### longitude (line) text
  # lat -40
  # long -120  -60    0   60  120  180
  Y_lon<-c(-2895951.938192, 2895951.938192, 5791903.876384, 2895951.938192, -2895951.938192, -5791903.876384)
  X_lon<-c(-5015935.893227, -5015935.893227, 0, 5015935.893227, 5015935.893227, 0)
  
  ### lat (circle) text
  # lat -40 -50 -60 -70
  # long 180
  Y_lat<-c(-5791903.876384,-4524537.706531,-3333134.02763,-2194494.247609)
  Y_lat<-c(-4524537.706531,-3333134.02763,-2194494.247609)
  X_lat<-180
  
  ### longitude (line) segment
  # lat -42
  # long -120  -60    0   60  120  180
  Y_lon_segment<-c(-2701064.981477,2701064.981477, 5402129.962954, 2701064.981477,-2701064.981477,-5402129.962954)
  X_lon_segment<-c(-4678381.782463,-4678381.782463, 0,4678381.782463, 4678381.782463,0)
  
  if(color_scale == "viridis"){
    
    ### alternative 
    # https://stackoverflow.com/questions/48816773/polar-stereographic-map-in-r
    # see below addings 
    
    p1<-ggplot() +
      geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
      geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey70", color=NA, linewidth = 0)+
      # geom_sf(data=wmap_df, fill = "grey70", colour = NA, linewidth = 0)+
      # scale_fill_viridis_d is for discrete, c is for continuous and b is for binned
      scale_fill_viridis_b(guide = guide_colorbar(
        title = legend_name,
        title.position = "top",
        title.hjust = 0.5),
        option = "D", # this is viridis as option, incread of e.g. magma
        breaks = breaks_vector,
        labels = labels_vector,
        limits=c(min, max),
        oob = scales::squish) 

    }else if(color_scale == "monochromatic"){

      p1<-ggplot() +
        geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
        geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey70", color=NA, linewidth = 0)+
        # geom_sf(data=wmap_df, fill = "grey70", colour = NA, linewidth = 0)+
        # https://colorbrewer2.org/#type=sequential&scheme=Blues&n=3
        scale_fill_gradient(guide = guide_colorbar(
         title = legend_name,
          title.position = "top",
          title.hjust = 0.5),
          low='#deebf7',
          high='#3182bd',
          breaks = breaks_vector,
          labels = labels_vector,
          limits=c(min, max),
          oob = scales::squish)
      
    } else if(color_scale == "divergent"){
      
      p1<-ggplot() +
        geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
        geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey70", color=NA, linewidth = 0)+
        # geom_sf(data=wmap_df, fill = "grey70", colour = NA, linewidth = 0)+
        scale_fill_continuous_divergingx(palette = 'Geyser', # ArmyRose
                                         mid = 1,
                                         guide = guide_colorbar(
                                           title = legend_name,
                                           title.position = "top",
                                           title.hjust = 0.5),
                                         breaks = breaks_vector,
                                         labels = labels_vector,
                                         limits=c(min, max),
                                         oob = scales::squish,
                                         na.value = "black") 
    }
  
  p1<-p1+
    
    # Adds labels
    # geom_text(aes(x = X_lat, y = Y_lat, hjust = -0.3, label = paste0(c("50", "60","70"), "°S")))+
    geom_text(aes(x = X_lon[1], y = Y_lon[1], hjust = 0.6, label = c("120°W")), size=1.9)+ 
    geom_text(aes(x = X_lon[2], y = Y_lon[2], hjust = 0.6, label = c("60°W")), size=1.9)+ 
    geom_text(aes(x = X_lon[3], y = Y_lon[3], label = c("0°")), size=1.9)+ 
    geom_text(aes(x = X_lon[4], y = Y_lon[4], label = c("60°E")), size=1.9)+ 
    geom_text(aes(x = X_lon[5], y = Y_lon[5], label = c("120°E")), size=1.9)+ 
    geom_text(aes(x = X_lon[6], y = Y_lon[6], label = c("180°")), size=1.9)+ 
    
    # Adds axes
    # geom_hline(aes(yintercept = -5791903.876384), size = 1, color = "red")  + # the -40
    geom_segment(aes(y = 0, yend =  Y_lon_segment, x = 0, xend = X_lon_segment), 
                 color = "grey50", linewidth = 0.25, linetype = 'dashed')
    # scale_x_continuous(expand=c(0,0))+ # this should get rid of the white space around the map...
    # scale_y_continuous(expand=c(0,0))
  
  if(theme_map == "my_theme_map"){
    p1 = p1 + my_theme_map
  }else if (theme_map == "my_theme_map2"){
    p1 = p1 + my_theme_map2
  }
  
  return (p1 = p1)
  
}

