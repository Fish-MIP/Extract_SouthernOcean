
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
                  theme_map = my_theme_map){
  
  # # trial
  # wmap_df = antmap_df
  # data_to_plot = phyt_raster
  # proj = circumpolarCRS
  # legend_name = legend_name
  # min = phyt_raster@data@min 
  # max = phyt_raster@data@max
  # data_type = "modelled"
  # color_scale = color_scale
  # theme_map= "my_theme_map"


  # convert rasterLayer to sf object and consider circumpolar projections within function
  data_to_plot <- rasterToPolygons(data_to_plot) # convert Rasterlayer to spatial polygon dataframe 
  data_to_plot <- st_as_sf(data_to_plot) # convert dataframe to sf object 
  data_to_plot <- st_transform(data_to_plot, crs = st_crs(circumpolarCRS)) # convert sf object to circumpolar Projection
  
  # ## understanding Yang intervals 
  # c(0,0.5,1.25,2.50,3.75,5,6.25,8.75)
  # 0.5-0 = 0.5
  # 1.25-0.5 = 0.75
  # 2.50-1.25 = 1.25
  # 3.75-2.5 = 1.25
  # 5-3.75 = 1.25
  # 6.25-5 = 1.25
  # 8.75-6.25 = 2.5
  
  # trial<-seq(min, max, length.out = 5)
  # trial<-trial+1.5
  
  if(data_type == "zoo"){
    breaks_vector = c(0,0.5,1,2,3,5,10,25)
    labels_vector = c(0,0.5,1,2,3,5,10,25)
  }else if (data_type == "phyto"){
    breaks_vector = c(0,0.5,1.2,2.5,3.7,5.0,6.2,8.7)
    labels_vector = c(0,0.5,1.2,2.5,3.7,5.0,6.2,8.7)
  }else{
    breaks_vector = seq(min, max, length.out = 10)
    labels_vector = format(round(seq(min, max, length.out = 10), digits = 1), nsmall = 1)
    # ## trial understand intervals
    # breaks_vector[2] = breaks_vector[2]-1
    # breaks_vector[3] = breaks_vector[3]-0.5
    # breaks_vector[4] = breaks_vector[4]
    # labels_vector = round(seq(min, max, length.out = 5), digits = 2)
    # labels_vector[2] = labels_vector[2]-1
    # labels_vector[3] = labels_vector[3]-0.5
    # labels_vector[4] = labels_vector[4]
    # labels_vector = as.character(labels_vector)
    
  }
  
  if(color_scale == "viridis"){
    
    p1<-ggplot() +
      # geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey40", color=NA, linewidth = 0.25) +
      geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
      geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey70", color=NA, linewidth = 0.25) +
      
      # scale_fill_viridis_d is for discrete, c is for continuous and b is for binned
      scale_fill_viridis_b(guide = guide_colorbar(
        title = legend_name,
        title.position = "top",
        title.hjust = 0.5),
        option = "D", # this is viridis as option, incread of e.g. magma
        breaks = breaks_vector,
        labels = labels_vector,
        limits=c(min, max),
        oob = scales::squish,
      ) +
      
      scale_x_continuous(expand=c(0,0))+ # this should get rid of the white space around the map... 
      scale_y_continuous(expand=c(0,0))
  
    }else if(color_scale == "monochromatic"){
      
      p1<-ggplot() +
        # geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey40", color=NA, linewidth = 0.25) +
        geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
        geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey70", color=NA, linewidth = 0.25) +
    
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
          oob = scales::squish)+
    
        scale_x_continuous(expand=c(0,0))+ # this should get rid of the white space around the map... 
        scale_y_continuous(expand=c(0,0))
      
    }
  
  if(theme_map == "my_theme_map"){
    p1 = p1 + my_theme_map
  }else if (theme_map == "my_theme_map2"){
    p1 = p1 + my_theme_map2
  }
  
  return (p1 = p1)
  
}

