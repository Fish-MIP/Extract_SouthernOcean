
# Camilla Novaglio, 30/04/2023
# This file contains all functions used to plot data for SO 

# Plot map of values ----
plot_SO<-function(wmap_df = antmap_df, 
                  data_to_plot = data,
                  proj = circumpolarCRS, 
                  legend_name = legend_name,
                  min = min, 
                  max = max){
  
  # # trial
  # wmap_df = antmap_df
  # data_to_plot = mean_phyto
  # proj = circumpolarCRS
  # legend_name = "g C m-2"
  # min = 0
  # max = 0.1
  
  # convert rasterLayer to sf object and consider circumpolar projections within function
  data_to_plot <- rasterToPolygons(data_to_plot) # convert Rasterlayer to spatial polygon dataframe 
  data_to_plot <- st_as_sf(data_to_plot) # convert dataframe to sf object 
  data_to_plot <- st_transform(data_to_plot, crs = st_crs(circumpolarCRS)) # convert sf object to circumpolar Projection
  
  # # bounding box instead of cropping. better solution but not working
  # disp_win_wgs84 <- st_sfc(st_point(c(-180, 180)), st_point(c(-90, -50)), crs = 4326)
  # target_crs<-pr
  # disp_win_trans <- st_transform(disp_win_wgs84, crs = target_crs)
  # disp_win_coord <- st_coordinates(disp_win_trans)
  # in collaboration with ggplot function:
  # coord_sf(xlim = disp_win_coord[,'X'], ylim = disp_win_coord[,'Y'], datum = target_crs, expand = FALSE)
  
  p1<-ggplot() +
    geom_polygon(data=antmap_df, aes(x=long, y=lat, group=group), fill="grey40", color=NA, linewidth = 0.25) +
    geom_sf(data=data_to_plot, colour = NA, aes(fill = index_1))+
    # scale_fill_viridis_d is for discrete, c is for continuous and b is for binned  
    scale_fill_viridis_b(guide = guide_colorbar(
      title = legend_name, 
      title.position = "top", 
      title.hjust = 0.5), 
      option = "D", 
      breaks = seq(min, max, length.out = 5),
      labels = format(round(seq(min, max, length.out = 5), digits = 2), nsmall = 2),
      limits=c(min, max), 
      oob = scales::squish, 
      na.value = "black" # not working
    ) +
    scale_x_continuous(expand=c(0,0))+ # this should get rid of the white spece around the map... 
    scale_y_continuous(expand=c(0,0))+
    my_theme_map
  
  p1
  
  return (p1 = p1)
  
}

