source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")




#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
#ADMINLEVEL1+2


for (n in 1:length(list.map.data.average)) {
for (i in 11:length(list.map.data.average[[n]])){
  
  var.labels <- df.labels.average[[2]][i]
  colname <- colnames(list.map.data.average[[n]][i])
  natural.breaks <- (classIntervals(list.df.average[[n]][,i-8], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute breaks and labels
  labels <- c()
  brks <- natural.breaks
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  
  # define a new variable on the data set with computed breaks
  list.map.data.average[[n]]$brks <- cut(list.map.data.average[[n]][,i], 
                        breaks = brks, 
                        include.lowest = TRUE,
                        label = labels)
  brks.scale <- levels(list.map.data.average[[n]]$brks)
  labels.scale <- rev(brks.scale)
  

  # creation of map
  p <- ggplot() +
    # administrative polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = list.map.data.average[[n]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha = 0.5)) +
    geom_polygon(data = list.map.data.average[[n]], aes(fill = brks, 
                                       x = long, 
                                       y = lat, 
                                       group = group)) +
    # administrative border
    geom_path(data = list.map.data.average[[n]], aes(x = long, 
                                    y = lat, 
                                    group = group), 
              color = "white", size = 0.1) +
    coord_equal() +
    # map text and styling
    theme_map() +
    labs(x = NULL, 
         y = NULL, 
         title = paste0("Average ",var.labels,"\nby Jordan governorate as place of asylum"),
         subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases: ",response_percentage,"%"),
         caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      # name = paste0("Average of ",colname),
      drop = FALSE,
      labels = labels.scale,
      guide = guide_legend(
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p
  
  path <- paste0("out/maps/JOR/average_numbers/adm",n,"/JOR_ADM",n,"_avg_",colname,".png")
  ggsave(path, p, width=10, height=10,units="in", dpi=300)
}
}







#### SINGLE CHOROPLETH MAP LOOP RATIO ###############################################################################
#ADMINLEVEL 1+2

for (n in 1:length(list.map.data.ratio)) {
for (i in 11:length(list.map.data.ratio[[n]])){

  var.labels <- df.labels.ratio[[2]][i]
  colname <- colnames(list.map.data.ratio[[n]][i])
  natural.breaks <- (classIntervals(list.df.ratio[[n]][,i-8], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data

  # computation of breaks and labels
  labels <- c()
  brks <- natural.breaks
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  
  # define a new variable on the data set with computed breaks
  list.map.data.ratio[[n]]$brks <- cut(list.map.data.ratio[[n]][,i],
                               breaks = brks,
                               include.lowest = TRUE,
                               label = labels)
  brks.scale <- levels(list.map.data.ratio[[n]]$brks)
  labels.scale <- rev(brks.scale)

  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
    # base layer to fill polygons where is no data (NA) in gray
    geom_polygon(data = list.map.data.average[[2]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha=0.4)) +
    geom_polygon(data = list.map.data.ratio[[n]], aes(fill = brks,
                                              x = long,
                                              y = lat,
                                              group = group)) +
    # municipality outline
    geom_path(data = list.map.data.ratio[[n]], aes(x = long,
                                           y = lat,
                                           group = group),
              color = "white", size = 0.1) +
    coord_equal() +
    theme_map() +
    labs(x = NULL,
         y = NULL,
         title = paste0("Percentage of ",var.labels,"\nby Jordan governorate as place of asylum"),
         subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases: ",response_percentage,"%"),
         caption = "Datasource: UNHCR p-code repository") +
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      drop = FALSE,
      labels = paste0(labels.scale, " %"),
      guide = guide_legend(
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p

  path <- paste0("out/maps/JOR/percentages/adm",n,"/JOR_ADM",n,"_ratio_",colname,".png")
  ggsave(path, p, width=10, height=10,units="in", dpi=300)
}
}






