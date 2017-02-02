source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")






#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
#ADMINLEVEL1

# plot.list = list()
for (i in 21:length(list.map.data.average[[1]])){
  
  colname <- colnames(list.map.data.average[[1]][i])
  natural.breaks <- (classIntervals(list.df.average[[1]][,i-18], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute labels
  labels <- c()
  # brks <- as.numeric(unlist(natural_breaks))
  brks <- natural.breaks
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  list.map.data.average[[1]]$brks <- cut(list.map.data.average[[1]][,i], 
                        breaks = brks, 
                        include.lowest = TRUE,
                        label = labels)
  brks.scale <- levels(list.map.data.average[[1]]$brks)
  labels.scale <- rev(brks.scale)
  

  
  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = list.map.data.average[[1]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha = 0.5)) +
    geom_polygon(data = list.map.data.average[[1]], aes(fill = brks, 
                                       x = long, 
                                       y = lat, 
                                       group = group)) +
    # municipality outline
    geom_path(data = list.map.data.average[[1]], aes(x = long, 
                                    y = lat, 
                                    group = group), 
              color = "white", size = 0.1) +
    coord_equal() +
    theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   # margin = margin(b = -0.1,  # margin creates error
                                   #                 t = -0.1, 
                                   #                 l = 2, 
                                   #                 unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.3,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.5,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  # margin = margin(t = 0.2, 
                                  #                 b = 0, 
                                  #                 unit = "cm"), 
                                  color = "#939184")
    ) +
    labs(x = NULL, 
         y = NULL, 
         title = "Jordan as country of Asylum", 
         subtitle = paste0("Average of ",colname," of registered refugees by Jordan Governorate \nPercentage of mapped cases:",response_percentage,"%"), 
         caption = "Datasource: UNHCR p-code repository") + 
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      name = paste0("Average of ",colname),
      drop = FALSE,
      labels = labels.scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p
  
  path <- paste0("out/maps/JOR/average_numbers/adm1/JOR_ADM",list.admlevel[[1]],"_avg_",colname,".png")
  ggsave(path, p, width=5, height=5,units="in", dpi=300)
}





#ADMINLEVEL1 ##

# plot.list = list()
for (i in 33:length(list.map.data.average[[2]])){
  
  colname <- colnames(list.map.data.average[[2]][i])
  natural.breaks <- (classIntervals(list.df.average[[2]][,i-30], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute labels
  labels <- c()
  # brks <- as.numeric(unlist(natural_breaks))
  brks <- natural.breaks
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  list.map.data.average[[2]]$brks <- cut(list.map.data.average[[2]][,i], 
                                         breaks = brks, 
                                         include.lowest = TRUE,
                                         label = labels)
  brks.scale <- levels(list.map.data.average[[2]]$brks)
  labels.scale <- rev(brks.scale)
  
  
  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    # base layer to fill polygons where is no data (NA) in gray
    geom_polygon(data = list.map.data.average[[2]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha=0.4)) +
    geom_polygon(data = list.map.data.average[[2]], aes(fill = brks, 
                                                        x = long, 
                                                        y = lat, 
                                                        group = group)) +
    # municipality outline
    geom_path(data = list.map.data.average[[2]], aes(x = long, 
                                                     y = lat, 
                                                     group = group), 
              color = "white", size = 0.1) +
    coord_equal() +
    theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", 
                                   # margin = margin(b = -0.1,  # margin creates error
                                   #                 t = -0.1, 
                                   #                 l = 2, 
                                   #                 unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.3,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.5,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  # margin = margin(t = 0.2, 
                                  #                 b = 0, 
                                  #                 unit = "cm"), 
                                  color = "#939184")
    ) +
    labs(x = NULL, 
         y = NULL, 
         title = "Jordan as country of Asylum", 
         subtitle = paste0("Average of ",colname," of registered refugees by Jordan Governorate \nPercentage of mapped cases:",response_percentage,"%"), 
         caption = "Datasource: UNHCR p-code repository") + 
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      name = paste0("Average of ",colname),
      drop = FALSE,
      labels = labels.scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p
  
  path <- paste0("out/maps/JOR/average_numbers/adm2/JOR_ADM",list.admlevel[[2]],"_avg_",colname,".png")
  ggsave(path, p, width=5, height=5,units="in", dpi=300)
}








#### SINGLE CHOROPLETH MAP LOOP RATIO ###############################################################################
#ADMINLEVEL 1


for (i in 21:length(list.map.data.ratio[[1]])){

  colname <- colnames(list.map.data.ratio[[1]][i])
  natural.breaks <- (classIntervals(list.df.ratio[[1]][,i-18], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data

  # compute labels
  labels <- c()
  # brks <- as.numeric(unlist(natural_breaks))
  brks <- natural.breaks
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }

  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  list.map.data.ratio[[1]]$brks <- cut(list.map.data.ratio[[1]][,i],
                               breaks = brks,
                               include.lowest = TRUE,
                               label = labels)
  brks.scale <- levels(list.map.data.ratio[[1]]$brks)
  labels.scale <- rev(brks.scale)

  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
    # base layer to fill polygons where is no data (NA) in gray
    geom_polygon(data = list.map.data.average[[2]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha=0.4)) +
    geom_polygon(data = list.map.data.ratio[[1]], aes(fill = brks,
                                              x = long,
                                              y = lat,
                                              group = group)) +
    # municipality outline
    geom_path(data = list.map.data.ratio[[1]], aes(x = long,
                                           y = lat,
                                           group = group),
              color = "white", size = 0.1) +
    coord_equal() +
    theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47",
                                   # margin = margin(b = -0.1,  # margin creates error
                                   #                 t = -0.1,
                                   #                 l = 2,
                                   #                 unit = "cm"),
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.3,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.5,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6,
                                  hjust = 0.92,
                                  # margin = margin(t = 0.2,
                                  #                 b = 0,
                                  #                 unit = "cm"),
                                  color = "#939184")
    ) +
    labs(x = NULL,
         y = NULL,
         title = "Jordan as country of Asylum",
         subtitle = paste0("Percent of ",colname," of registered refugees by Jordan Governorate \nPercentage of mapped cases:",response_percentage,"%"),
         caption = "Datasource: UNHCR p-code repository") +
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      name = paste0("Percent of ",colname),
      drop = FALSE,
      labels = labels.scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p

  path <- paste0("out/maps/JOR/percentages/adm1/JOR_ADM",list.admlevel[[1]],"_ratio_",colname,".png")
  ggsave(path, p, width=5, height=5,units="in", dpi=300)
}



#ADMINLEVEL 2


for (i in 33:length(list.map.data.ratio[[2]])){
  
  colname <- colnames(list.map.data.ratio[[2]][i])
  natural.breaks <- (classIntervals(list.df.ratio[[2]][,i-30], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute labels
  labels <- c()
  # brks <- as.numeric(unlist(natural_breaks))
  brks <- natural.breaks
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  list.map.data.ratio[[2]]$brks <- cut(list.map.data.ratio[[2]][,i],
                                       breaks = brks,
                                       include.lowest = TRUE,
                                       label = labels)
  brks.scale <- levels(list.map.data.ratio[[2]]$brks)
  labels.scale <- rev(brks.scale)
  
  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  +
    # base layer to fill polygons where is no data (NA) in gray
    geom_polygon(data = list.map.data.average[[2]], aes(x = long, 
                                                        y = lat, 
                                                        group = group, alpha=0.3)) +
    geom_polygon(data = list.map.data.ratio[[2]], aes(fill = brks,
                                                      x = long,
                                                      y = lat,
                                                      group = group)) +
    # municipality outline
    geom_path(data = list.map.data.ratio[[2]], aes(x = long,
                                                   y = lat,
                                                   group = group),
              color = "white", size = 0.1) +
    coord_equal() +
    theme_map() +
    theme(
      legend.position = c(0.5, 0.03),
      legend.text.align = 0,
      legend.background = element_rect(fill = alpha('white', 0.0)),
      legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
      plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47",
                                   # margin = margin(b = -0.1,  # margin creates error
                                   #                 t = -0.1,
                                   #                 l = 2,
                                   #                 unit = "cm"),
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.3,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.5,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6,
                                  hjust = 0.92,
                                  # margin = margin(t = 0.2,
                                  #                 b = 0,
                                  #                 unit = "cm"),
                                  color = "#939184")
    ) +
    labs(x = NULL,
         y = NULL,
         title = "Jordan as country of Asylum",
         subtitle = paste0("Percent of ",colname," of registered refugees by Jordan Governorate \nPercentage of mapped cases:",response_percentage,"%"),
         caption = "Datasource: UNHCR p-code repository") +
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks.scale),
      name = paste0("Percent of ",colname),
      drop = FALSE,
      labels = labels.scale,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = unit(2, units = "mm"),
        keywidth = unit(70/length(labels), units = "mm"),
        title.position = 'top',
        title.hjust = 0.5,
        label.hjust = 1,
        nrow = 1,
        byrow = T,
        reverse = T,
        label.position = "bottom"
      )
    )
  p
  
  path <- paste0("out/maps/JOR/percentages/adm2/JOR_ADM",list.admlevel[[2]],"_ratio_",colname,".png")
  ggsave(path, p, width=5, height=5,units="in", dpi=300)
}




