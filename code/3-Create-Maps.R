source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")


## adjust data length for index including coming column 'brks'
map_data1$brks <- 0
map_data1 <- map_data1[,c(1:2, 38, 3:37)] #reorder data so that column 'brks' is at 3. 
df.choropleth$fill <- 0
df.choropleth <- df.choropleth[,c(1:2, 20, 3:19)] ## do the same in df.choropleth to match with index



for (i in 22:length(map_data1)){
  
  
  
  
  colname <- colnames(map_data1[i])
  natural_breaks <- (classIntervals(df.choropleth[,i-18], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map_data
  
  # natural_breaks <- natural_breaks["brks"]
  
  # # find the extremes
  # minVal <- min(map_data1[i], na.rm = T)
  # maxVal <- max(map_data1[i], na.rm = T)
  # interval <- maxVal-minVal
  # step <- interval/5
  # pretty_breaks <- c((minVal+step),(minVal+(2*step)),(minVal+(3*step)),(minVal+(4*step)),(minVal+(5*step)))
  # # pretty_breaks <- c(25,26,27,28,29)
  
  # compute labels
  labels <- c()
  # brks <- as.numeric(unlist(natural_breaks))
  brks <- natural_breaks
  # round the labels (actually, only the extremes)
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  
  labels <- labels[1:length(labels)-1]
  # define a new variable on the data set just as above
  map_data1$brks <- cut(map_data1[,i], 
                        breaks = brks, 
                        include.lowest = TRUE,
                        label = labels)
  
  brks_scale <- levels(map_data1$brks)
  labels_scale <- rev(brks_scale)
  
  ###FINAL MAP
  p <- ggplot() +
    # municipality polygons
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map_data1, aes(fill = brks, 
                                       x = long, 
                                       y = lat, 
                                       group = group)) +
    # municipality outline
    geom_path(data = map_data1, aes(x = long, 
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
                                   margin = margin(b = -0.1, 
                                                   t = -0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      legend.title = element_text(size = 8),
      plot.margin = unit(c(.5,.5,.3,.5), "cm"),
      panel.spacing = unit(c(-.1,0.2,.5,0.2), "cm"),
      panel.border = element_blank(),
      plot.caption = element_text(size = 6, 
                                  hjust = 0.92, 
                                  margin = margin(t = 0.2, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184")
    ) +
    labs(x = NULL, 
         y = NULL, 
         title = "Jordan as country of Asylum", 
         subtitle = paste0(colname," of registered refugees by Jordan Governorate \nPercentage of cases:",response_percentage,"%"), 
         caption = "Datasource: UNHCR p-code repository") + 
    scale_fill_manual(
      values = rev(magma(8, alpha = 0.8)[2:7]),
      breaks = rev(brks_scale),
      name = colname,
      drop = FALSE,
      labels = labels_scale,
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
  
  path <- paste0("out/maps/JOR_ADM",admlevel,"_",colname,".png")
  ggsave(path, p, width=4, height=3,units="in", dpi=300)
  
}






# ##more intuitive legend
# extendLegendWithExtremes <- function(p){
#   p_grob <- ggplotGrob(p)
#   legend <- gtable_filter(p_grob, "guide-box")
#   legend_grobs <- legend$grobs[[1]]$grobs[[1]]
#   # grab the first key of legend
#   legend_first_key <- gtable_filter(legend_grobs, "key-3-1-1")
#   legend_first_key$widths <- unit(2, units = "cm")
#   # modify its width and x properties to make it longer
#   legend_first_key$grobs[[1]]$width <- unit(2, units = "cm")
#   legend_first_key$grobs[[1]]$x <- unit(0.15, units = "cm")
#   
#   # last key of legend
#   legend_last_key <- gtable_filter(legend_grobs, "key-3-6-1")
#   legend_last_key$widths <- unit(2, units = "cm")
#   # analogous
#   legend_last_key$grobs[[1]]$width <- unit(2, units = "cm")
#   legend_last_key$grobs[[1]]$x <- unit(1.02, units = "cm")
#   
#   # grab the last label so we can also shift its position
#   legend_last_label <- gtable_filter(legend_grobs, "label-5-6")
#   legend_last_label$grobs[[1]]$x <- unit(2, units = "cm")
#   
#   # Insert new color legend back into the combined legend
#   legend_grobs$grobs[legend_grobs$layout$name == "key-3-1-1"][[1]] <- 
#     legend_first_key$grobs[[1]]
#   legend_grobs$grobs[legend_grobs$layout$name == "key-3-6-1"][[1]] <- 
#     legend_last_key$grobs[[1]]
#   legend_grobs$grobs[legend_grobs$layout$name == "label-5-6"][[1]] <- 
#     legend_last_label$grobs[[1]]
#   
#   # finally, I need to create a new label for the minimum value 
#   new_first_label <- legend_last_label$grobs[[1]]
#   new_first_label$label <- round(min(map_data$AVG_Age, na.rm = T), 2)
#   new_first_label$x <- unit(-0.15, units = "cm")
#   new_first_label$hjust <- 1
#   
#   legend_grobs <- gtable_add_grob(legend_grobs, 
#                                   new_first_label, 
#                                   t = 6, 
#                                   l = 2, 
#                                   name = "label-5-0", 
#                                   clip = "off")
#   legend$grobs[[1]]$grobs[1][[1]] <- legend_grobs
#   p_grob$grobs[p_grob$layout$name == "guide-box"][[1]] <- legend
#   
#   # the plot is now drawn using this grid function
#   grid.newpage()
#   grid.draw(p_grob)
# }
# 
# 
# 
# extendLegendWithExtremes(p)

