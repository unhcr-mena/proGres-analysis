## clear workspace except necessary aggregation function
#keep(country, titles, theme.base, theme.choropleth, theme.choropleth, theme.symbol, theme.confidence, list.adm2, adm2.list, mapdata.list.adm2, map.data.adm2, consistency.table, sure = TRUE)





map.list <- list()
final.maps.adm2 <- list()


#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
## ADMINLEVEL2
## steps here are the same as in loop for admin level 1 
for (n in 1:length(adm2.list)) {
  data <- adm2.list[[n]][[1]][[1]][[1]]
  
  ## compute centers of polygons as position of circles in symbol map
  distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
                           FUN=function(x)mean(range(x)))
  distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
  distcenters[,6:7] <- NULL
  this.country.code <- data[1,7]
  this.country.name <- toString(country[n,3])
  
  cat(paste("Now creating maps for country ", this.country.name, "\n"))
  
  ## calculate max x- and y-distance in geojson to select optimal zoom level of basemap
  dx <- (max(data$long))-(min(data$long))
  dy <- (max(data$lat))-(min(data$lat))
  
  
  ## Not loading base map
  cat (" Now loading base map \n")
  if( dx <= 20 && dy <= 20 && dx > 13 && dy > 13) {
    basemap <- get_map(location = this.country.name, zoom = 5, maptype="satellite")
    } 
  if ( dx <= 13 && dy <= 13 && dx > 5 && dy > 5) {
      basemap <- get_map(location = this.country.name, zoom = 6, maptype="satellite")
    }
    
  if ( dx <= 5 && dy <= 5 && dx > 2 && dy > 2) {
      basemap <- get_map(location = this.country.name, zoom = 7, maptype="satellite")
    }
    
  if ( dx <= 2 && dy <= 2) {
      basemap <- get_map(location = this.country.name, zoom = 8, maptype="satellite")
    }

  ## inner loop goes through all variables in each country
  
  cat (" Starting generating thematic maps \n")
  for (i in 1:length(list.adm2)) {
    data.map <-   fortify(adm2.list[[n]][[1]][[i]][[1]])
    
    ## use variable name of data as file name later 
    colname <- colnames(list.adm2[[i]][3])
    ## compute class breaks for map -> fisher classification is an optimization of natural jenks
    natural.breaks <- (classIntervals(data.map[[10]], n = 5, style = "fisher", intervalClosure='right')$brks) 
    
    # fill column breaks with data and use rounded breaks as labels
    labels <- c()
    brks <- natural.breaks
    for(idx in 1:length(brks)){
        labels <- c(labels,round(brks[idx + 1], 2))
      }
    labels <- labels[1:length(labels)-1]
    
    # prepare legend of map based on breaks and define classes
    data.map$brks <- cut(data.map[[10]], 
                         breaks = brks, 
                         include.lowest = TRUE,
                         label = labels)
    brks.scale <- levels(data.map$brks)
    labels.scale <- rev(brks.scale)
    
    
    cat(paste("now creating thematic map",i, titles[i] , "for country ", this.country.name, "\n"))
    
    # creating choropleth map of variable
    p.map <- ggmap(basemap) +
      #ggplot(data.map[!data.map[,9] %in% data.map[data.map$hole,][,9],], aes(x = long, y = lat, group = idprogres)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(data=data.map, aes(x = long, y = lat, group = group, alpha = 0.5)) + # administrative polygons
      geom_polygon(data=data.map, colour = "white", aes(x = long, y = lat, group = group, fill = brks)) +
      coord_equal() +
      theme.base() +
      theme.choropleth() +     # map text and styling
      labs(x = NULL, y = NULL, 
           title = paste0("Refugees in ", this.country.name,", breakdown per District\n",
                                                       titles[i]),
           subtitle =  paste0("% of mapped cases: ",consistency.table[n,4],"%\n"),
          caption = "Source: UNHCR proGres Registration, based on consistent data rows.") + 
                             
      scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                         drop = FALSE,
                         labels = labels.scale,
                         guide = guide_legend(
                           byrow = T, reverse = T, label.position = "bottom"
                         )
      )
    p.map
    
    # creating map for absolute number of input data 
    p.number <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(colour = "#979A9A", fill="#D7DBDD", aes(alpha = 0.3)) +   
      geom_point(data=distcenters, aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.5, colour="black", stroke=1) + # prop. symbols for absolute number
      scale_size_area(max_size = 12) + # # If you want zero value to have zero size, use scale_size_area instead of scale_size
      guides(fill=guide_legend("Absolute number of cases"), 
             size = guide_legend("Absolute number of cases")) +
      scale_fill_gradient(low="#9999ff", high="#00007f")+
      theme.base() +
      theme.symbol()  +
      labs(x = NULL, y = NULL, 
           title = paste0("# of registered cases: ",consistency.table[n,2])) +
      coord_equal()
    p.number
    
    
    # creating map of level of confidence (margin of error) 
    # create dataframe of rows where N is 1 and Margin of Error couldn't be calculated
    n.is.one <- data.frame()
    n.is.one <- data.map[data.map$N == 1, ]
    n.is.one <- subset(n.is.one, !is.na(N))
    
    # create breaks for Margin of Error
    data.map$error.breaks <- cut(data.map$ci, 
                                 breaks=c(-Inf, 0.3, 0.5, 0.8, Inf), 
                                 labels=c("<= 0.3", "<= 0.5", "<= 0.8", 
                                          "> 0.8"))
    color <- c("chartreuse3", "#f2c300", "#ff5a00", "#d01b22")
    labels <- c("<= 0.3", "<= 0.5", "<= 0.8", "> 0.8")
    legend <- c("<= 0.3" = "chartreuse3", "<= 0.5" = "#f2c300", "<= 0.8" = "#ff5a00", "> 0.8" = "#d01b22")
    
    
    if( nrow(n.is.one) > 0) {
      
      p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
        geom_polygon(aes(fill="#696969")) +     # administrative polygons
        geom_polygon(colour = "#707272", aes(fill = error.breaks)) +
        geom_polygon(data=n.is.one,  aes(x = long, y = lat, group = group, fill= "#D7DBDD")) +
        
        scale_fill_manual(values = c("#D7DBDD","#696969", color),
                          guide = guide_legend(title = "Error in %"),
                          labels = c('  No data', "N* = 1", labels)) +
        #scale_color_manual(values = "#696969", name = 'the fill', guide_legend(order = 2, title = "gg"),labels = c('m1')) +
        coord_equal() +
        theme.base() + # map text and styling
        theme.confidence() +
        labs(x = NULL, y = NULL,
             title = "Margin of Error",
             caption = "Error cannot be calculated when there's no case.")
    } else
    {
      p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
        #scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
        #geom_polygon(aes(alpha = 0.3, fill="#D7DBDD")) +     # administrative polygons
        geom_polygon(colour = "#707272", aes(fill = error.breaks)) +  
        scale_fill_manual(values = legend,
                          guide = guide_legend(title = "Error in %"),
                          na.value = "#D7DBDD") +
        coord_equal() +
        theme.base() + # map text and styling
        theme.confidence() +
        labs(x = NULL, y = NULL,
           title = "Margin of Error",
           caption = "Error cannot be calculated when there's no case.")
    }
    p.confidence
    
    
    ## put all three maps on one (cow-)plot
    p <- ggdraw() +
      draw_plot(p.map, 0, 0, 0.5, 1) +
      draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
      draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
      draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
    
    
    map.list[[i]] <- p
    
    # ## safe final plot
    path <- paste0("out/maps/",this.country.code,"/adm2/",this.country.code,"_adm2_",colname,".png")
    ggsave(path, p, width=15, height=8,units="in", dpi=300)
    
  }
  
  final.maps.adm2[[n]] <- map.list
  
}




# n=1
# ## Detail maps for only Egypt
# for (n in 1:1) {
#   data <- adm2.list[[n]][[1]][[1]][[1]]
#   
#   ## compute centers of polygons as position of circles in symbol map
#   distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
#                            FUN=function(x)mean(range(x)))
#   distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
#   distcenters[,6:7] <- NULL
#   this.country.code <- data[1,7]
#   this.country.name <- toString(country[n,3])
#   
#   
#   ## calculate max x- and y-distance in geojson to select optimal zoom level of basemap
#   dx <- (max(data$long))-(min(data$long))
#   dy <- (max(data$lat))-(min(data$lat))
#   
#     basemap <- get_map(location = "Kairo", zoom = 7, maptype="satellite")
#   plot(basemap)
#   
#   
#   
#   ## inner loop goes through all variables in each country
# #  for (i in 1:length(list.adm2)) {
#   for (i in 1:1) {
#     data.map <-   fortify(adm2.list[[n]][[1]][[i]][[1]])
#     
#     ## use variable name of data as file name later 
#     colname <- colnames(list.adm2[[i]][3])
#     ## compute class breaks for map -> fisher classification is an optimization of natural jenks
#     natural.breaks <- (classIntervals(data.map[[10]], n = 5, style = "fisher", intervalClosure='right')$brks) 
#     
#     # fill column breaks with data and use rounded breaks as labels
#     labels <- c()
#     brks <- natural.breaks
#     for(idx in 1:length(brks)){
#       labels <- c(labels,round(brks[idx + 1], 2))
#     }
#     labels <- labels[1:length(labels)-1]
#     
#     # prepare legend of map based on breaks and define classes
#     data.map$brks <- cut(data.map[[10]], 
#                          breaks = brks, 
#                          include.lowest = TRUE,
#                          label = labels)
#     brks.scale <- levels(data.map$brks)
#     labels.scale <- rev(brks.scale)
#     
#     
#     
#     # creating choropleth map of variable
#     p.map <- ggmap(basemap) +
#       #ggplot(data.map[!data.map[,9] %in% data.map[data.map$hole,][,9],], aes(x = long, y = lat, group = idprogres)) +
#       scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
#       geom_polygon(data=data.map, aes(x = long, y = lat, group = group, alpha = 0.5)) + # administrative polygons
#       geom_polygon(data=data.map, colour = "white", aes(x = long, y = lat, group = group, fill = brks)) +
#       coord_equal() +
#       theme.base() +
#       theme.choropleth() +     # map text and styling
#       labs(x = NULL, y = NULL, 
#            title = paste0("Average ",titles[i],"\nby Governorate of ", this.country.name," as place of asylum"),
#            subtitle =  paste0("Absolute number of registered cases in this country: ",consistency.table[n,2]," cases\nPercentage of mapped cases: ",consistency.table[n,3],"% rounded to one decimal place\n(shows consistent data rows as percent of absolute number of registered cases in this country)"),
#            caption = "Datasource:\nData: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'") + 
#       scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
#                          drop = FALSE,
#                          labels = labels.scale,
#                          guide = guide_legend(
#                            byrow = T, reverse = T, label.position = "bottom"
#                          )
#       )
#     p.map
#     
#     # creating map for absolute number of input data 
#     p.number <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
#       scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
#       geom_polygon(colour = "#979A9A", fill="#D7DBDD", aes(alpha = 0.3)) +   
#       geom_point(data=distcenters, aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.5, colour="black", stroke=1) + # prop. symbols for absolute number
#       scale_size_area(max_size = 12) + # # If you want zero value to have zero size, use scale_size_area instead of scale_size
#       guides(fill=guide_legend("Absolute number of cases"), 
#              size = guide_legend("Absolute number of cases")) +
#       scale_fill_gradient(low="#9999ff", high="#00007f")+
#       theme.base() +
#       theme.symbol() +
#       labs(x = NULL, y = NULL, 
#            title = "") +
#       coord_equal()
#     p.number
#     
#     
#     # creating map of level of confidence (margin of error) 
#     # create dataframe of rows where N is 1 and Margin of Error couldn't be calculated
#     n.is.one <- data.frame()
#     n.is.one <- data.map[data.map$N == 1, ]
#     n.is.one <- subset(n.is.one, !is.na(N))
#     
#     # create breaks for Margin of Error
#     data.map$error.breaks <- cut(data.map$ci, 
#                                  breaks=c(-Inf, 0.3, 0.5, 0.8, Inf), 
#                                  labels=c("<= 0.3", "<= 0.5", "<= 0.8", 
#                                           "> 0.8"))
#     color <- c("chartreuse3", "#f2c300", "#ff5a00", "#d01b22")
#     labels <- c("<= 0.3", "<= 0.5", "<= 0.8", "> 0.8")
#     legend <- c("<= 0.3" = "chartreuse3", "<= 0.5" = "#f2c300", "<= 0.8" = "#ff5a00", "> 0.8" = "#d01b22")
#     
#     
#     if( nrow(n.is.one) > 0) {
#       
#       p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
#         geom_polygon(aes(fill="#696969")) +     # administrative polygons
#         geom_polygon(colour = "#707272", aes(fill = error.breaks)) +
#         geom_polygon(data=n.is.one,  aes(x = long, y = lat, group = group, fill= "#D7DBDD")) +
#         
#         scale_fill_manual(values = c("#D7DBDD","#696969", color), guide = guide_legend(title = "Margin of Error"), labels = c('  No data', "N* = 1", labels)) +
#         #scale_color_manual(values = "#696969", name = 'the fill', guide_legend(order = 2, title = "gg"),labels = c('m1')) +
#         coord_equal() +
#         theme.base() + # map text and styling
#         theme.confidence() +
#         labs(x = NULL, y = NULL,
#              title = "",
#              caption = "*N is total number of cases.\nMargin of Error can't be calculated in this case.")
#     } else
#     {
#       p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
#         #scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
#         #geom_polygon(aes(alpha = 0.3, fill="#D7DBDD")) +     # administrative polygons
#         geom_polygon(colour = "#707272", aes(fill = error.breaks)) +  
#         scale_fill_manual(values = legend, guide = guide_legend(title = "Margin of Error"), na.value = "#D7DBDD") +
#         coord_equal() +
#         theme.base() + # map text and styling
#         theme.confidence() + 
#         labs(x = NULL, y = NULL, 
#              title = "")
#     }
#     p.confidence
#     
#     
#     ## put all three maps on one (cow-)plot
#     p <- ggdraw() +
#       draw_plot(p.map, 0, 0, 0.5, 1) +
#       draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
#       draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
#       draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
#     
#     
#     #map.list[[i]] <- p
#     
#     ## safe final plot
#     path <- paste0("out/maps/",this.country.code,"/adm2/detail_maps/",this.country.code,"_adm2_",colname,".png")
#     ggsave(path, p, width=15, height=8,units="in", dpi=300)
#     
#   }
#   
#   #final.maps.adm2[[n]] <- map.list
#   
# }
# 
