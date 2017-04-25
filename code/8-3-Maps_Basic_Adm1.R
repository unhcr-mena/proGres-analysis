#source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")

## clear workspace except necessary aggregation function
#keep(country, list.adm1, list.adm2, adm1.list, adm2.list, mapdata.list.adm1, mapdata.list.adm2, map.data.adm1, map.data.adm2, consistency.table, sure = TRUE)





titles <- c("Average # of individuals per case", 
            "Average # of children in the age of 0-14",
            "Average # of adolescent in the age of 15-17", 
            "Average # of persons in working age (15-65)",
            "Average # of elderly in the age of 65+",
            "standard deviation of age within cases",
            "Average age of principal applicant", 
            "Average # of females",
            "Average # of males",
            "% of cases with only female members", 
            "% of cases with only male members",
            "% of cases with female principal applicants",
            "% of dependent persons (children < 14 & elderly 65+) / total", 
            "% of dependent children < 14 / total",
            "% of dependent elderly 65 + / total")

map.list <- list()
final.maps.adm1 <- list()


#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
## ADMINLEVEL1
## outer loop goes through every country
for (n in 1:length(adm1.list)) {
  
  # n <- 1
  data <- adm1.list[[n]][[1]][[1]][[1]]
  
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
  
  if( dx <= 20 && dx > 13 && dy > 13 && dy <= 20) {
        basemap <- get_map(location = this.country.name, zoom = 5, maptype="terrain")
    }

  if ( dx <= 13 && dy <= 13 && dx > 5 && dy > 5) {
            basemap <- get_map(location = this.country.name, zoom = 6, maptype="terrain")
    }

  if ( dx <= 5 && dy <= 5 && dx > 2 && dy > 2) {
            basemap <- get_map(location = this.country.name, zoom = 7, maptype="terrain")
    }

  if ( dx <= 2 && dy <= 2) {
            basemap <- get_map(location = this.country.name, zoom = 8, maptype="terrain")
    }

  

  
  cat (" Starting generating thematic maps \n")
## inner loop goes through all variables in each country
  for (i in 1:length(list.adm1)) {
  data.map <-   fortify(adm1.list[[n]][[1]][[i]][[1]])
  
  ## use variable name of data as file name later 
  colname <- colnames(list.adm1[[i]][3])
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
  
  
  
  cat(paste("now creating themtic map",titles[i] , "for country ", this.country.name, "\n"))
  
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
         title = paste0(this.country.name," as country of Asylum by Governorate\nAverage ",titles[i]),
         subtitle =  paste0("Percentage of mapped cases: ",consistency.table[n,3],"% rounded to one decimal place\n(consistent data rows as percent of absolute number of registered cases in ",this.country.name,")"),
         caption = "Source: UNHCR proGres Registration") + 
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
    theme.symbol() +
    labs(x = NULL, y = NULL, 
         title = paste0("Total number of registered cases in ",this.country.name,":"),
         subtitle = paste0(consistency.table[n,2]," cases")) +
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

    scale_fill_manual(values = c("#D7DBDD","#696969", color), guide = guide_legend(title = "Margin of Error"), labels = c('No data', "N* = 1", labels)) +
    #scale_color_manual(values = "#696969", name = 'the fill', guide_legend(order = 2, title = "gg"),labels = c('m1')) +
      coord_equal() +
    theme.base() + # map text and styling
    theme.confidence() +
    labs(x = NULL, y = NULL,
         title = "",
         caption = "*N is total number of cases.\nMargin of Error can't be calculated in this case.")
  } else
  {
    p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
      #scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      #geom_polygon(aes(alpha = 0.3, fill="#D7DBDD")) +     # administrative polygons
      geom_polygon(colour = "#707272", aes(fill = error.breaks)) +  
      scale_fill_manual(values = legend, guide = guide_legend(title = "Margin of Error"), na.value = "#D7DBDD") +
      coord_equal() +
      theme.base() + # map text and styling
      theme.confidence() + 
      labs(x = NULL, y = NULL, 
           title = "")
  }
  p.confidence
  
  
  ## put all three maps on one (cow-)plot
  p <- ggdraw() +
    draw_plot(p.map, 0, 0, 0.5, 1) +
    draw_plot(p.number, 0.5, 0.5, 0.4, 0.5) +
    draw_plot(p.confidence, 0.5, 0, 0.4, 0.5) +
    draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
 
  map.list[[i]] <- p
  
  # ## safe final plot
  path <- paste0("out/maps/",this.country.code,"/adm1/",this.country.code,"_ADM1_",colname,".png")
  ggsave(path, p, width=15, height=8,units="in", dpi=300)
  
  }
  final.maps.adm1[[n]] <- map.list
}

