#source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")

## clear workspace except necessary aggregation function
keep(country, list.adm1, list.adm2, adm1.list, adm2.list, mapdata.list.adm1, mapdata.list.adm2, map.data.adm1, map.data.adm2, consistency.table, sure = TRUE)

devtools::install_github("wilkelab/cowplot", force=TRUE)
library(cowplot)


##################################################################
## styling theme for maps
## theme for general text and borders which is the same in all maps
theme.base <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Ubuntu Regular", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.spacing = unit(c(-.1,0.2,0.02,0.2), "cm"),
      
      plot.caption = element_text(size = 8, hjust = 1, color = "#939184"),
      panel.grid.minor = element_blank(),
      
      panel.border = element_blank(),
      
      ...
    )
}

## theme for main choropleth map
theme.choropleth <- function(...) {
  theme(
    plot.title = element_text(hjust = 0, color = "#4e4d47", size = 15),
    plot.subtitle = element_text(hjust = 0, color = "#4e4d47", size = 10, debug = F),
    
    legend.direction = "horizontal",
    legend.position = "bottom", #c(0.5, 0.01),
    legend.text.align = 1,                                                #postition of label: 0=left, 1=right
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 10, hjust = 0, color = "#4e4d47"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    legend.key.height = unit(4, units = "mm"),                             #height of legend
    legend.key.width = unit(100/length(labels), units = "mm"),             #width of legend
    legend.title = element_text(size = 0),           #put to 0 to remove title but keep space
    
    panel.grid.major = element_line(color = "#f5f5f2", size = 0.2),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = unit(c(.5,.5,1,.5), "cm"),
    
    ...
  )
}

## theme for bubble map absolute number of datarows
theme.symbol <-  function(...) {
  theme(
    legend.title = element_text(size=10, face="bold"),   
    legend.direction = "vertical",
    legend.position = "right", #c(0.5, 0.01),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 10, color = "#4e4d47"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.margin = unit(c(0.3,0.3,0.3,0,3), "cm"),
    
    ...
  )
}

## theme for choropleth map margin of error
theme.confidence <- function(...) {
  theme(
    legend.title = element_text(size=10, face="bold"), 
    legend.direction = "vertical",
    legend.position = "right", #c(0.15, 1),
    legend.text.align = 1,                                #postition of label: 0=left, 1=right
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 10, color = "#4e4d47"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    legend.key.height = unit(5, units = "mm"),            #height of legend
    legend.key.width = unit(6, units = "mm"),             #width of legend
    legend.key.size = unit(1.5, 'lines'),
    
    
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.margin = unit(c(0.3,1.05,0.3,0,3), "cm"),
    
    ...
  )
}


titles <- c("number of individuals", 
            "number of children in the age of 0-14", "number of adolescent in the age of 15-17", "number of persons in working age (15-65)", "number of elderly in the age of 65+",
            "standard deviation of age within cases", "age of principal applicant", 
            "number of females", "number of males", "number of cases with only female members", "number of cases with only male members", "number of cases with female principal applicants",
            "number of dependent persons (children < 14 & elderly 65+)", "number of dependent children < 14", "number of dependent elderly 65 +")


## creation of output folders for maps 
for (i in 1:nrow(country)) {
  ## create output folder for maps
  mainDir <- "out"
  subDir <- paste0("/maps/",country[i,2],"/adm1")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  subDir <- paste0("/maps/",country[i,2],"/adm2")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
}







#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
## ADMINLEVEL1

## outer loop goes through every country
for (n in 1:length(adm1.list)) {
  data <- adm1.list[[n]][[1]][[1]][[1]]
  
  ## compute centers of polygons as position of circles in symbol map
  distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
                           FUN=function(x)mean(range(x)))
  distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
  this.country.code <- data[1,7]
  this.country.name <- toString(country[n,3])

  
  ## calculate max x- and y-distance in geojson to select good zoom level of basemap
  dx <- (max(data$long))-(min(data$long))
  dy <- (max(data$lat))-(min(data$lat))
  
  if( dx <= 20 && dy <= 20) {
        basemap <- get_map(location = this.country.name, zoom = 5, maptype="satellite")
    
    if ( dx <= 13 && dy <= 13) {
            basemap <- get_map(location = this.country.name, zoom = 6, maptype="satellite")
    }
    
    if ( dx <= 5 && dy <= 5) {
            basemap <- get_map(location = this.country.name, zoom = 7, maptype="satellite")
    }
    
    if ( dx <= 2 && dy <= 2) {
            basemap <- get_map(location = this.country.name, zoom = 8, maptype="satellite")
    }
  }
  
  

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
         title = paste0("Average ",titles[i],"\nby Governorate of ", this.country.name," as place of asylum"),
         subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases in total: ",consistency.table[n,3],"%\n(percent shows consistent data rows as proportion of all registered cases for this country)"),
         caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
    scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                       drop = FALSE,
                       labels = labels.scale,
                       guide = guide_legend(
                         byrow = T, reverse = T, label.position = "bottom"
                       )
    )
  
  # creating map for absolute number of input data 
  p.number <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(colour = "#979A9A", fill="#D7DBDD", aes(alpha = 0.5)) +  
    geom_point(data=distcenters, colour= "#424949", aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.8, stroke=1) + # prop. symbols for absolute number
    scale_size(range = c(4, 12)) +
    scale_fill_gradient(low="#D4E6F1", high="#154360")+
    guides(fill=guide_legend("Absolute number of cases"), size = guide_legend("Absolute number of cases")) +
    theme.base() +
    theme.symbol() +
    labs(x = NULL, y = NULL, 
         title = "") +
    coord_equal()
  
  
 
  
  # creating map of level of confidence (margin of error) 
  p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(aes(alpha = 0.5)) +     # administrative polygons
    geom_polygon(colour = "white", aes(fill = cut(ci, c(-Inf, 0.3, 0.5, 0.8, Inf)))) +  
    scale_fill_manual(name = "Margin of Error",
                      values = c("(-Inf,0.3]" = "#088A4B",
                                 "(0.3,0.5]" = "#86B404",
                                 "(0.5, 0.8]" = "#DF7401",
                                 "(0.8, Inf]" = "#FE2E2E"),
                      labels = c("<= 0.3", "<= 0.5", "<= 0.8","> 0.8")) +
    coord_equal() +
    theme.base() + # map text and styling
    theme.confidence() + 
    labs(x = NULL, y = NULL, 
         title = "")

  
  ## put all three maps on one (cow-)plot
  p <- ggdraw() +
    draw_plot(p.map, 0, 0, 0.5, 1) +
    draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
    draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
    draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
  #p
  
  ## safe final plot
  path <- paste0("out/maps/",this.country.code,"/adm1/",this.country.code,"_ADM1_",colname,".png")
  ggsave(path, p, width=15, height=8,units="in", dpi=300)
  
}
}








#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
## ADMINLEVEL2
## steps are the same as in loop for admin level 1 


for (n in 1:length(adm2.list)) {
  data <- adm2.list[[n]][[1]][[1]][[1]]
  distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
                           FUN=function(x)mean(range(x)))
  distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
  this.country.code <- data[[1,7]]
  this.country.name <- toString(country[n,3])
  
  ## calculate max x- and y-distance in geojson to select good zoom level of basemap
  dx <- (max(data$long))-(min(data$long))
  dy <- (max(data$lat))-(min(data$lat))
  
  if( dx <= 20 && dy <= 20) {
    basemap <- get_map(location = this.country.name, zoom = 5, maptype="satellite")
    
    if ( dx <= 13 && dy <= 13) {
      basemap <- get_map(location = this.country.name, zoom = 6, maptype="satellite")
    }
    
    if ( dx <= 5 && dy <= 5) {
      basemap <- get_map(location = this.country.name, zoom = 7, maptype="satellite")
    }
    
    if ( dx <= 2 && dy <= 2) {
      basemap <- get_map(location = this.country.name, zoom = 8, maptype="satellite")
    }
  }

  for (i in 1:length(list.adm2)) {
    data.map <-   fortify(adm2.list[[n]][[1]][[i]][[1]])
    
    colname <- colnames(list.adm2[[i]][3])
   
    natural.breaks <- (classIntervals(data.map[[10]], n = 5, style = "fisher", intervalClosure='right')$brks) 
    
    labels <- c()
    brks <- natural.breaks
    for(idx in 1:length(brks)){
      labels <- c(labels,round(brks[idx + 1], 2))
    }
    labels <- labels[1:length(labels)-1]
    
    data.map$brks <- cut(data.map[[10]], 
                         breaks = brks, 
                         include.lowest = TRUE,
                         label = labels)
    brks.scale <- levels(data.map$brks)
    labels.scale <- rev(brks.scale)
    
    p.map <- ggmap(basemap) +
      #ggplot(data.map[!data.map[,9] %in% data.map[data.map$hole,][,9],], aes(x = long, y = lat, group = idprogres)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(data=data.map, aes(x = long, y = lat, group = group, alpha = 0.5)) + # administrative polygons
      geom_polygon(data=data.map, colour = "white", aes(x = long, y = lat, group = group, fill = brks)) +
      coord_equal() +
      theme.base() +
      theme.choropleth() +     # map text and styling
      labs(x = NULL, y = NULL, 
           title = paste0("Average ",titles[i],"\nby Governorate of ", this.country.name," as place of asylum"),
           subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases in total: ",consistency.table[n,3],"%\n(percent shows consistent data rows as proportion of all registered cases for this country)"),
           caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
      scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                         drop = FALSE,
                         labels = labels.scale,
                         guide = guide_legend(
                           byrow = T, reverse = T, label.position = "bottom"
                         )
      )
    
    p.number <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(colour = "#979A9A", fill="#D7DBDD", aes(alpha = 0.5)) +  
      geom_point(data=distcenters, colour= "#424949", aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.8, stroke=1) + # prop. symbols for absolute number
      scale_size(range = c(4, 12)) +
      scale_fill_gradient(low="#D4E6F1", high="#154360")+
      guides(fill=guide_legend("Absolute number of cases"), size = guide_legend("Absolute number of cases")) +
      theme.base() +
      theme.symbol() +
      labs(x = NULL, y = NULL, 
           title = "") +
      coord_equal()
    
    p.confidence <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(aes(alpha = 0.5)) +    
      geom_polygon(colour = "white", aes(fill = cut(ci, c(-Inf, 0.3, 0.5, 0.8, Inf)))) +  
      scale_fill_manual(name = "Margin of Error",
                        values = c("(-Inf,0.3]" = "#088A4B",
                                   "(0.3,0.5]" = "#86B404",
                                   "(0.5, 0.8]" = "#DF7401",
                                   "(0.8, Inf]" = "#FE2E2E"),
                        labels = c("<= 0.3", "<= 0.5", "<= 0.8","> 0.8")) +
      coord_equal() +
      theme.base() +
      theme.confidence() + 
      labs(x = NULL, y = NULL, 
           title = "")
    
    p <- ggdraw() +
      draw_plot(p.map, 0, 0, 0.5, 1) +
      draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
      draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
      draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
    
    path <- paste0("out/maps/",this.country.code,"/adm2/",this.country.code,"_ADM2_",colname,".png")
    ggsave(path, p, width=15, height=8,units="in", dpi=300)
    
  }
}

