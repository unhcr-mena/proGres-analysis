#source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")

## clear workspace except necessary aggregation function
#keep(country, list.adm1, list.adm2, adm1.list, adm2.list, mapdata.list.adm1, mapdata.list.adm2, map.data.adm1, map.data.adm2, consistency.table, sure = TRUE)

devtools::install_github("wilkelab/cowplot", force=TRUE)
library(cowplot)


##################################################################
## styling theme for maps
## theme for general text and borders which is the same in all maps
theme.base <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.spacing = unit(c(-.1,1,0.02,1), "cm"),
      
      plot.caption = element_text(size = 9, hjust = 0, color = "#595851"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      
      aspect.ratio=1.2/1.8,
      ...
    )
}

## theme for main choropleth map
theme.choropleth <- function(...) {
  theme(
    plot.title = element_text(hjust = 0, color = "black", size = 20, face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "black", size = 13, debug = F),
    
    legend.direction = "horizontal",
    legend.position = "bottom", #c(0.5, 0.01),
    legend.text.align = 1,                                                #postition of label: 0=left, 1=right
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 12, hjust = 0, color = "black"),
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
    legend.title = element_text(size=12, face="bold", color = "black", hjust = 1),  
    plot.title = element_text(size=12, face="bold", color = "black", hjust = 1), 
    plot.subtitle = element_text(size=17, face="bold", color = "black", hjust = 1),  
    legend.direction = "vertical",
    legend.position = "right", #c(0.5, 0.01),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12, color = "black"),
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
    legend.title = element_text(size=12, face="bold", color = "black"), 
    legend.direction = "vertical",
    legend.position = "right", #c(0.15, 1),                              
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12, color = "black"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    legend.key.height = unit(7, units = "mm"),            #height of legend
    legend.key.width = unit(6, units = "mm"),             #width of legend
    legend.key.size = unit(1.5, 'lines'),
    
    
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.margin = unit(c(0.3,1.05,0.3,0,3), "cm"),
    
    ...
  )
}

titles <- c("number of individuals per case", 
            "number of children in the age of 0-14", "number of adolescent in the age of 15-17", 
            "number of persons in working age (15-65)", "number of elderly in the age of 65+",
            "standard deviation of age within cases", "age of principal applicant", 
            "number of females", "number of males", "proportion of cases with only female members", 
            "proportion of cases with only male members", "proportion of cases with female principal applicants",
            "proportion of dependent persons (children < 14 & elderly 65+) / total", 
            "proportion of dependent children < 14 / total", "proportion of dependent elderly 65 + / total")

map.list.egypt <- list()
final.maps.adm1.egypt <- list()
final.maps.adm2.egypt <- list()

## basemap
basemap <- get_map("Egypt", zoom = 5, maptype="satellite")


## creation of output folders

#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
## ADMINLEVEL1
## outer loop goes through every country
n=1
for (n in 1:1) {
  data <- adm1.list[[n]][[1]][[1]][[1]]
  
  ## compute centers of polygons as position of circles in symbol map
  distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
                           FUN=function(x)mean(range(x)))
  distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
  distcenters[,6:7] <- NULL
  this.country.code <- data[1,7]
  this.country.name <- toString(country[n,3])
  
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
      coord_map(xlim = c(29, 34),ylim = c(27.5, 31.7)) +
      theme.base() +
      theme.choropleth() +     # map text and styling
      labs(x = NULL, y = NULL, 
           title = paste0(this.country.name," as plase of Asylum\nAverage ",titles[i]," by Governorate"),
           subtitle =  paste0("Percentage of mapped cases: ",consistency.table[n,3],"% rounded to one decimal place\n(shows consistent data rows as percent of absolute number of registered cases in this country)"),
           caption = "Source:\nData: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google") + 
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
      guides(fill=guide_legend("Absolute number of cases\nby Governorate"), 
             size = guide_legend("Absolute number of cases\nby Governorate")) +
      scale_fill_gradient(low="#9999ff", high="#00007f")+
      theme.base() +
      theme.symbol() +
      labs(x = NULL, y = NULL, 
           title = paste0("Total number of registered cases in ",this.country.name,":"),
           subtitle = paste0(consistency.table[n,2]," cases")) +
      coord_map(xlim = c(28.5, 34),ylim = c(27.5, 31.7)) 
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
        coord_map(xlim = c(28.5, 34),ylim = c(27.5, 31.7)) +
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
        coord_map(xlim = c(28.5, 34),ylim = c(27.5, 32.3)) +
        theme.base() + # map text and styling
        theme.confidence() + 
        labs(x = NULL, y = NULL, 
             title = "")
    }
    p.confidence
    
    
    ## put all three maps on one (cow-)plot
    p <- ggdraw() +
      draw_plot(p.map, 0, 0, 0.5, 1) +
      draw_plot(p.number, 0.46, 0.5, 0.5, 0.5) +
      draw_plot(p.confidence, 0.46, 0, 0.5, 0.5) +
      draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
    p
    map.list.egypt[[i]] <- p
    
    # ## safe final plot
    # path <- paste0("out/maps/",this.country.code,"/adm1/detail_maps/",this.country.code,"_ADM1_",colname,".png")
    # ggsave(path, p, width=15, height=8,units="in", dpi=300)
    
  }
  final.maps.adm1.egypt[[n]] <- map.list
}




## ADMINLEVEL2
for (n in 1:1) {
  data <- adm2.list[[n]][[1]][[1]][[1]]
  
  ## compute centers of polygons as position of circles in symbol map
  distcenters <- aggregate(cbind(long, lat) ~ idprogres + admname, data=data, 
                           FUN=function(x)mean(range(x)))
  distcenters <- plyr::join(x=distcenters, y=data, by="idprogres")
  distcenters[,6:7] <- NULL
  this.country.code <- data[1,7]
  this.country.name <- toString(country[n,3])
  
  i=1
  ## inner loop goes through all variables in each country
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
    
    
    
    # creating choropleth map of variable
    p.map <- ggmap(basemap) +
      #ggplot(data.map[!data.map[,9] %in% data.map[data.map$hole,][,9],], aes(x = long, y = lat, group = idprogres)) +
      scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
      geom_polygon(data=data.map, aes(x = long, y = lat, group = group, alpha = 0.5)) + # administrative polygons
      geom_polygon(data=data.map, colour = "white", aes(x = long, y = lat, group = group, fill = brks)) +
      coord_map(xlim = c(29.5, 32),ylim = c(29.8, 31.7)) +
      theme.base() +
      theme.choropleth() +     # map text and styling
      labs(x = NULL, y = NULL, 
           title = paste0(this.country.name," as plase of Asylum\nAverage ",titles[i]," by Governorate"),
           subtitle =  paste0("Percentage of mapped cases: ",consistency.table[n,4],"% rounded to one decimal place\n(shows consistent data rows as percent of absolute number of registered cases in this country)"),
           caption = "Source:\nData: UNHCR proGres\nShapefiles: UNHCR Github repository 'p-codes'\nBasemap: Google") + 
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
      geom_polygon(colour = "#979A9A", fill="#D7DBDD", aes(alpha = 0.3)) +   
      geom_point(data=distcenters, aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.5, colour="black", stroke=1) + # prop. symbols for absolute number
      scale_size_area(max_size = 12) + # # If you want zero value to have zero size, use scale_size_area instead of scale_size
      guides(fill=guide_legend("Absolute number of cases\nby Governorate"), 
             size = guide_legend("Absolute number of cases\nby Governorate")) +
      scale_fill_gradient(low="#9999ff", high="#00007f")+
      theme.base() +
      theme.symbol() +
      labs(x = NULL, y = NULL, 
           title = paste0("Total number of registered cases in ",this.country.name,":"),
           subtitle = paste0(consistency.table[n,2]," cases")) +
      coord_map(xlim = c(29.5, 32),ylim = c(29.8, 31.7)) 
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
        coord_map(xlim = c(29.5, 32),ylim = c(29.8, 31.7))  +
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
        coord_map(xlim = c(29.5, 32),ylim = c(29.8, 31.7))  +
        theme.base() + # map text and styling
        theme.confidence() + 
        labs(x = NULL, y = NULL, 
             title = "")
    }
    p.confidence
    
    
    ## put all three maps on one (cow-)plot
    p <- ggdraw() +
      draw_plot(p.map, 0, 0, 0.5, 1) +
      draw_plot(p.number, 0.46, 0.5, 0.5, 0.5) +
      draw_plot(p.confidence, 0.46, 0, 0.5, 0.5) +
      draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
    p
    map.list[[i]] <- p
    
    # ## safe final plot
    # path <- paste0("out/maps/",this.country.code,"/adm2/detail_maps/",this.country.code,"_adm2_",colname,".png")
    # ggsave(path, p, width=15, height=8,units="in", dpi=300)
    
  }
  final.maps.adm2.egypt[[n]] <- map.list
}
