#source("code/0-packages.R")
# source("code/3-Create-Maps-data-preparation.R")
devtools::install_github("wilkelab/cowplot")
library(cowplot)

##################################################################
#styling theme for maps
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

titlename <- c("number of individuals", 
           "number of children in the age of 0-14", "number of adolescent in the age of 15-17", "number of persons in working age (15-65)", "number of elderly in the age of 65+",
           "standard deviation of age within cases", "age of head of case", 
           "number of females", "number of males", "number of cases with only female members", "number of cases with only male members", "number of female headed cases",
           "number of dependent persons (children < 14 & elderly 65+)", "number of dependent children < 14", "number of dependent elderly 65 +")


#jor <- get_map("Jordan", zoom = 7, maptype = "terrain")
#jor <- get_map("Jordan", zoom = 7, source = "osm")
jor <- get_map("Jordan", zoom = 7, maptype="satellite")
#ggmap(jor)
n=1
#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
#ADMINLEVEL1
for (n in 1:length(list.adm1)) {       
  
  
  # now we join the thematic data
  map.data <- plyr::join(x=map.data1, y=list.adm1[[n]], by="idprogres")
  
  #adjust length of data for loop in '3-Create-Map'
  map.data$brks <- 0
  map.data <- map.data[,c(ncol(map.data),1:(ncol(map.data)-1))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
  #list.adm1[[n]]['fill'] <- 0
  #list.adm1[[n]] <- list.adm1[[n]][,c(ncol(list.adm1[[n]]['fill']),1:(ncol(list.adm1[[n]])-1))] ## do the same in df.choropleth to match with index
 
  distcenters <- aggregate(cbind(long, lat) ~ N + name, data=map.data, 
                           FUN=function(x)mean(range(x)))
   
# for (i in 11:length(list.map.data.average[[n]])){  # loop for different data/columns within each admin level
  
#  var.labels <- df.labels.average[[2]][i]
  colname <- colnames(list.adm1[[n]][3])
  natural.breaks <- (classIntervals(map.data[,12], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute breaks and labels
  labels <- c()
  brks <- natural.breaks
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  
  # define a new variable on the data set with computed breaks
  map.data$brks <- cut(map.data[,12], 
                        breaks = brks, 
                        include.lowest = TRUE,
                        label = labels)
  brks.scale <- levels(map.data$brks)
  labels.scale <- rev(brks.scale)
  
  # creation of map for variable
  p.map <- ggmap(jor) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, aes(x = long, y = lat, group = group, alpha = 0.5)) +     # administrative polygons
    geom_polygon(data = map.data, colour = "white", aes(fill = brks, x = long, y = lat, group = group)) +
    coord_equal() +
    theme.base() +
    theme.choropleth() +     # map text and styling
    labs(x = NULL, y = NULL, 
         title = paste0("Average ",titlename[n],"\nby Governorate of Jordan as place of asylum"),
         subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases in total: ",pct.consistent.data.adm1,"%\n(percent shows consistent data rows as proportion of all registered cases for this country)"),
         caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
    scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                       drop = FALSE,
                       labels = labels.scale,
                       guide = guide_legend(
                         byrow = T, reverse = T, label.position = "bottom"
                       )
    )
  p.map
  
  
  # creation of map for absolute number of input data by variable and admin level
  p.number <- ggplot() +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, colour = "#979A9A", fill="#D7DBDD", aes(x = long, y = lat, group = group, alpha = 0.5)) +  
    geom_point(data=distcenters, colour= "#424949", aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.8, stroke=1) + # prop. symbols for absolute number
    scale_size(range = c(4, 12)) +
    scale_fill_gradient(low="#D4E6F1", high="#154360")+
    guides(fill=guide_legend("Absolute number of cases"), size = guide_legend("Absolute number of cases")) +
    theme.base() +
    theme.symbol() +
    labs(x = NULL, y = NULL, 
         title = "") +
    coord_equal()
  p.number
  
    # creation of map for confidence (margin of error) of variable by admin level
  p.confidence <- ggplot() +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, aes(x = long, y = lat, group = group, alpha = 0.5)) +     # administrative polygons
    geom_polygon(data = map.data, colour = "white", aes(fill = cut(ci, c(-Inf, 0.3, 0.5, 0.8, Inf)), x = long, y = lat, group = group)) +  #fill = ci <= 0.3
    scale_fill_manual(name = "Margin of Error",
                       values = c("(-Inf,0.3]" = "#088A4B",
                                  "(0.3,0.5]" = "#86B404",
                                  "(0.5, 0.8]" = "#DF7401",
                                  "(0.8, Inf]" = "#FE2E2E"),
                       labels = c("<= 0.3", "<= 0.5", "<= 0.8","> 0.8")) +
    # scale_fill_gradientn(colours=c("#32723A","#F86E28","#AC4537"), 
    #                      breaks=c(0.3,0.5,0.8), labels=format(c("< 0.3","< 0.5","< 0.8"))) +
    # guides(title="Margin of Error") +
    coord_equal() +
    #scale_fill_manual(name = '', values = setNames(c('#A9DFBF','#EC7063'),c(T, F))) +
    #scale_fill_manual(name="Margin of Error", colour="blue") +
    theme.base() + # map text and styling
    theme.confidence() + 
    labs(x = NULL, y = NULL, 
         title = "")
  p.confidence
  
  # path <- paste0("out/maps/JOR/average_numbers/adm1/JOR_ADM1_conf_",colname,".png")
  # ggsave(path, p.map, width=10, height=10,units="in", dpi=300)
  # 
  # path <- paste0("out/maps/JOR/average_numbers/adm1_confidence/JOR_ADM1_avg_",colname,".png")
  # ggsave(path, p.confidence, width=5, height=5,units="in", dpi=300)
  # 
  # path <- paste0("out/maps/JOR/average_numbers/adm1_numbers/JOR_ADM1_numb_",colname,".png")
  # ggsave(path, p.number, width=5, height=5,units="in", dpi=300)
  
  p <- ggdraw() +
    draw_plot(p.map, 0, 0, 0.5, 1) +
    draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
    draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
    draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
  
  path <- paste0("out/maps/JOR/average_numbers/adm1_final/JOR_ADM1_",colname,".png")
  ggsave(path, p, width=15, height=8,units="in", dpi=300)

}



#### SINGLE CHOROPLETH MAP LOOP AVERAGE ###############################################################################
#ADMINLEVEL2
  
for (n in 1:length(list.adm2)) {       
  

  # now we join the thematic data
  map.data <- plyr::join(x=map.data2, y=list.adm2[[n]], by="idadm2")
  
  #adjust length of data for loop in '3-Create-Map'
  map.data$brks <- 0
  map.data <- map.data[,c(ncol(map.data),1:(ncol(map.data)-1))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
  #list.adm1[[n]]['fill'] <- 0
  #list.adm1[[n]] <- list.adm1[[n]][,c(ncol(list.adm1[[n]]['fill']),1:(ncol(list.adm1[[n]])-1))] ## do the same in df.choropleth to match with index
  
  distcenters <- aggregate(cbind(long, lat) ~ N + adm2name, data=map.data, 
                           FUN=function(x)mean(range(x)))
  
  # for (i in 11:length(list.map.data.average[[n]])){  # loop for different data/columns within each admin level
  
  #  var.labels <- df.labels.average[[2]][i]
  colname <- colnames(list.adm2[[n]][3])
  natural.breaks <- (classIntervals(map.data[,12], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
  
  # compute breaks and labels
  labels <- c()
  brks <- natural.breaks
  for(idx in 1:length(brks)){
    labels <- c(labels,round(brks[idx + 1], 2))
  }
  labels <- labels[1:length(labels)-1]
  
  # define a new variable on the data set with computed breaks
  map.data$brks <- cut(map.data[,12], 
                       breaks = brks, 
                       include.lowest = TRUE,
                       label = labels)
  brks.scale <- levels(map.data$brks)
  labels.scale <- rev(brks.scale)
  
  # creation of map for variable
  p.map <- ggmap(jor) +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, aes(x = long, y = lat, group = group, alpha = 0.5)) +     # administrative polygons
    geom_polygon(data = map.data, colour = "white", aes(fill = brks, x = long, y = lat, group = group)) +
    coord_equal() +
    theme.base() +
    theme.choropleth() +     # map text and styling
    labs(x = NULL, y = NULL, 
         title = paste0("Average ",titlename[n],"\nby District of Jordan as place of asylum"),
         subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases in total: ",pct.consistent.data.adm2,"%\n(percent shows consistent data rows as proportion of all registered cases for this country)"),
         caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
    scale_fill_manual( values = rev(magma(8, alpha = 0.8)[2:7]), breaks = rev(brks.scale),
                       drop = FALSE,
                       labels = labels.scale,
                       guide = guide_legend(
                         byrow = T, reverse = T, label.position = "bottom"
                       )
    )
  p.map
  
  
  # creation of map for absolute number of input data by variable and admin level
  # creation of map for absolute number of input data by variable and admin level
  p.number <- ggplot() +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, colour = "#979A9A", fill="#D7DBDD", aes(x = long, y = lat, group = group, alpha = 0.5)) +  
    geom_point(data=distcenters, colour= "#424949", aes(x=long, y=lat, size=N, fill=N), shape=21, alpha=0.8, stroke=1) + # prop. symbols for absolute number
    scale_size(range = c(4, 12)) +
    scale_fill_gradient(low="#D4E6F1", high="#154360")+
    guides(fill=guide_legend("Absolute number of cases"), size = guide_legend("Absolute number of cases")) +
    theme.base() +
    theme.symbol() +
    labs(x = NULL, y = NULL, 
         title = "") +
    coord_equal()
  p.number
  
  # creation of map for confidence (margin of error) of variable by admin level
  p.confidence <- ggplot() +
    scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
    geom_polygon(data = map.data, aes(x = long, y = lat, group = group, alpha = 0.5)) +     # administrative polygons
    geom_polygon(data = map.data, colour = "white", aes(fill = cut(ci, c(-Inf, 0.3, 0.5, 0.8, Inf)), x = long, y = lat, group = group)) +  #fill = ci <= 0.3
    scale_fill_manual(name = "Margin of Error",
                      values = c("(-Inf,0.3]" = "#088A4B",
                                 "(0.3,0.5]" = "#86B404",
                                 "(0.5, 0.8]" = "#DF7401",
                                 "(0.8, Inf]" = "#FE2E2E"),
                      labels = c("<= 0.3", "<= 0.5", "<= 0.8","> 0.8")) +
    # scale_fill_gradientn(colours=c("#32723A","#F86E28","#AC4537"), 
    #                      breaks=c(0.3,0.5,0.8), labels=format(c("< 0.3","< 0.5","< 0.8"))) +
    # guides(title="Margin of Error") +
    coord_equal() +
    #scale_fill_manual(name = '', values = setNames(c('#A9DFBF','#EC7063'),c(T, F))) +
    #scale_fill_manual(name="Margin of Error", colour="blue") +
    theme.base() + # map text and styling
    theme.confidence() + 
    labs(x = NULL, y = NULL, 
         title = "")
  p.confidence
   
  # path <- paste0("out/maps/JOR/average_numbers/adm2/JOR_ADM2_",colname,".png")
  # ggsave(path, p.map, width=10, height=10,units="in", dpi=300)
  # 
  # path <- paste0("out/maps/JOR/average_numbers/adm2_confidence/JOR_ADM2_conf_",colname,".png")
  # ggsave(path, p.confidence, width=5, height=5,units="in", dpi=300)
  # 
  # path <- paste0("out/maps/JOR/average_numbers/adm2_numbers/JOR_ADM2_numb_",colname,".png")
  # ggsave(path, p.number, width=5, height=5,units="in", dpi=300)
  
  p <- ggdraw() +
    draw_plot(p.map, 0, 0, 0.5, 1) +
    draw_plot(p.number, 0.5, 0.5, 0.5, 0.5) +
    draw_plot(p.confidence, 0.5, 0, 0.5, 0.5) +
    draw_plot_label(c("", "", ""), c(0, 0.5, 0.5), c(0.5, 1, 0.5), size = 15)
  
  path <- paste0("out/maps/JOR/average_numbers/adm2_final/JOR_ADM2_",colname,".png")
  ggsave(path, p, width=15, height=8,units="in", dpi=300)
  
}




#not working so far
# #### SINGLE CHOROPLETH MAP LOOP RATIO ###############################################################################
# #ADMINLEVEL 1+2
# 
# 
# for (n in 1:length(list.multifactor.ratio)) {       
#   for (i in 11:length(list.multifactor.ratio[[n]])){  # loop for different data/columns within each admin level
#     
#     
#     list.multifactor.ratio[[n]]$brks <- 0
#     list.multifactor.ratio[[n]] <- list.multifactor.ratio[[n]][,c(ncol(list.multifactor.ratio[[n]]),1:(ncol(list.multifactor.ratio[[n]])-1))] #reorder data so that column 'brks' is at 3, first comma means keep all the rows
#     df.multifactor.ratio[[n]]$fill <- 0
#     df.multifactor.ratio[[n]] <- df.multifactor.ratio[[n]][,c(ncol(df.multifactor.ratio[[n]]),1:(ncol(df.multifactor.ratio[[n]])-1))] ## do the same in df.choropleth to match with index
#     
#     
#     # var.labels <- df.labels.average[[2]][i]
#     colname <- colnames(list.multifactor.ratio[[n]][i])
#     natural.breaks <- (classIntervals(df.multifactor.ratio[[n]][,i-8], n = 5, style = "fisher", intervalClosure='right')$brks) #applied in df.choropleth because much faster as in map.data
#     
#     # compute breaks and labels
#     labels <- c()
#     brks <- natural.breaks
#     for(idx in 1:length(brks)){
#       labels <- c(labels,round(brks[idx + 1], 2))
#     }
#     labels <- labels[1:length(labels)-1]
#     
#     # define a new variable on the data set with computed breaks
#     list.multifactor.ratio[[n]]$brks <- cut(list.multifactor.ratio[[n]][,i], 
#                                            breaks = brks, 
#                                            include.lowest = TRUE,
#                                            label = labels)
#     brks.scale <- levels(list.multifactor.ratio[[n]]$brks)
#     labels.scale <- rev(brks.scale)
#     
#     
#     # creation of map
#     p <- ggplot() +
#       # administrative polygons
#       scale_alpha(name = "", range = c(0.6, 0), guide = F)  + 
#       geom_polygon(data = list.multifactor.ratio[[n]], aes(x = long, 
#                                                           y = lat, 
#                                                           group = group, alpha = 0.5)) +
#       geom_polygon(data = list.multifactor.ratio[[n]], aes(fill = brks, 
#                                                           x = long, 
#                                                           y = lat, 
#                                                           group = group)) +
#       # administrative border
#       geom_path(data = list.multifactor.ratio[[n]], aes(x = long, 
#                                                        y = lat, 
#                                                        group = group), 
#                 color = "white", size = 0.1) +
#       coord_equal() +
#       # map text and styling
#       theme_map() +
#       labs(x = NULL, 
#            y = NULL, 
#            title = paste0("Average ",colname,"\nby registered in Jordan governorates as place of asylum"),
#            #subtitle =  paste0("Based on registration data by UNHCR. Percentage of mapped cases: ",response.percentage.list[[n]],"%"),
#            caption = "Datasource:\nUNHCR proGres\nUNHCR Github repository 'p-codes'") + 
#       scale_fill_manual(
#         values = rev(magma(8, alpha = 0.8)[2:7]),
#         breaks = rev(brks.scale),
#         # name = paste0("Average of ",colname),
#         drop = FALSE,
#         labels = labels.scale,
#         guide = guide_legend(
#           byrow = T,
#           reverse = T,
#           label.position = "bottom"
#         )
#       )
#     p
#     
#     path <- paste0("out/maps/JOR/percent_facets/adm",n,"/JOR_ADM",n,"_avg_",colname,".png")
#     ggsave(path, p, width=10, height=10,units="in", dpi=300)
#   }
# }





# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
