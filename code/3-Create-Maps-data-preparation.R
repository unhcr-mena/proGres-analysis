source("code/0-packages.R")



###### DECLARATION OF STATIC VARIABLES & DATAFRAMES ####################################################
#load data from file (please consider to run previous code-files (""1-Recode-data..."))
data.original <- read.csv("data/progrescase2.csv")
df_original <- data.original
df_CountryAsylum <- sqldf("select * from df_original where CountryAsylum = 'JOR'")
data_country <- sqldf("select * from df_CountryAsylum where coal1id LIKE 'JOR%'")
response_percentage <- round((nrow(data_country)/nrow(df_CountryAsylum))*100, 2)



##data where factor is two as ratio


# for (k in 79:100){
#   varname <- colnames(data_country[k])
#   df <- dcast((melt(data_country, id=c(13), measure.vars=data_country[[k]])), coal1 ~ value  )
#   df$ratio <- (df$yes/(df$yes+df$no))*100
#   colnames(df[1]) <- "name"
#   path <- paste0("data/out/progrescase2.",varname,".ratio.csv")
#   write.csv(df, path,na="")
#   results<-rbind(df)   
#   
# }

##where data is integer
##average age
data.asylum.JOR.Num_Inds <- aggregate(cbind(Num_Inds) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.Child_0_14 <- aggregate(cbind(Child_0_14) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.Youth_15_17 <- aggregate(cbind(Youth_15_17) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.Work_15_64 <- aggregate(cbind(Work_15_64) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.Eldern_65 <- aggregate(cbind(Eldern_65) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.AVG_Age <- aggregate(cbind(AVG_Age) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)
data.asylum.JOR.STDEV_Age <- aggregate(cbind(STDEV_Age) ~ coal1id + coal1 , data = data_country, FUN = mean, na.rm = TRUE)


##where data is factor 2 (yes/no) or (1/0)
#tortured
data.asylum.JOR.Torture <- dcast((melt(data_country, id=c(14), measure=c(100))), coal1id ~ value  )
data.asylum.JOR.Torture$Torture.ratio <- (data.asylum.JOR.Torture$yes/(data.asylum.JOR.Torture$yes+data.asylum.JOR.Torture$no))*100
#legal protection needs
data.asylum.JOR.prot_needs <- dcast((melt(data_country, id=c(14), measure=c(99))), coal1id ~ value  )
data.asylum.JOR.prot_needs$prot_needs.ratio <- (data.asylum.JOR.prot_needs$yes/(data.asylum.JOR.prot_needs$yes+data.asylum.JOR.prot_needs$no))*100
#single parent ratio
data.asylum.JOR.single_parent <- dcast((melt(data_country, id=c(14), measure=c(98))), coal1id ~ value  )
data.asylum.JOR.single_parent$single_parent.ratio <- (data.asylum.JOR.single_parent$yes/(data.asylum.JOR.single_parent$yes+data.asylum.JOR.single_parent$no))*100
#SGBV
data.asylum.JOR.SGBV <- dcast((melt(data_country, id=c(14), measure=c(97))), coal1id ~ value  )
data.asylum.JOR.SGBV$SGBV.ratio <- (data.asylum.JOR.SGBV$yes/(data.asylum.JOR.SGBV$yes+data.asylum.JOR.SGBV$no))*100
#serious medical condition
data.asylum.JOR.ser.medical.condition <- dcast((melt(data_country, id=c(14), measure=c(96))), coal1id ~ value  )
data.asylum.JOR.ser.medical.condition$ser.medical.condition.ratio <- (data.asylum.JOR.ser.medical.condition$yes/(data.asylum.JOR.ser.medical.condition$yes+data.asylum.JOR.ser.medical.condition$no))*100
#pregnant or lactating
data.asylum.JOR.pregnant.lactating <- dcast((melt(data_country, id=c(14), measure=c(95))), coal1id ~ value  )
data.asylum.JOR.pregnant.lactating$pregnant.lactating.ratio <- (data.asylum.JOR.pregnant.lactating$yes/(data.asylum.JOR.pregnant.lactating$yes+data.asylum.JOR.pregnant.lactating$no))*100
#older persons at risk
data.asylum.JOR.Older.risk <- dcast((melt(data_country, id=c(14), measure=c(94))), coal1id ~ value  )
data.asylum.JOR.Older.risk$ser.medical.condition.ratio <- (data.asylum.JOR.Older.risk$yes/(data.asylum.JOR.Older.risk$yes+data.asylum.JOR.Older.risk$no))*100
#family unity
data.asylum.JOR.family.unity <- dcast((melt(data_country, id=c(14), measure=c(93))), coal1id ~ value  )
data.asylum.JOR.family.unity$family.unity.ratio <- (data.asylum.JOR.family.unity$yes/(data.asylum.JOR.family.unity$yes+data.asylum.JOR.family.unity$no))*100
#disability
data.asylum.JOR.disability <- dcast((melt(data_country, id=c(14), measure=c(92))), coal1id ~ value  )
data.asylum.JOR.disability$disability.ratio <- (data.asylum.JOR.disability$yes/(data.asylum.JOR.disability$yes+data.asylum.JOR.disability$no))*100
#child at risk
data.asylum.JOR.child.risk <- dcast((melt(data_country, id=c(14), measure=c(91))), coal1id ~ value  )
data.asylum.JOR.child.risk$child.risk.ratio <- (data.asylum.JOR.child.risk$yes/(data.asylum.JOR.child.risk$yes+data.asylum.JOR.child.risk$no))*100
#gender.female
data.asylum.JOR.gender.female <- dcast((melt(data_country, id=c(14), measure=c(90))), coal1id ~ value  )
data.asylum.JOR.gender.female$gender.female <- (data.asylum.JOR.gender.female$"1"/(data.asylum.JOR.gender.female$"1"+data.asylum.JOR.gender.female$"0"))*100

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.choropleth <- plyr::join_all(list(data.asylum.JOR.Num_Inds, data.asylum.JOR.Child_0_14, data.asylum.JOR.Youth_15_17, data.asylum.JOR.Work_15_64, data.asylum.JOR.Eldern_65, data.asylum.JOR.AVG_Age, data.asylum.JOR.STDEV_Age, data.asylum.JOR.Num_Inds, data.asylum.JOR.Torture, data.asylum.JOR.prot_needs, data.asylum.JOR.single_parent,data.asylum.JOR.SGBV, data.asylum.JOR.ser.medical.condition, data.asylum.JOR.pregnant.lactating, data.asylum.JOR.Older.risk, data.asylum.JOR.family.unity, data.asylum.JOR.disability, data.asylum.JOR.child.risk, data.asylum.JOR.gender.female), by = 'coal1id', type = 'full')
df.choropleth[,c("yes","no", "1", "0")]<- NULL
colnames(df.choropleth)[1] <- "idprogres"
rm(data.asylum.JOR.Num_Inds, data.asylum.JOR.Eldern_65, data.asylum.JOR.Work_15_64, data.asylum.JOR.Youth_15_17, data.asylum.JOR.Child_0_14, data.asylum.JOR.AVG_Age, data.asylum.JOR.STDEV_Age, data.asylum.JOR.Torture, data.asylum.JOR.prot_needs, data.asylum.JOR.single_parent,data.asylum.JOR.SGBV, data.asylum.JOR.ser.medical.condition, data.asylum.JOR.pregnant.lactating, data.asylum.JOR.Older.risk, data.asylum.JOR.family.unity, data.asylum.JOR.disability, data.asylum.JOR.child.risk, data.asylum.JOR.gender.female)





# ##Multiple factors
# #ref.status
# data.asylum.JOR.ref.status <- dcast((melt(data_country, id=c(13), measure=c(42))), coal1 ~ value  )
# #dem.ethn
# data.asylum.JOR.dem.ethn <- dcast((melt(data_country, id=c(13), measure=c(36))), coal1 ~ value  )
# #case size
# data.asylum.JOR.case.size <- dcast((melt(data_country, id=c(13), measure=c(45))), coal1 ~ value  )
# #dependency
# data.asylum.JOR.dependency <- dcast((melt(data_country, id=c(13), measure=c(46))), coal1 ~ value  )
# #youthdependency
# data.asylum.JOR.youthdependency <- dcast((melt(data_country, id=c(13), measure=c(47))), coal1 ~ value  )
# #elderndependency
# data.asylum.JOR.elderndependency <- dcast((melt(data_country, id=c(13), measure=c(48))), coal1 ~ value  )
# #season of arrival
# data.asylum.JOR.season <- dcast((melt(data_country, id=c(13), measure=c(69))), coal1 ~ value  )
# #highest education
# data.asylum.JOR.education <- dcast((melt(data_country, id=c(13), measure=c(71))), coal1 ~ value  )
# #occupation
# data.asylum.JOR.occupation <- dcast((melt(data_country, id=c(13), measure=c(77))), coal1 ~ value  )
# #marriage category
# data.asylum.JOR.marragecat <- dcast((melt(data_country, id=c(13), measure=c(78))), coal1 ~ value  )
# data.asylum.JOR.marragecat[2] <- NULL
# 
# 
# 
# 
# df.multfactors <- plyr::join_all(list(data.asylum.JOR.ref.status, data.asylum.JOR.dem.ethn, data.asylum.JOR.case.size, data.asylum.JOR.dependency, data.asylum.JOR.youthdependency, data.asylum.JOR.elderndependency, data.asylum.JOR.season, data.asylum.JOR.education, data.asylum.JOR.occupation, data.asylum.JOR.marragecat), by = 'coal1', type = 'full')
# df.multfactors[2:3]<- NULL



###read geojson from github repository 'p-codes'
#rm(geojson)
admlevel = 1
geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM",admlevel,".geojson")
destfilepath <- paste0("geo/geojson/JOR_ADM1.geojson" )
download.file( geojsonurl, destfile=destfilepath )
json.raw <- geojson_read( destfilepath, method="local", what="sp" )
#plot(json.raw)


# fortify, i.e., make ggplot2-compatible
json.raw@data$id = rownames(json.raw@data)
map_data_fortified <- fortify(json.raw, region = "id")
map_data <- plyr::join(map_data_fortified, json.raw@data, by="id") #joining dataframe with geojson-data

# now we join the thematic data
map_data1 <- plyr::join(x=map_data, y=df.choropleth, by="idprogres") 
# map_data2 <- plyr::join(x=map_data, y=df.choropleth, by="idprogres")


# # dendrogramm to see number of classes depending on existent age classes
# hc = hclust(dist(data.asylum.JOR.AVG_Age$AVG_Age))
# # very simple dendrogram
# plot(hc)
# 
# 
# # histogramm to see distribution of age classes
# ggplot(data.asylum.JOR.AVG_Age, aes(x=AVG_Age)) +
#   geom_histogram(binwidth=.5, colour="black", fill="white") +
# xlab("Average age of refugees by Governorate of Asylum") +
#   ylab("Count")



# theme_map <- function(...) {
#   theme_minimal() +
#     theme(
#       text = element_text(family = "Ubuntu Regular", color = "#22211d"),
#       
#       # plot.title = element_text(size = 14),
#       # plot.subtitle = element_text(size = 10),
#       plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
#       plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47", debug = F),
#       plot.margin = unit(c(.5,.5,1.2,.5), "cm"),
#       plot.caption = element_text(size = 6, hjust = 0.92, color = "#939184"),
#       
#       axis.line = element_blank(),
#       axis.text.x = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank(),
#       axis.title.x = element_blank(),
#       axis.title.y = element_blank(),
#       
#       # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
#       panel.grid.major = element_line(color = "#fcfcfc", size = 0.2),
#       panel.grid.minor = element_blank(),
#       plot.background = element_rect(fill = "#fcfcfc", color = NA), 
#       panel.background = element_rect(fill = "#fcfcfc", color = NA), 
#       panel.border = element_blank(),
#       panel.spacing = unit(c(-.5,0.2,.2,0.2), "cm"),
# 
#       # legend.background = element_rect(fill = "#fcfcfc", color = NA),
#       legend.title = element_text(size = 8),
#       legend.spacing = unit(c(-.5,0.2,.2,0.2), "cm"),
#       legend.position = c(0.5, 0.03),
#       legend.text.align = 0,
#       legend.background = element_rect(fill = alpha('white', 0.0)),
#       legend.text = element_text(size = 7, hjust = 0, color = "#4e4d47"),
#       ...
#     )
# }


theme_map <- function(...) {
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
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}