source("code/0-packages.R")



###### LOAD DATA ###################################################################################
#load thematic data from file (please consider to run previous code-files (""1-Recode-data..."))
data.original <- read.csv("data/progrescase2.csv")
df_original <- data.original
# #vlookup to add iso3 codes to data according to existing pcodes
# country_codes <- read.csv("data/country_code_translation.csv", sep = ";")
# df_countries <- merge(df_original, country_codes, by.x = "CountryAsylum", by.y = "PCountryCode", all = TRUE)
#filter data for country

df_CountryAsylum <- sqldf("select * from df_original where CountryAsylum = 'JOR'")

#adminlevel1
data.country.adm1 <- sqldf("select * from df_CountryAsylum where coal1id LIKE 'JOR%'")
response_percentage <- round((nrow(data.country.adm1)/nrow(df_CountryAsylum))*100, 2)

#adminlevel2
data.country.adm2 <- sqldf("select * from df_CountryAsylum where coal2id LIKE 'JOR%'")
data.country.list <- list(data.country.adm1, data.country.adm2)

# test <- unique(data.country.adm2$coal2id) #backup check of existing data


#### DATA WHICH CAN BE PLOTTED IN ONE CHOROPLETH MAP ###############################################
##################################### ADMIN 1 ################################################
##where data is integer as average
##average age
data.asylum.Num_Inds <- aggregate(cbind(Num_Inds) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Child_0_14 <- aggregate(cbind(Child_0_14) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Youth_15_17 <- aggregate(cbind(Youth_15_17) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Work_15_64 <- aggregate(cbind(Work_15_64) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Eldern_65 <- aggregate(cbind(Eldern_65) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.AVG_Age <- aggregate(cbind(AVG_Age) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.STDEV_Age <- aggregate(cbind(STDEV_Age) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.average1 <- plyr::join_all(list(data.asylum.Num_Inds, data.asylum.Child_0_14, data.asylum.Youth_15_17, data.asylum.Work_15_64, data.asylum.Eldern_65, data.asylum.AVG_Age, data.asylum.STDEV_Age), by = 'coal1id', type = 'full')
colnames(df.average1)[1] <- "idprogres"
rm(data.asylum.Num_Inds, data.asylum.Eldern_65, data.asylum.Work_15_64, data.asylum.Youth_15_17, data.asylum.Child_0_14, data.asylum.AVG_Age, data.asylum.STDEV_Age)


###### LOAD GEOJSON AND PREPARE FOR JOIN WITH THEMATIC DATA #################
#rm(geojson)

geojsonurl1 <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM1.geojson")
destfilepath1 <- paste0("geo/geojson/JOR_ADM1.geojson" )
download.file( geojsonurl1, destfile=destfilepath1 )
json.raw1 <- geojson_read( destfilepath1, method="local", what="sp" )
#plot(json.raw)

# fortify, i.e., make ggplot2-compatible
json.raw1@data$id = rownames(json.raw1@data)
map.data.fortified <- fortify(json.raw1, region = "id")
map.data1 <- plyr::join(map.data.fortified, json.raw1@data, by="id") #joining dataframe with geojson-data
rm(map.data.fortified)

# now we join the thematic data
map.data.average1 <- plyr::join(x=map.data1, y=df.average1, by="idprogres")

#adjust length of data for loop in '3-Create-Map'
map.data.average1$brks <- 0
map.data.average1 <- map.data.average1[,c(ncol(map.data.average1),1:(ncol(map.data.average1)-1))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
df.average1$fill <- 0
df.average1 <- df.average1[,c(ncol(df.average1),1:(ncol(df.average1)-1))] ## do the same in df.choropleth to match with index





##################################### ADMIN 2 ################################################
##where data is integer as average
##average age
data.asylum.Num_Inds <- aggregate(cbind(Num_Inds) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.Child_0_14 <- aggregate(cbind(Child_0_14) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.Youth_15_17 <- aggregate(cbind(Youth_15_17) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.Work_15_64 <- aggregate(cbind(Work_15_64) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.Eldern_65 <- aggregate(cbind(Eldern_65) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.AVG_Age <- aggregate(cbind(AVG_Age) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)
data.asylum.STDEV_Age <- aggregate(cbind(STDEV_Age) ~ coal2id, data = data.country.list[[2]], FUN = mean, na.rm = TRUE)

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.average2 <- plyr::join_all(list(data.asylum.Num_Inds, data.asylum.Child_0_14, data.asylum.Youth_15_17, data.asylum.Work_15_64, data.asylum.Eldern_65, data.asylum.AVG_Age, data.asylum.STDEV_Age), by = 'coal2id', type = 'full')
colnames(df.average2)[1] <- "idadm2"
rm(data.asylum.Num_Inds, data.asylum.Eldern_65, data.asylum.Work_15_64, data.asylum.Youth_15_17, data.asylum.Child_0_14, data.asylum.AVG_Age, data.asylum.STDEV_Age)

#admin2
#rm(geojson)

geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM2.geojson")
destfilepath <- paste0("geo/geojson/JOR_ADM2.geojson" )
download.file( geojsonurl, destfile=destfilepath )
json.raw2 <- geojson_read( destfilepath, method="local", what="sp" )
#plot(json.raw)

# fortify, i.e., make ggplot2-compatible
json.raw2@data$id = rownames(json.raw2@data)
map.data.fortified <- fortify(json.raw2, region = "id")
map.data2 <- plyr::join(map.data.fortified, json.raw2@data, by="id") #joining dataframe with geojson-data
rm(map.data.fortified)

# now we join the thematic data
map.data.average2 <- plyr::join(x=map.data2, y=df.average2, by="idadm2", type="left")

#adjust length of data for loop in '3-Create-Map'
map.data.average2$brks <- 0
map.data.average2 <- map.data.average2[,c(ncol(map.data.average2),1:(ncol(map.data.average2)-1))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
df.average2$fill <- 0
df.average2 <- df.average2[,c(ncol(df.average2),1:(ncol(df.average2)-1))] ## do the same in df.choropleth to match with index



######## combine data of adm1 and adm2 in list that will be used in ggplot
list.df.average <- list(df.average1, df.average2)
list.map.data.average <- list(map.data.average1, map.data.average2)







##where data is factor 2 (yes/no) or (1/0) as ratio ################################################
###ADMINLEVEL1
#tortured
data.asylum.Torture <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(100))), coal1id ~ value  )
data.asylum.Torture$Torture.ratio <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
#legal protection needs
data.asylum.prot_needs <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(99))), coal1id ~ value  )
data.asylum.prot_needs$prot_needs.ratio <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
#single parent ratio
data.asylum.single_parent <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(98))), coal1id ~ value  )
data.asylum.single_parent$single_parent.ratio <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
#SGBV
data.asylum.SGBV <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(97))), coal1id ~ value  )
data.asylum.SGBV$SGBV.ratio <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
#serious medical condition
data.asylum.ser.medical.condition <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(96))), coal1id ~ value  )
data.asylum.ser.medical.condition$ser.medical.condition.ratio <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
#pregnant or lactating
data.asylum.pregnant.lactating <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(95))), coal1id ~ value  )
data.asylum.pregnant.lactating$pregnant.lactating.ratio <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
#older persons at risk
data.asylum.Older.risk <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(94))), coal1id ~ value  )
data.asylum.Older.risk$ser.medical.condition.ratio <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
#family unity
data.asylum.family.unity <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(93))), coal1id ~ value  )
data.asylum.family.unity$family.unity.ratio <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
#disability
data.asylum.disability <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(92))), coal1id ~ value  )
data.asylum.disability$disability.ratio <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
#child at risk
data.asylum.child.risk <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(91))), coal1id ~ value  )
data.asylum.child.risk$child.risk.ratio <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
#gender.female
data.asylum.gender.female <- dcast((melt(data.country.list[[1]], id=c(14), measure=c(90))), coal1id ~ value  )
data.asylum.gender.female$gender.female <- (data.asylum.gender.female$"1"/(data.asylum.gender.female$"1"+data.asylum.gender.female$"0"))*100

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.ratio1 <- plyr::join_all(list(data.asylum.Torture, data.asylum.prot_needs, data.asylum.single_parent,data.asylum.SGBV, data.asylum.ser.medical.condition, data.asylum.pregnant.lactating, data.asylum.Older.risk, data.asylum.family.unity, data.asylum.disability, data.asylum.child.risk, data.asylum.gender.female), by = 'coal1id', type = 'full')
df.ratio1[,c("yes","no", "1", "0")]<- NULL
colnames(df.ratio1)[1] <- "idprogres"
rm(data.asylum.Torture, data.asylum.prot_needs, data.asylum.single_parent,data.asylum.SGBV, data.asylum.ser.medical.condition, data.asylum.pregnant.lactating, data.asylum.Older.risk, data.asylum.family.unity, data.asylum.disability, data.asylum.child.risk, data.asylum.gender.female)

#join with geodata
map.data.ratio1 <- plyr::join(x=map.data1, y=df.ratio1, by="idprogres")
rm(map.data1)

#adjust length of data for loop in '3-Create-Map'
map.data.ratio1$brks <- 0
map.data.ratio1 <- map.data.ratio1[,c(ncol(map.data.ratio1),1:(ncol(map.data.ratio1)-1))] #reorder data so that column 'brks' is at 3, first comma means keep all the rows
df.ratio1$fill <- 0
df.ratio1 <- df.ratio1[,c(ncol(df.ratio1),1:(ncol(df.ratio1)-1))] ## do the same in df.choropleth to match with index





#### ADMINLEVEL 2 ###########################

#tortured
data.asylum.Torture <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(100))), coal2id ~ value  )
data.asylum.Torture$Torture.ratio <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
#legal protection needs
data.asylum.prot_needs <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(99))), coal2id ~ value  )
data.asylum.prot_needs$prot_needs.ratio <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
#single parent ratio
data.asylum.single_parent <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(98))), coal2id ~ value  )
data.asylum.single_parent$single_parent.ratio <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
#SGBV
data.asylum.SGBV <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(97))), coal2id ~ value  )
data.asylum.SGBV$SGBV.ratio <- (data.asylum.JOR.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.JOR.SGBV$no))*100
#serious medical condition
data.asylum.ser.medical.condition <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(96))), coal2id ~ value  )
data.asylum.ser.medical.condition$ser.medical.condition.ratio <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
#pregnant or lactating
data.asylum.pregnant.lactating <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(95))), coal2id ~ value  )
data.asylum.pregnant.lactating$pregnant.lactating.ratio <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
#older persons at risk
data.asylum.Older.risk <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(94))), coal2id ~ value  )
data.asylum.Older.risk$ser.medical.condition.ratio <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
#family unity
data.asylum.family.unity <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(93))), coal2id ~ value  )
data.asylum.family.unity$family.unity.ratio <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
#disability
data.asylum.disability <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(92))), coal2id ~ value  )
data.asylum.disability$disability.ratio <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
#child at risk
data.asylum.child.risk <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(91))), coal2id ~ value  )
data.asylum.child.risk$child.risk.ratio <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
#gender.female
data.asylum.gender.female <- dcast((melt(data.country.list[[2]], id=c(16), measure=c(90))), coal2id ~ value  )
data.asylum.gender.female$gender.female <- (data.asylum.gender.female$"1"/(data.asylum.gender.female$"1"+data.asylum.gender.female$"0"))*100

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.ratio2 <- plyr::join_all(list(data.asylum.Torture, data.asylum.prot_needs, data.asylum.single_parent,data.asylum.SGBV, data.asylum.ser.medical.condition, data.asylum.pregnant.lactating, data.asylum.Older.risk, data.asylum.family.unity, data.asylum.disability, data.asylum.child.risk, data.asylum.gender.female), by = 'coal2id', type = 'full')
df.ratio2[,c("yes","no", "1", "0")]<- NULL
colnames(df.ratio2)[1] <- "idadm2"
rm(data.asylum.Torture, data.asylum.prot_needs, data.asylum.single_parent,data.asylum.SGBV, data.asylum.ser.medical.condition, data.asylum.pregnant.lactating, data.asylum.Older.risk, data.asylum.family.unity, data.asylum.disability, data.asylum.child.risk, data.asylum.gender.female)

#join with geodata
map.data.ratio2 <- plyr::join(x=map.data2, y=df.ratio2, by="idadm2")
rm(map.data2)

#adjust length of data for loop in '3-Create-Map'
map.data.ratio2$brks <- 0
map.data.ratio2 <- map.data.ratio2[,c(ncol(map.data.ratio2),1:(ncol(map.data.ratio2)-1))] #reorder data so that column 'brks' is at 3, first comma means keep all the rows
df.ratio2$fill <- 0
df.ratio2 <- df.ratio2[,c(ncol(df.ratio2),1:(ncol(df.ratio2)-1))] ## do the same in df.choropleth to match with index




######## combine ratio data of adm1 and adm2 in list that will be used in ggplot
list.df.ratio <- list(df.ratio1, df.ratio2)
list.map.data.ratio <- list(map.data.ratio1, map.data.ratio2)



############################

admlevel1 <- 1
admlevel2 <- 2

list.admlevel <- list(admlevel1, admlevel2)





################################################################################################
##data with multiple factors


# #ref.status
# data.asylum.ref.status <- dcast((melt(data_country, id=c(14), measure=c(42))), coal1id ~ value  )
# #dem.ethn
# data.asylum.dem.ethn <- dcast((melt(data_country, id=c(14), measure=c(36))), coal1id ~ value  )
# #case size
# data.asylum.case.size <- dcast((melt(data_country, id=c(14), measure=c(45))), coal1id ~ value  )
# #dependency
# data.asylum.dependency <- dcast((melt(data_country, id=c(14), measure=c(46))), coal1id ~ value  )
# #youthdependency
# data.asylum.youthdependency <- dcast((melt(data_country, id=c(14), measure=c(47))), coal1id ~ value  )
# #elderndependency
# data.asylum.elderndependency <- dcast((melt(data_country, id=c(14), measure=c(48))), coal1id ~ value  )
# #season of arrival
# data.asylum.season <- dcast((melt(data_country, id=c(14), measure=c(69))), coal1id ~ value  )
# #highest education
# data.asylum.education <- dcast((melt(data_country, id=c(14), measure=c(71))), coal1id ~ value  )
# #occupation
# data.asylum.occupation <- dcast((melt(data_country, id=c(14), measure=c(77))), coal1id ~ value  )
# #marriage category
# data.asylum.marragecat <- dcast((melt(data_country, id=c(14), measure=c(78))), coal1id ~ value  )
# data.asylum.marragecat[2] <- NULL
# 
# list.multifactors <- list(data.asylum.ref.status, data.asylum.dem.ethn, data.asylum.case.size, data.asylum.dependency, data.asylum.youthdependency, data.asylum.elderndependency, data.asylum.season, data.asylum.education, data.asylum.occupation, data.asylum.marragecat)
# 
# # df.multfactors <- plyr::join_all(data.asylum.ref.status, data.asylum.dem.ethn, data.asylum.case.size, data.asylum.dependency, data.asylum.youthdependency, data.asylum.elderndependency, data.asylum.season, data.asylum.education, data.asylum.occupation, data.asylum.marragecat), by = 'coal1id', type = 'full')
# rm(data.asylum.ref.status, data.asylum.dem.ethn, data.asylum.case.size, data.asylum.dependency, data.asylum.youthdependency, data.asylum.elderndependency, data.asylum.season, data.asylum.education, data.asylum.occupation, data.asylum.marragecat)
#
##join with geodata
#map.data.multfactor <- plyr::join(x=map.data, y=df.multfactor, by="idprogres")














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
      panel.grid.major = element_line(color = "#f5f5f2", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}



