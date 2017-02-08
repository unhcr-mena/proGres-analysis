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

#adminlevel2
data.country.adm2 <- sqldf("select * from df_CountryAsylum where coal2id LIKE 'JOR%'")
data.country.list <- list(data.country.adm1, data.country.adm2)

response_percentage_adm1 <- round((nrow(data.country.list[[1]])/nrow(df_CountryAsylum))*100, 2)
response_percentage_adm2 <- round((nrow(data.country.list[[2]])/nrow(df_CountryAsylum))*100, 2)
response.percentage.list <- list(response_percentage_adm1, response_percentage_adm2)

# test <- unique(data.country.adm2$coal2id) #backup check of existing data


#### DATA WHICH CAN BE PLOTTED IN ONE CHOROPLETH MAP ###############################################
##################################### ADMIN 1 ################################################
##where data is integer as average
##average age
data.asylum.Num_Inds <- aggregate(cbind(Num_Inds) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Child_0_14 <- aggregate(cbind(Child_0_14) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)
data.asylum.Youth_15_17 <- aggregate(cbind(Youth_15_17) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = FALSE)
data.asylum.Work_15_64 <- aggregate(cbind(Work_15_64) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = FALSE)
data.asylum.Eldern_65 <- aggregate(cbind(Eldern_65) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = FALSE)
data.asylum.AVG_Age <- aggregate(cbind(AVG_Age) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = FALSE)
data.asylum.STDEV_Age <- aggregate(cbind(STDEV_Age) ~ coal1id, data = data.country.list[[1]], FUN = mean, na.rm = TRUE)

#join all ratios and avergages to one list for mapping purpose and delete variables as don't needed
df.average1 <- plyr::join_all(list(data.asylum.Num_Inds, data.asylum.Child_0_14, data.asylum.Youth_15_17, data.asylum.Work_15_64, data.asylum.Eldern_65, data.asylum.AVG_Age, data.asylum.STDEV_Age), by = 'coal1id', type = 'full')
colnames(df.average1)[1] <- "idprogres"
rm(data.asylum.Num_Inds, data.asylum.Eldern_65, data.asylum.Work_15_64, data.asylum.Youth_15_17, data.asylum.Child_0_14, data.asylum.AVG_Age, data.asylum.STDEV_Age)


###### LOAD GEOJSON AND PREPARE FOR JOIN WITH THEMATIC DATA #################
#rm(geojson)

geojsonurl1 <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM1.geojson")
# 
# dir.create(file.path(mainDir, subDir))
destfilepath1 <- paste0("geo/geojson/JOR_ADM1.geojson" )
download.file( geojsonurl1, destfile=destfilepath1 )
json.raw1 <- geojson_read( destfilepath1, method="local", what="sp" )
#plot(json.raw)

# fortify, i.e., make ggplot2-compatible
json.raw1@data$id = rownames(json.raw1@data)
map.data.fortified <- fortify(json.raw1, region = "id")
map.data1 <- plyr::join(map.data.fortified, json.raw1@data, by="id") #joining dataframe with geojson-data
map.data1 <- map.data1[, c("long", "lat", "order", "id", "group", "gid", "name", "iso3", "idprogres")]
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
map.data2 <- map.data2[, c("long", "lat", "order", "id", "group", "admin_unhcr2_fid", "adm2name", "idcountry", "idadm2")]

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


#adjust length of data for loop in '3-Create-Map'
map.data.ratio1$brks <- 0
map.data.ratio1 <- map.data.ratio1[,c(ncol(map.data.ratio1),1:(ncol(map.data.ratio1)-1))] #reorder data so that column 'brks' is at 3, first comma means keep all the rows
df.ratio1$fill <- 0
df.ratio1 <- df.ratio1[,c(ncol(df.ratio1),1:(ncol(df.ratio1)-1))] ## do the same in df.choropleth to match with index





#### ADMINLEVEL 2 ###############################

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
data.asylum.SGBV$SGBV.ratio <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
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

#adjust length of data for loop in '3-Create-Map'
map.data.ratio2$brks <- 0
map.data.ratio2 <- map.data.ratio2[,c(ncol(map.data.ratio2),1:(ncol(map.data.ratio2)-1))] #reorder data so that column 'brks' is at 3, first comma means keep all the rows
df.ratio2$fill <- 0
df.ratio2 <- df.ratio2[,c(ncol(df.ratio2),1:(ncol(df.ratio2)-1))] ## do the same in df.choropleth to match with index





######## combine ratio data of adm1 and adm2 in list that will be used in ggplot
list.df.ratio <- list(df.ratio1, df.ratio2)
list.map.data.ratio <- list(map.data.ratio1, map.data.ratio2)




# computation of labels for variables --> used in title of maps
df.labels.average <- data.frame(x = (attributes(list.map.data.average[[1]]))[1], y = c( "brks", "long", "lat", "order", "id", "group", "gid", "name", "iso3", "idprogres",
                                                                                        "number of individuals per case", "number of children in the age 0-14", "number of youth in the age of 15-17", "number of persons in working age 15-65", "number of persons with age > 65", "age of individuals", "standard deviation of age" )) 

df.labels.ratio <- data.frame(x = (attributes(list.map.data.ratio[[1]]))[1], y = c( "brks", "long", "lat", "order", "id", "group", "gid", "name", "iso3", "idprogres",      
                                                                                        "tortured persons", "persons with serious legal protection needs", "single parent households", "persons registered as suffered by SGBV", "persons in a serious medical condition", "pregnant or lactating women", "family unities", "disabled persons", "childs at risk", "female"))








################################################################################################
##data with multiple factors
#calculation of row-wise ratios


#ref.status
data.asylum.ref.status <- dcast((melt(data.country.adm1, id=c(14), measure=c(42))), coal1id ~ value  )
ratio.asylum.ref.status <- cbind(idprogres = data.asylum.ref.status[, 1], data.asylum.ref.status[, -1]/rowSums(data.asylum.ref.status[, -1])*100)
map.ratio.asylum.ref.status <- plyr::join(x=map.data1, y=ratio.asylum.ref.status, by="idprogres")
#dem.ethn
data.asylum.dem.ethn <- dcast((melt(data.country.adm1, id=c(14), measure=c(36))), coal1id ~ value  )
ratio.asylum.dem.ethn <- cbind(idprogres = data.asylum.dem.ethn[, 1], data.asylum.dem.ethn[, -1]/rowSums(data.asylum.dem.ethn[, -1])*100)
map.ratio.asylum.dem.ethn <- plyr::join(x=map.data1, y=ratio.asylum.dem.ethn, by="idprogres")
#case size
data.asylum.case.size <- dcast((melt(data.country.adm1, id=c(14), measure=c(45))), coal1id ~ value)
ratio.asylum.case.size <- cbind(idprogres = data.asylum.case.size[, 1], data.asylum.case.size[, -1]/rowSums(data.asylum.case.size[, -1])*100)
map.ratio.asylum.case.size <- plyr::join(x=map.data1, y=ratio.asylum.case.size, by="idprogres")
#dependency
data.asylum.dependency <- dcast((melt(data.country.adm1, id=c(14), measure=c(46))), coal1id ~ value)
ratio.asylum.dependency <- cbind(idprogres = data.asylum.dependency[, 1], data.asylum.dependency[, -1]/rowSums(data.asylum.dependency[, -1])*100)
map.ratio.asylum.dependency <- plyr::join(x=map.data1, y=ratio.asylum.dependency, by="idprogres")
#youthdependency
data.asylum.youthdependency <- dcast((melt(data.country.adm1, id=c(14), measure=c(47))), coal1id ~ value)
ratio.asylum.youthdependency <- cbind(idprogres = data.asylum.youthdependency[, 1], data.asylum.youthdependency[, -1]/rowSums(data.asylum.youthdependency[, -1])*100)
map.ratio.asylum.youthdependency <- plyr::join(x=map.data1, y=ratio.asylum.youthdependency, by="idprogres")
#elderndependency
data.asylum.elderndependency <- dcast((melt(data.country.adm1, id=c(14), measure=c(48))), coal1id ~ value  )
ratio.asylum.elderndependency <- cbind(idprogres = data.asylum.elderndependency[, 1], data.asylum.elderndependency[, -1]/rowSums(data.asylum.elderndependency[, -1])*100)
map.ratio.asylum.elderndependency <- plyr::join(x=map.data1, y=ratio.asylum.elderndependency, by="idprogres")
#season of arrival
data.asylum.season <- dcast((melt(data.country.adm1, id=c(14), measure=c(69))), coal1id ~ value  )
ratio.asylum.season <- cbind(idprogres = data.asylum.season[, 1], data.asylum.season[, -1]/rowSums(data.asylum.season[, -1])*100)
map.ratio.asylum.season <- plyr::join(x=map.data1, y=ratio.asylum.season, by="idprogres")
#highest education
data.asylum.education <- dcast((melt(data.country.adm1, id=c(14), measure=c(71))), coal1id ~ value  )
data.asylum.education$Var.2 <- NULL
ratio.asylum.education <- cbind(idprogres = data.asylum.education[, 1], data.asylum.education[, -1]/rowSums(data.asylum.education[, -1])*100)
map.ratio.asylum.education <- plyr::join(x=map.data1, y=ratio.asylum.education, by="idprogres")
#occupation
data.asylum.occupation <- dcast((melt(data.country.adm1, id=c(14), measure=c(77))), coal1id ~ value  )
ratio.asylum.occupation <- cbind(idprogres = data.asylum.occupation[, 1], data.asylum.occupation[, -1]/rowSums(data.asylum.occupation[, -1])*100)
map.ratio.asylum.occupation <- plyr::join(x=map.data1, y=ratio.asylum.occupation, by="idprogres")

#marriage category
data.asylum.marragecat <- dcast((melt(data.country.adm1, id=c(14), measure=c(78))), coal1id ~ value  )
data.asylum.marragecat[2] <- NULL
ratio.asylum.marragecat <- cbind(idprogres = data.asylum.marragecat[, 1], data.asylum.marragecat[, -1]/rowSums(data.asylum.marragecat[, -1])*100)
map.ratio.asylum.marragecat <- plyr::join(x=map.data1, y=ratio.asylum.marragecat, by="idprogres")

df.multifactor.ratio <- list(data.asylum.ref.status, data.asylum.dem.ethn, data.asylum.case.size, data.asylum.dependency, data.asylum.youthdependency, data.asylum.elderndependency, data.asylum.season, data.asylum.education, data.asylum.occupation, data.asylum.marragecat)
rm(data.asylum.ref.status, data.asylum.dem.ethn, data.asylum.case.size, data.asylum.dependency, data.asylum.youthdependency, data.asylum.elderndependency, data.asylum.season, data.asylum.education, data.asylum.occupation, data.asylum.marragecat)

list.multifactor.ratio <- list(map.ratio.asylum.ref.status, map.ratio.asylum.dem.ethn, map.ratio.asylum.case.size, map.ratio.asylum.dependency, map.ratio.asylum.youthdependency, map.ratio.asylum.elderndependency, map.ratio.asylum.season, map.ratio.asylum.education, map.ratio.asylum.occupation, map.ratio.asylum.marragecat)
rm(ratio.asylum.ref.status, ratio.asylum.dem.ethn, ratio.asylum.case.size, ratio.asylum.dependency, ratio.asylum.youthdependency, ratio.asylum.elderndependency, ratio.asylum.season, ratio.asylum.education, ratio.asylum.occupation, ratio.asylum.marragecat)
rm(map.ratio.asylum.ref.status, map.ratio.asylum.dem.ethn, map.ratio.asylum.case.size, map.ratio.asylum.dependency, map.ratio.asylum.youthdependency, map.ratio.asylum.elderndependency, map.ratio.asylum.season, map.ratio.asylum.education, map.ratio.asylum.occupation, map.ratio.asylum.marragecat)




#styling theme for maps
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
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.spacing = unit(c(-.1,0.2,0.02,0.2), "cm"),
      panel.border = element_blank(),
      
      
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      plot.title = element_text(hjust = 0, color = "#4e4d47", size = 22),
      plot.subtitle = element_text(hjust = 0, color = "#4e4d47", size = 12, debug = F),
      plot.margin = unit(c(.5,.5,1,.5), "cm"),
      plot.caption = element_text(size = 10, hjust = 1, color = "#939184"),
      

      # legend.border = element_blank(),
      legend.direction = "horizontal",
      legend.position = "bottom", #c(0.5, 0.01),
      legend.text.align = 1,                                                 #postition of label: 0=left, 1=right
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.text = element_text(size = 14, hjust = 0, color = "#4e4d47"),
      legend.margin = unit(c(1,.5,0.2,.5), "cm"),
      legend.key.height = unit(4, units = "mm"),                             #height of legend
      legend.key.width = unit(100/length(labels), units = "mm"),             #width of legend
      legend.title = element_text(size = 0),                                 #put to 0 to remove title but keep space
      



      ...
    )
}



