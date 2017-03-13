## clear workspace except necessary aggregation function
keep(summarySE, sure = TRUE)

##############################
## load prepared data
progrescase.processed <- read.csv("data/progrescase_maps.csv")
data.progrescase.specificneed <- progrescase.processed



### extra df for numerical data -> remove rows without values -> count length for each var -> caluculate conf.interval for map 
# use other df for other data


# ## unique country codes within data, necessary for looping all countries in the end
# country.values <- data.frame(unique(data.progrescase.specificneed$CountryAsylum))

##############################
### Subset for country 
df_CountryAsylum <- sqldf("select * from 'data.progrescase.specificneed' where CountryAsylum == 'JOR'")
## just select rows where country code AND admin level code are matching to remove inconsistent data
data.country.adm1 <- sqldf("select * from 'data.progrescase.specificneed' where CountryAsylum == 'JOR' AND coal1id LIKE 'JOR%'")
data.country.adm2 <- sqldf("select * from 'data.progrescase.specificneed' where CountryAsylum == 'JOR' AND coal1id LIKE 'JOR%' AND coal2id LIKE 'JOR%'")
# let's check how much percent of data is consistent within this country
pct.consistent.data.adm1 <- round((nrow(data.country.adm1)/nrow(df_CountryAsylum)), digits = 2) # if nrow country codes is considered as total 
pct.consistent.data.adm2 <- round((nrow(data.country.adm2)/nrow(df_CountryAsylum)), digits = 2) # if nrow country codes is considered as total 


## change column names of key columns for successful join with geojson
names(data.country.adm1)[names(data.country.adm1) == 'coal1id'] <- 'idprogres'
names(data.country.adm2)[names(data.country.adm2) == 'coal2id'] <- 'idadm2'


##############################
# data processing steps on country level (e.g. ratio where 100% should be total within country not of all data)
##############################

##############################
## Calculating dependency percentage within country (100% should be current country subset not adm. unit or all countries)
# child+elderly, # child, # elderly #single female already existent
# adm1
data.country.adm1$dependency <- (data.country.adm1$Child_0_14+data.country.adm1$Eldern_65) / (data.country.adm1$Work_15_64+data.country.adm1$Child_0_14+data.country.adm1$Eldern_65)
data.country.adm1$youthdependency <- data.country.adm1$Child_0_14 / (data.country.adm1$Work_15_64+data.country.adm1$Child_0_14+data.country.adm1$Eldern_65)
data.country.adm1$elederndependency <- data.country.adm1$Eldern_65 / (data.country.adm1$Work_15_64+data.country.adm1$Child_0_14+data.country.adm1$Eldern_65)
# adm2
data.country.adm2$dependency <- (data.country.adm2$Child_0_14+data.country.adm2$Eldern_65) / (data.country.adm2$Work_15_64+data.country.adm2$Child_0_14+data.country.adm2$Eldern_65)
data.country.adm2$youthdependency <- data.country.adm2$Child_0_14 / (data.country.adm2$Work_15_64+data.country.adm2$Child_0_14+data.country.adm2$Eldern_65)
data.country.adm2$elederndependency <- data.country.adm2$Eldern_65 / (data.country.adm2$Work_15_64+data.country.adm2$Child_0_14+data.country.adm2$Eldern_65)


##############################
## Individual subsets for some variables to loose as less data as possible
## remove observation where we do not have gender data
data.country.adm1.sex <- data.country.adm1[data.country.adm1$dem_sex !=c("Unknown","U","f","m","-"), ]
data.country.adm2.sex <- data.country.adm2[data.country.adm2$dem_sex !=c("Unknown","U","f","m","-"), ]


##############################
## store data of adm1 and adm2 together in one list
data.country.list <- list(data.country.adm1, data.country.adm2)
#rm(data.country.adm1, data.country.adm2)


## aggregation by admin level 1 code of asylum ################
adm1.Num_Inds <- summarySE(data.country.list[[1]], measurevar="Num_Inds", groupvars=c("idprogres"), na.rm=TRUE)
adm1.Child_0_14 <- summarySE(data.country.list[[1]], measurevar="Child_0_14", groupvars=c("idprogres"), na.rm=TRUE)
adm1.Youth_15_17 <- summarySE(data.country.list[[1]], measurevar="Youth_15_17", groupvars=c("idprogres"), na.rm=TRUE)
adm1.Work_15_64 <- summarySE(data.country.list[[1]], measurevar="Work_15_64", groupvars=c("idprogres"), na.rm=TRUE)
adm1.Eldern_65 <- summarySE(data.country.list[[1]], measurevar="Eldern_65", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.STDEV_Age <- summarySE(data.country.list[[1]], measurevar="STDEV_Age", groupvars=c("idprogres"), na.rm=TRUE)
adm1.head.dem_age <- summarySE(data.country.list[[1]], measurevar="dem_age", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.female <- summarySE(data.country.list[[1]], measurevar="Female", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.male <- summarySE(data.country.list[[1]], measurevar="Male", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.only.female <- summarySE(data.country.list[[1]], measurevar="only.female.case", groupvars=c("idprogres"), na.rm=TRUE) 
adm1.case.only.male <- summarySE(data.country.list[[1]], measurevar="only.male.case", groupvars=c("idprogres"), na.rm=TRUE) 
adm1.case.dependency <- summarySE(data.country.list[[1]], measurevar="dependency", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.youthdependency <- summarySE(data.country.list[[1]], measurevar="youthdependency", groupvars=c("idprogres"), na.rm=TRUE)
adm1.case.elederndependency <- summarySE(data.country.list[[1]], measurevar="elederndependency", groupvars=c("idprogres"), na.rm=TRUE)
# special case where other data is used: column 'female.headed' is based on column 'dem_sex' which contains invalid values
# therefore it is for this particular case necessary to use a special cleaned data 
adm1.case.female.headed <- summarySE(data.country.adm1.sex, measurevar="female.headed", groupvars=c("idprogres"), na.rm=TRUE) 

#join all processed variables to one list for mapping purpose and delete variables for better variable management
list.adm1 <- list(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.case.STDEV_Age, adm1.head.dem_age, adm1.case.female, adm1.case.male, adm1.case.only.female, adm1.case.only.male, adm1.case.female.headed, adm1.case.dependency, adm1.case.youthdependency, adm1.case.elederndependency)
rm(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.case.STDEV_Age, adm1.head.dem_age, adm1.case.female, adm1.case.male, adm1.case.only.female, adm1.case.only.male, adm1.case.female.headed, adm1.case.dependency, adm1.case.youthdependency, adm1.case.elederndependency)



### aggregation by admin level 2 code of asylum ################
adm2.Num_Inds <- summarySE(data.country.list[[2]], measurevar="Num_Inds", groupvars=c("idadm2"), na.rm=TRUE)
adm2.Child_0_14 <- summarySE(data.country.list[[2]], measurevar="Child_0_14", groupvars=c("idadm2"), na.rm=TRUE)
adm2.Youth_15_17 <- summarySE(data.country.list[[2]], measurevar="Youth_15_17", groupvars=c("idadm2"), na.rm=TRUE)
adm2.Work_15_64 <- summarySE(data.country.list[[2]], measurevar="Work_15_64", groupvars=c("idadm2"), na.rm=TRUE)
adm2.Eldern_65 <- summarySE(data.country.list[[2]], measurevar="Eldern_65", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.STDEV_Age <- summarySE(data.country.list[[2]], measurevar="STDEV_Age", groupvars=c("idadm2"), na.rm=TRUE)
adm2.head.dem_age <- summarySE(data.country.list[[2]], measurevar="dem_age", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.female <- summarySE(data.country.list[[2]], measurevar="Female", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.male <- summarySE(data.country.list[[2]], measurevar="Male", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.only.female <- summarySE(data.country.list[[2]], measurevar="only.female.case", groupvars=c("idadm2"), na.rm=TRUE) 
adm2.case.only.male <- summarySE(data.country.list[[2]], measurevar="only.male.case", groupvars=c("idadm2"), na.rm=TRUE) 
adm2.case.dependency <- summarySE(data.country.list[[2]], measurevar="dependency", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.youthdependency <- summarySE(data.country.list[[2]], measurevar="youthdependency", groupvars=c("idadm2"), na.rm=TRUE)
adm2.case.elederndependency <- summarySE(data.country.list[[2]], measurevar="elederndependency", groupvars=c("idadm2"), na.rm=TRUE)
# special case where other data is used: column 'female.headed' is based on column 'dem_sex' which contains invalid values
# therefore it is for this particular case necessary to use a special cleaned data 
adm2.case.female.headed <- summarySE(data.country.adm2.sex, measurevar="female.headed", groupvars=c("idadm2"), na.rm=TRUE) 

#join all processed variables to one list for mapping purpose and delete variables for better variable management
list.adm2 <- list(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)
rm(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)


###### LOAD GEOJSON AND PREPARE FOR JOIN WITH THEMATIC DATA #################
#rm(geojson)


# for (i in 1:2) {

geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM1.geojson")

# create output folder
mainDir <- "out"
subDir <- "/geo/geojson"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
filename <- "JOR_ADM1.geojson"
destfilepath <- paste0("out/geo/geojson/", filename )
download.file( geojsonurl, destfile=destfilepath )
json.raw <- geojson_read( destfilepath, method="local", what="sp" )
#plot(json.raw)

proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
json.raw <- spTransform(json.raw,
                       CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later

# fortify, i.e., make ggplot2-compatible
json.raw@data$id = rownames(json.raw@data)
map.data.fortified <- fortify(json.raw, region = "id")
map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
map.data1 <- map.data[, c("long", "lat", "order", "id", "group", "gid", "name", "iso3", "idprogres")] # subset with only needed columns to accelerate performance

rm(map.data.fortified, destfilepath, json.raw, geojsonurl)

# }
#######
# ADM2


geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM2.geojson")


filename <- "JOR_ADM2.geojson"
destfilepath <- paste0("out/geo/geojson/", filename )

download.file( geojsonurl, destfile=destfilepath )
json.raw <- geojson_read( destfilepath, method="local", what="sp" )
#plot(json.raw)

proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
json.raw <- spTransform(json.raw,
                         CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later

# fortify, i.e., make ggplot2-compatible
json.raw@data$id = rownames(json.raw@data)
map.data.fortified <- fortify(json.raw, region = "id")
map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
map.data2 <- map.data[, c("long", "lat", "order", "id", "group", "admin_unhcr2_fid", "adm2name", "idcountry", "idadm2")] # subset with only needed columns to accelerate performance
rm(map.data.fortified, destfilepath, json.raw, geojsonurl)

####################
####################












# average by admin level
#total <- sum(data.country.list[[1]]['Num_Inds'])
myfun1 <- function(x){c(pct=sum(x)/length(x))}

adm1.Num_Inds <- summaryBy(cbind(Num_Inds) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.AVG_Age <- summaryBy(cbind(AVG_Age) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.STDEV_Age <- summaryBy(cbind(STDEV_Age) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))


adm1.Child_0_14 <- summaryBy(cbind(Child_0_14) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.Youth_15_17 <- summaryBy(cbind(Youth_15_17) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.Work_15_64 <- summaryBy(cbind(Work_15_64) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.Eldern_65 <- summaryBy(cbind(Eldern_65) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.dependency <- summaryBy(cbind(dependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.youthdependency <- summaryBy(cbind(youthdependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.elederndependency <- summaryBy(cbind(elederndependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.gender.female <- summaryBy(cbind(Female) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))

adm1.Torture <- summaryBy(cbind(Torture) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.prot_needs <- summaryBy(cbind(Specific.legal.and.physical.protection.needs) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.single_parent <- summaryBy(cbind(Single.parent) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.SGBV <- summaryBy(cbind(SGBV) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.ser.medical.condition <- summaryBy(cbind(Serious.medical.condition) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.pregnant.lactating <- summaryBy(cbind(Pregnant.or.lactating) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.Older.risk <- summaryBy(cbind(Older.person.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.family.unity <- summaryBy(cbind(Family.unity) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.disability <- summaryBy(cbind(Disability) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.child.risk <- summaryBy(cbind(Child.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.unaccomp.children <- summaryBy(cbind(Unaccompanied.or.separated.child) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
adm1.woman.at.risk <- summaryBy(cbind(Woman.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))

#join all avergages to one list for mapping purpose and delete variables don't needed
adm1.asylum.averages <- plyr::join_all(list(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.AVG_Age, adm1.STDEV_Age, adm1.dependency, adm1.youthdependency, adm1.elederndependency, adm1.Torture, adm1.prot_needs, adm1.single_parent, adm1.SGBV, adm1.ser.medical.condition, adm1.pregnant.lactating, adm1.Older.risk, adm1.family.unity, adm1.disability, adm1.child.risk, adm1.unaccomp.children, adm1.woman.at.risk), by = 'coal1id', type = 'full')
colnames(adm1.asylum.averages)[1] <- "idprogres"
rm(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.AVG_Age, adm1.STDEV_Age, adm1.dependency, adm1.youthdependency, adm1.elederndependency, adm1.Torture, adm1.prot_needs, adm1.single_parent, adm1.SGBV, adm1.ser.medical.condition, adm1.pregnant.lactating, adm1.Older.risk, adm1.family.unity, adm1.disability, adm1.child.risk, adm1.unaccomp.children, adm1.woman.at.risk)



### aggregation by admin level 2 code of asylum ################
## use 'doBy'-package
# frequency of data rowns as sign of data quality
adm2.Num_Inds.count <- summaryBy(cbind(Num_Inds) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.Child_0_14.count <- summaryBy(cbind(Child_0_14) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.Youth_15_17.count <- summaryBy(cbind(Youth_15_17) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.Work_15_64.count <- summaryBy(cbind(Work_15_64) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.Eldern_65.count <- summaryBy(cbind(Eldern_65) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.AVG_Age.count <- summaryBy(cbind(AVG_Age) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.STDEV_Age.count <- summaryBy(cbind(STDEV_Age) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.dependency.count <- summaryBy(cbind(dependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.youthdependency.count <- summaryBy(cbind(youthdependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
adm2.elederndependency.count <- summaryBy(cbind(elederndependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))

#join all avergages to one list for mapping purpose and delete variables don't needed
adm2.asylum.frequency <- plyr::join_all(list(adm2.Num_Inds.count, adm2.Child_0_14.count, adm2.Youth_15_17.count, adm2.Work_15_64.count, adm2.Eldern_65.count, adm2.AVG_Age.count, adm2.STDEV_Age.count, adm2.dependency.count, adm2.youthdependency.count, adm2.elederndependency.count), by = 'coal2id', type = 'full')
colnames(adm2.asylum.frequency)[1] <- "idprogres"
rm(adm2.Num_Inds.count, adm2.Child_0_14.count, adm2.Youth_15_17.count, adm2.Work_15_64.count, adm2.Eldern_65.count, adm2.AVG_Age.count, adm2.STDEV_Age.count, adm2.dependency.count, adm2.youthdependency.count, adm2.elederndependency.count)


# average by admin level
adm2.Num_Inds <- summaryBy(cbind(Num_Inds) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.Child_0_14 <- summaryBy(cbind(Child_0_14) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.Youth_15_17 <- summaryBy(cbind(Youth_15_17) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.Work_15_64 <- summaryBy(cbind(Work_15_64) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.Eldern_65 <- summaryBy(cbind(Eldern_65) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.AVG_Age <- summaryBy(cbind(AVG_Age) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.STDEV_Age <- summaryBy(cbind(STDEV_Age) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.dependency <- summaryBy(cbind(dependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.youthdependency <- summaryBy(cbind(youthdependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
adm2.elederndependency <- summaryBy(cbind(elederndependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))

#join all avergages to one list for mapping purpose and delete variables don't needed
adm2.asylum.averages <- plyr::join_all(list(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.AVG_Age, adm2.STDEV_Age, adm2.dependency, adm2.youthdependency, adm2.elederndependency), by = 'coal2id', type = 'full')
rm(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.AVG_Age, adm2.STDEV_Age, adm2.dependency, adm2.youthdependency, adm2.elederndependency)



#############
#############



##where data is factor 2 (yes/no) or (1/0) as ratio ################################################
###ADMINLEVEL1
total <- nrow(data.country.list[[1]])
#tortured
data.asylum.Torture <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c('Torture'))), coal1id ~ value  )
data.asylum.Torture$Torture.ratio <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
#legal protection needs
data.asylum.prot_needs <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(99))), coal1id ~ value  )
data.asylum.prot_needs$prot_needs.ratio <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
#single parent ratio
data.asylum.single_parent <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(98))), coal1id ~ value  )
data.asylum.single_parent$single_parent.ratio <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
#SGBV
data.asylum.SGBV <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(97))), coal1id ~ value  )
data.asylum.SGBV$SGBV.ratio <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
#serious medical condition
data.asylum.ser.medical.condition <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(96))), coal1id ~ value  )
data.asylum.ser.medical.condition$ser.medical.condition.ratio <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
#pregnant or lactating
data.asylum.pregnant.lactating <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(95))), coal1id ~ value  )
data.asylum.pregnant.lactating$pregnant.lactating.ratio <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
#older persons at risk
data.asylum.Older.risk <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(94))), coal1id ~ value  )
data.asylum.Older.risk$ser.medical.condition.ratio <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
#family unity
data.asylum.family.unity <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(93))), coal1id ~ value  )
data.asylum.family.unity$family.unity.ratio <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
#disability
data.asylum.disability <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(92))), coal1id ~ value  )
data.asylum.disability$disability.ratio <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
#child at risk
data.asylum.child.risk <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(91))), coal1id ~ value  )
data.asylum.child.risk$child.risk.ratio <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
#gender.female
data.asylum.gender.female <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(90))), coal1id ~ value  )
data.asylum.gender.female$gender.female <- (data.asylum.gender.female$"1"/(data.asylum.gender.female$"1"+data.asylum.gender.female$"0"))*100













#### join with geojson for MENA --> subset for countries



##where data is factor 2 (yes/no) or (1/0) as ratio ################################################
###ADMINLEVEL1

xc <- aggregate(cbind(Child.at.risk) ~ coal1id, data = subset.progrescase.percentage, FUN = function (x) {x/sum(x)} , na.rm = TRUE)



myfun <- function (x) ifelse(sum(x) == 0, 0, x/sum(x)) 
adm1.child.risk <- aggregate(cbind('Child.at.risk'), by = list(unique.values = subset.progrescase.percentage$coal1id), FUN = myfun, na.rm = TRUE)




#child at risk
adm1.child.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(20)), coal1id ~ value  )
adm1.child.risk$child.risk.percent <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
#disability
adm1.disability <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(21)), coal1id ~ value  )
data.asylum.disability$disability.percent <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
#family unity
adm1.family.unity <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(22)), coal1id ~ value  )
data.asylum.family.unity$family.unity.percent <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
#older persons at risk
adm1.Older.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(23)), coal1id ~ value  )
data.asylum.Older.risk$ser.medical.condition.percent <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
#pregnant or lactating
adm1.pregnant.lactating <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(24)), coal1id ~ value  )
data.asylum.pregnant.lactating$pregnant.lactating.percent <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
#serious medical condition
adm1.ser.medical.condition <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(25)), coal1id ~ value  )
data.asylum.ser.medical.condition$ser.medical.condition.percent <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
#SGBV
adm1.SGBV <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(26)), coal1id ~ value  )
data.asylum.SGBV$SGBV.percent <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
#single parent ratio
adm1.single_parent <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(27)), coal1id ~ value  )
data.asylum.single_parent$single_parent.percent <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
#legal protection needs
adm1.prot_needs <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(28)), coal1id ~ value  )
data.asylum.prot_needs$prot_needs.percent <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
#tortured
adm1.Torture <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(29)), coal1id ~ value  )
data.asylum.Torture$Torture.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
#unaccompanied or separated children
adm1.unaccomp.children <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(30)), coal1id ~ value  )
adm1.unaccomp.children$Unaccompanied.or.separated.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
#woman at risk            
adm1.woman.at.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(31)), coal1id ~ value  )
adm1.woman.at.risk$Woman.at.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
                      




