## clear workspace
rm(list = ls())

##############################
## Add iso3 countrycodes for merge with geojson later
country.codes <- read.csv("data/countrycodes.csv")

## Function to summarize data, source (http://www.cookbook-r.com/Manipulating_data/Summarizing_data/):
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  
  return(datac)
}

##############################
## load prepared data
progrescase.processed <- read.csv("data/progrescase_maps.csv")
data.progrescase.specificneed <- progrescase.processed

##############################
## create country code conversion table only for existent countries
all.countries <- data.frame(unique(data.progrescase.specificneed$CountryAsylum))
colnames(all.countries) <- "P_Code"
country <- merge(all.countries, country.codes, all.x=TRUE)
country$ISO_Alpha_3Code <- as.factor(country$ISO_Alpha_3Code)
country$ISO_Alpha_3Code <- factor(country$ISO_Alpha_3Code)

## needs to be entire dataframe not only Jordan when looping through countries
country<- country[country$P_Code == "JOR", ]
# country<- country[!country$P_Code == "GCC", ]
# country<- country[!country$P_Code == "ISR", ] #remove as long as there is no geojson
# country<- country[!country$P_Code == "LEB", ] #remove as long as geojson has different columnnames

##############################
## create lists and dataframes that will be filled in the loop by joining data and geojson
## consistency table will show percentage of consistent data for each country and each admin level
adm1.list <- list()
adm2.list <- list()
mapdata.list.adm1 <- list()
mapdata.list.adm2 <- list()
rm(consistency.table)
consistency.table <- NULL


##############################
## looping spatial data aggregation through all countries
## steps in loop: 
## 1) country subset of data for adm1 & adm2
## 2) calculate percentage of consistent data within each country & admin level and write values in consistency table
## 3) adjust keycolumn names in data for coming join with geojson
## 4) creation of geojson folder and loading/saving of geojson adm1 & adm2
## 5) apply function to all data variables to calculate mean values and margin of error
## 6) check if dataframe has data and join with geojson adm1 & adm2
i=1

for (i in 1:nrow(country)) {
  pcode <- country[i,1]
  iso3 <- country[i,2]
  
  ## 1) country subset for different admin levels
  df_CountryAsylum <- data.progrescase.specificneed[grep(pcode, data.progrescase.specificneed$CountryAsylum), ]
  adm1 <- df_CountryAsylum[grep(pcode, df_CountryAsylum$coal1id), ] #only rows where adm0 and adm1 are consistent
  adm2 <- adm1[grep(pcode, adm1$coal2id), ] #only rows where adm1 and adm2 are consistent
  
  
  ## 2) let's check how much percent of data is consistent within each country and adminlevel
  ## rows where CountryAsylum has countrycode is considered as total
  ## create empty consistency table and fill with accuracy percentage data
  total <- nrow(df_CountryAsylum)
  pct.adm1 <- round((nrow(adm1)/total), digits = 2) # if nrow country codes is considered as total 
  pct.adm2 <- round((nrow(adm2)/total), digits = 2) # if nrow country codes is considered as total 
  consistency.table <- rbind(consistency.table, data.frame(iso3, total, pct.adm1, pct.adm2))
  
  
  ## 3) adjust column names of key columns for successful join with geojson
  names(adm1)[names(adm1) == 'coal1id'] <- 'idprogres'
  names(adm2)[names(adm2) == 'coal2id'] <- 'idadm2'
  
  
  ## 4) loading geojson
  ## admin level 1
  
  geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM1.geojson")
  
  # create output folder
  mainDir <- "data"
  subDir <- "/mapping/geojson"
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  filename <- paste0(iso3,"_ADM1.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  download.file( geojsonurl, destfile=destfilepath )
  json.raw <- geojson_read( destfilepath, method="local", what="sp" )
  
  proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  json.raw <- spTransform(json.raw,
                          CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later
  
  # this is a well known R / GEOS hack (usually combined with the above) to 
  # deal with "bad" polygons
  json.raw <- gBuffer(json.raw, byid=TRUE, width=0)
  # plot(json.raw)
  
  # fortify, i.e., make ggplot2-compatible
  json.raw@data$id = rownames(json.raw@data)
  map.data.fortified <- fortify(json.raw, region = "id")
  map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
  map.data.adm1 <- map.data[, c("long", "lat", "order", "id", "group", "gid", "name", "iso3", "idprogres")] # subset with only needed columns to accelerate performance
  
  
  ## p-code repository needs to be consistent in case of id's (sometimes idprogres is used sometimes idadm2)
  ## admin level 2
  geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM2.geojson")
  
  # use already created output folder and existent variables
  filename <- paste0(iso3,"_ADM2.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename)
  download.file( geojsonurl, destfile=destfilepath)
  json.raw <- geojson_read( destfilepath, method="local", what="sp")
  
  proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  json.raw <- spTransform(json.raw,
                          CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later
  json.raw <- gBuffer(json.raw, byid=TRUE, width=0)
  
  json.raw@data$id = rownames(json.raw@data)
  map.data.fortified <- fortify(json.raw, region = "id")
  map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
  map.data.adm2 <- map.data[, c("long", "lat", "order", "id", "group", "idcountry", "idadm2", "adm2name")] # subset with only needed columns to accelerate performance
  
  
  
  ## 5) data processing steps on country level
  
  ## Calculating dependency percentage within country (100% should be current country subset not adm. unit or all countries)
  # child+elderly, # child, # elderly #single female already existent
  # adm1
  adm1$dependency <- (adm1$Child_0_14+adm1$Eldern_65) / (adm1$Work_15_64+adm1$Child_0_14+adm1$Eldern_65)
  adm1$youthdependency <- adm1$Child_0_14 / (adm1$Work_15_64+adm1$Child_0_14+adm1$Eldern_65)
  adm1$elederndependency <- adm1$Eldern_65 / (adm1$Work_15_64+adm1$Child_0_14+adm1$Eldern_65)
  # adm2
  adm2$dependency <- (adm2$Child_0_14+adm2$Eldern_65) / (adm2$Work_15_64+adm2$Child_0_14+adm2$Eldern_65)
  adm2$youthdependency <- adm2$Child_0_14 / (adm2$Work_15_64+adm2$Child_0_14+adm2$Eldern_65)
  adm2$elederndependency <- adm2$Eldern_65 / (adm2$Work_15_64+adm2$Child_0_14+adm2$Eldern_65)
  
  ## Individual subsets for some variables to loose as less data as possible
  ## remove observation where we do not have gender data
  adm1.sex <- adm1[adm1$dem_sex !=c("Unknown","U","f","m","-"), ]
  adm2.sex <- adm2[adm2$dem_sex !=c("Unknown","U","f","m","-"), ]
  
  ## store data of adm1 and adm2 together in one list
  data.country.list <- list(adm1, adm2)
  
  ## data aggregation by admin level 1 code of asylum
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
  adm1.case.female.headed <- summarySE(adm1.sex, measurevar="female.headed", groupvars=c("idprogres"), na.rm=TRUE) 
  
  
  
  ## data aggregation by admin level 2 
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
  adm2.case.female.headed <- summarySE(adm2.sex, measurevar="female.headed", groupvars=c("idadm2"), na.rm=TRUE) 
  
  
  
  ## 6) check if dataframe has data and join with geojson
  ## adm1
  ## loop for listing of all dataframes with consistent data
  if (nrow(adm1.Num_Inds) >= 1){
    #list all processed variables to one list for mapping purpose and delete variables for better variable management
    list.adm1 <- list(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.case.STDEV_Age, adm1.head.dem_age, adm1.case.female, adm1.case.male, adm1.case.only.female, adm1.case.only.male, adm1.case.female.headed, adm1.case.dependency, adm1.case.youthdependency, adm1.case.elederndependency)
    #adm1.list[[i]] <- list(list.adm1)
    
    ## loop for joining each variable within all dataframes with geojson
    for (n in 1:length(list.adm1)) {
      map.data <- plyr::join(x=map.data.adm1, y=list.adm1[[n]], by="idprogres")
      map.data$brks <- 0
      map.data <- map.data[,c(which(colnames(map.data)=="brks"),which(colnames(map.data)!="brks"))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
      mapdata.list.adm1[[n]] <- list(map.data)
    }
    rm(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.case.STDEV_Age, adm1.head.dem_age, adm1.case.female, adm1.case.male, adm1.case.only.female, adm1.case.only.male, adm1.case.female.headed, adm1.case.dependency, adm1.case.youthdependency, adm1.case.elederndependency)
    
  } else { 
    rm(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.case.STDEV_Age, adm1.head.dem_age, adm1.case.female, adm1.case.male, adm1.case.only.female, adm1.case.only.male, adm1.case.female.headed, adm1.case.dependency, adm1.case.youthdependency, adm1.case.elederndependency)
  } 
  
  ## may be needed later when looping through all countries
  # adm1.list[[i]] <- list(map.data.list)
  
  
  ## adm2
  if (nrow(adm2.Num_Inds) >= 1){
    #list all processed variables to one list for mapping purpose and delete variables for better variable management
    list.adm2 <- list(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)
    #adm2.list[[i]] <- list(list.adm2)
    
    for (n in 1:length(list.adm2)) {
      map.data <- plyr::join(x=map.data.adm2, y=list.adm2[[n]], by="idadm2")
      ##adjust length of data for loop in '3-Create-Map'
      map.data$brks <- 0
      map.data <- map.data[,c(which(colnames(map.data)=="brks"),which(colnames(map.data)!="brks"))] #reorder data so that column 'brks' is at 1, first comma means keep all the rows
      mapdata.list.adm2[[n]] <- list(map.data)
    }
    rm(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)
    
  } else {
    rm(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)
    rm(map.data)
  }
  
}





















## NEEDS TO BE CHECKED
#
# # average by admin level
# #total <- sum(data.country.list[[1]]['Num_Inds'])
# myfun1 <- function(x){c(pct=sum(x)/length(x))}
# 
# adm1.Num_Inds <- summaryBy(cbind(Num_Inds) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.AVG_Age <- summaryBy(cbind(AVG_Age) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.STDEV_Age <- summaryBy(cbind(STDEV_Age) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# 
# 
# adm1.Child_0_14 <- summaryBy(cbind(Child_0_14) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.Youth_15_17 <- summaryBy(cbind(Youth_15_17) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.Work_15_64 <- summaryBy(cbind(Work_15_64) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.Eldern_65 <- summaryBy(cbind(Eldern_65) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.dependency <- summaryBy(cbind(dependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.youthdependency <- summaryBy(cbind(youthdependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.elederndependency <- summaryBy(cbind(elederndependency) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.gender.female <- summaryBy(cbind(Female) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# 
# adm1.Torture <- summaryBy(cbind(Torture) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.prot_needs <- summaryBy(cbind(Specific.legal.and.physical.protection.needs) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.single_parent <- summaryBy(cbind(Single.parent) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.SGBV <- summaryBy(cbind(SGBV) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.ser.medical.condition <- summaryBy(cbind(Serious.medical.condition) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.pregnant.lactating <- summaryBy(cbind(Pregnant.or.lactating) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.Older.risk <- summaryBy(cbind(Older.person.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.family.unity <- summaryBy(cbind(Family.unity) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.disability <- summaryBy(cbind(Disability) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.child.risk <- summaryBy(cbind(Child.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.unaccomp.children <- summaryBy(cbind(Unaccompanied.or.separated.child) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm1.woman.at.risk <- summaryBy(cbind(Woman.at.risk) ~ coal1id, data=data.country.list[[1]], FUN=c(function(x)mean(x, na.rm=TRUE)))
# 
# #join all avergages to one list for mapping purpose and delete variables don't needed
# adm1.asylum.averages <- plyr::join_all(list(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.AVG_Age, adm1.STDEV_Age, adm1.dependency, adm1.youthdependency, adm1.elederndependency, adm1.Torture, adm1.prot_needs, adm1.single_parent, adm1.SGBV, adm1.ser.medical.condition, adm1.pregnant.lactating, adm1.Older.risk, adm1.family.unity, adm1.disability, adm1.child.risk, adm1.unaccomp.children, adm1.woman.at.risk), by = 'coal1id', type = 'full')
# colnames(adm1.asylum.averages)[1] <- "idprogres"
# rm(adm1.Num_Inds, adm1.Child_0_14, adm1.Youth_15_17, adm1.Work_15_64, adm1.Eldern_65, adm1.AVG_Age, adm1.STDEV_Age, adm1.dependency, adm1.youthdependency, adm1.elederndependency, adm1.Torture, adm1.prot_needs, adm1.single_parent, adm1.SGBV, adm1.ser.medical.condition, adm1.pregnant.lactating, adm1.Older.risk, adm1.family.unity, adm1.disability, adm1.child.risk, adm1.unaccomp.children, adm1.woman.at.risk)
# 
# 
# 
# ### aggregation by admin level 2 code of asylum ################
# ## use 'doBy'-package
# # frequency of data rowns as sign of data quality
# adm2.Num_Inds.count <- summaryBy(cbind(Num_Inds) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.Child_0_14.count <- summaryBy(cbind(Child_0_14) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.Youth_15_17.count <- summaryBy(cbind(Youth_15_17) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.Work_15_64.count <- summaryBy(cbind(Work_15_64) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.Eldern_65.count <- summaryBy(cbind(Eldern_65) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.AVG_Age.count <- summaryBy(cbind(AVG_Age) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.STDEV_Age.count <- summaryBy(cbind(STDEV_Age) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.dependency.count <- summaryBy(cbind(dependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.youthdependency.count <- summaryBy(cbind(youthdependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# adm2.elederndependency.count <- summaryBy(cbind(elederndependency) ~ coal2id, data=data.country.list[[2]], FUN=c(length))
# 
# #join all avergages to one list for mapping purpose and delete variables don't needed
# adm2.asylum.frequency <- plyr::join_all(list(adm2.Num_Inds.count, adm2.Child_0_14.count, adm2.Youth_15_17.count, adm2.Work_15_64.count, adm2.Eldern_65.count, adm2.AVG_Age.count, adm2.STDEV_Age.count, adm2.dependency.count, adm2.youthdependency.count, adm2.elederndependency.count), by = 'coal2id', type = 'full')
# colnames(adm2.asylum.frequency)[1] <- "idprogres"
# rm(adm2.Num_Inds.count, adm2.Child_0_14.count, adm2.Youth_15_17.count, adm2.Work_15_64.count, adm2.Eldern_65.count, adm2.AVG_Age.count, adm2.STDEV_Age.count, adm2.dependency.count, adm2.youthdependency.count, adm2.elederndependency.count)
# 
# 
# # average by admin level
# adm2.Num_Inds <- summaryBy(cbind(Num_Inds) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.Child_0_14 <- summaryBy(cbind(Child_0_14) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.Youth_15_17 <- summaryBy(cbind(Youth_15_17) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.Work_15_64 <- summaryBy(cbind(Work_15_64) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.Eldern_65 <- summaryBy(cbind(Eldern_65) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.AVG_Age <- summaryBy(cbind(AVG_Age) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.STDEV_Age <- summaryBy(cbind(STDEV_Age) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.dependency <- summaryBy(cbind(dependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.youthdependency <- summaryBy(cbind(youthdependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# adm2.elederndependency <- summaryBy(cbind(elederndependency) ~ coal2id, data=data.progrescase.specificneed, FUN=c(function(x)mean(x, na.rm=TRUE)))
# 
# #join all avergages to one list for mapping purpose and delete variables don't needed
# adm2.asylum.averages <- plyr::join_all(list(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.AVG_Age, adm2.STDEV_Age, adm2.dependency, adm2.youthdependency, adm2.elederndependency), by = 'coal2id', type = 'full')
# rm(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.AVG_Age, adm2.STDEV_Age, adm2.dependency, adm2.youthdependency, adm2.elederndependency)



#############
#############



# ##where data is factor 2 (yes/no) or (1/0) as ratio ################################################
# ###ADMINLEVEL1
# total <- nrow(data.country.list[[1]])
# #tortured
# data.asylum.Torture <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c('Torture'))), coal1id ~ value  )
# data.asylum.Torture$Torture.ratio <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
# #legal protection needs
# data.asylum.prot_needs <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(99))), coal1id ~ value  )
# data.asylum.prot_needs$prot_needs.ratio <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
# #single parent ratio
# data.asylum.single_parent <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(98))), coal1id ~ value  )
# data.asylum.single_parent$single_parent.ratio <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
# #SGBV
# data.asylum.SGBV <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(97))), coal1id ~ value  )
# data.asylum.SGBV$SGBV.ratio <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
# #serious medical condition
# data.asylum.ser.medical.condition <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(96))), coal1id ~ value  )
# data.asylum.ser.medical.condition$ser.medical.condition.ratio <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
# #pregnant or lactating
# data.asylum.pregnant.lactating <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(95))), coal1id ~ value  )
# data.asylum.pregnant.lactating$pregnant.lactating.ratio <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
# #older persons at risk
# data.asylum.Older.risk <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(94))), coal1id ~ value  )
# data.asylum.Older.risk$ser.medical.condition.ratio <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
# #family unity
# data.asylum.family.unity <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(93))), coal1id ~ value  )
# data.asylum.family.unity$family.unity.ratio <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
# #disability
# data.asylum.disability <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(92))), coal1id ~ value  )
# data.asylum.disability$disability.ratio <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
# #child at risk
# data.asylum.child.risk <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(91))), coal1id ~ value  )
# data.asylum.child.risk$child.risk.ratio <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
# #gender.female
# data.asylum.gender.female <- dcast((melt(data.country.list[[1]], id=c('coal1id'), measure=c(90))), coal1id ~ value  )
# data.asylum.gender.female$gender.female <- (data.asylum.gender.female$"1"/(data.asylum.gender.female$"1"+data.asylum.gender.female$"0"))*100







#### join with geojson for MENA --> subset for countries



##where data is factor 2 (yes/no) or (1/0) as ratio ################################################
###ADMINLEVEL1

# xc <- aggregate(cbind(Child.at.risk) ~ coal1id, data = subset.progrescase.percentage, FUN = function (x) {x/sum(x)} , na.rm = TRUE)
# 
# 
# 
# myfun <- function (x) ifelse(sum(x) == 0, 0, x/sum(x)) 
# adm1.child.risk <- aggregate(cbind('Child.at.risk'), by = list(unique.values = subset.progrescase.percentage$coal1id), FUN = myfun, na.rm = TRUE)
# 
# 
# 
# 
# #child at risk
# adm1.child.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(20)), coal1id ~ value  )
# adm1.child.risk$child.risk.percent <- (data.asylum.child.risk$yes/(data.asylum.child.risk$yes+data.asylum.child.risk$no))*100
# #disability
# adm1.disability <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(21)), coal1id ~ value  )
# data.asylum.disability$disability.percent <- (data.asylum.disability$yes/(data.asylum.disability$yes+data.asylum.disability$no))*100
# #family unity
# adm1.family.unity <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(22)), coal1id ~ value  )
# data.asylum.family.unity$family.unity.percent <- (data.asylum.family.unity$yes/(data.asylum.family.unity$yes+data.asylum.family.unity$no))*100
# #older persons at risk
# adm1.Older.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(23)), coal1id ~ value  )
# data.asylum.Older.risk$ser.medical.condition.percent <- (data.asylum.Older.risk$yes/(data.asylum.Older.risk$yes+data.asylum.Older.risk$no))*100
# #pregnant or lactating
# adm1.pregnant.lactating <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(24)), coal1id ~ value  )
# data.asylum.pregnant.lactating$pregnant.lactating.percent <- (data.asylum.pregnant.lactating$yes/(data.asylum.pregnant.lactating$yes+data.asylum.pregnant.lactating$no))*100
# #serious medical condition
# adm1.ser.medical.condition <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(25)), coal1id ~ value  )
# data.asylum.ser.medical.condition$ser.medical.condition.percent <- (data.asylum.ser.medical.condition$yes/(data.asylum.ser.medical.condition$yes+data.asylum.ser.medical.condition$no))*100
# #SGBV
# adm1.SGBV <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(26)), coal1id ~ value  )
# data.asylum.SGBV$SGBV.percent <- (data.asylum.SGBV$yes/(data.asylum.SGBV$yes+data.asylum.SGBV$no))*100
# #single parent ratio
# adm1.single_parent <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(27)), coal1id ~ value  )
# data.asylum.single_parent$single_parent.percent <- (data.asylum.single_parent$yes/(data.asylum.single_parent$yes+data.asylum.single_parent$no))*100
# #legal protection needs
# adm1.prot_needs <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(28)), coal1id ~ value  )
# data.asylum.prot_needs$prot_needs.percent <- (data.asylum.prot_needs$yes/(data.asylum.prot_needs$yes+data.asylum.prot_needs$no))*100
# #tortured
# adm1.Torture <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(29)), coal1id ~ value  )
# data.asylum.Torture$Torture.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
# #unaccompanied or separated children
# adm1.unaccomp.children <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(30)), coal1id ~ value  )
# adm1.unaccomp.children$Unaccompanied.or.separated.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100
# #woman at risk            
# adm1.woman.at.risk <- dcast(melt(subset.progrescase.percentage, id=c('coal1id'), measure=c(31)), coal1id ~ value  )
# adm1.woman.at.risk$Woman.at.percent <- (data.asylum.Torture$yes/(data.asylum.Torture$yes+data.asylum.Torture$no))*100





