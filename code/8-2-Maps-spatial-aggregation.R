
#### Create spatial aggregation

source("code/0-packages.R")

## clear workspace except necessary data frame function
#keep(data.progrescase, sure = TRUE)

original.progrescase <- read.csv("data/progrescase-1.csv")
data.progrescase <- original.progrescase

#############################################################################################
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

for (i in 1:nrow(country)) {
  pcode <- country[i,1]
  iso3 <- country[i,2]
  
  ############################################################################
  cat(paste("Loading country", iso3,"\n"))
  
  ############################################################################
  cat("Subset and looking at consistency \n")
  #############################################################################################  
  ## 1) country subset for different admin levels
  df_CountryAsylum <- data.progrescase[grep(pcode, data.progrescase$CountryAsylum), ]
  adm1 <- df_CountryAsylum[grep(pcode, df_CountryAsylum$coal1id), ] #only rows where adm0 and adm1 are consistent
  adm2 <- adm1[grep(pcode, adm1$coal2id), ] #only rows where adm1 and adm2 are consistent

  #############################################################################################
  ## 2) let's check how much percent of data is consistent within each country and adminlevel
  ## rows where CountryAsylum has countrycode is considered as total
  ## create empty consistency table and fill with accuracy percentage data
  total <- nrow(df_CountryAsylum)
  pct.adm1 <- round((nrow(adm1)/total)*100, digits = 1) # if nrow country codes is considered as total 
  pct.adm2 <- round((nrow(adm2)/total)*100, digits = 1) # if nrow country codes is considered as total 
  consistency.table <- rbind(consistency.table, data.frame(iso3, total, pct.adm1, pct.adm2))
  
  #############################################################################################
  ## 3) adjust column names of key columns for successful join with geojson
  names(adm1)[names(adm1) == 'coal1id'] <- 'idprogres'
  names(adm2)[names(adm2) == 'coal2id'] <- 'idprogres'
  
  #############################################################################################
  ## 4) loading geojson
  
  ############################################################################
  cat("Fortifying level 1 \n")
  ## admin level 1
  filename <- paste0(iso3,"_ADM1.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  json.raw <- geojson_read( destfilepath, method="local", what="sp" )
  
  proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  json.raw <- spTransform(json.raw, CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later
  
  # this is a well known R / GEOS hack (usually combined with the above) to 
  # deal with "bad" polygons
  json.raw <- gBuffer(json.raw, byid=TRUE, width=0)
  # plot(json.raw)
  
  # fortify, i.e., make ggplot2-compatible
  json.raw@data$id = rownames(json.raw@data)
  map.data.fortified <- fortify(json.raw, region = "id")
  map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
  map.data.adm1 <- map.data[, c("long", "lat", "group", "hole", "admname", "iso3", "idprogres")] # subset with only needed columns to accelerate performance
  
  ############################################################################
  cat("Fortifying level 2 \n")
  ## p-code repository needs to be consistent in case of id's (sometimes idadm1 is used sometimes idadm2)
  ## admin level 2
  filename <- paste0(iso3,"_ADM2.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  json.raw <- geojson_read( destfilepath, method="local", what="sp")
  
  proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  json.raw <- spTransform(json.raw, CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later
  
  json.raw <- gBuffer(json.raw, byid=TRUE, width=0)
  
  json.raw@data$id = rownames(json.raw@data)
  map.data.fortified <- fortify(json.raw, region = "id")
  map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
  map.data.adm2 <- map.data[, c("long", "lat", "group", "hole", "admname", "iso3", "idprogres")] # subset with only needed columns to accelerate performance
  
  
  ####################################################################################
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
  adm.sex.list <- list(adm1.sex, adm2.sex)
  
  
  
  ############################################################################
  cat("data aggregation by admin level 1 \n")
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
  adm1.case.female.headed <- summarySE(adm.sex.list[[1]], measurevar="female.headed", groupvars=c("idprogres"), na.rm=TRUE) 
  

  
  ############################################################################
  cat("data aggregation by admin level 2 \n")
  ## data aggregation by admin level 2 
  adm2.Num_Inds <- summarySE(data.country.list[[2]], measurevar="Num_Inds", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.Child_0_14 <- summarySE(data.country.list[[2]], measurevar="Child_0_14", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.Youth_15_17 <- summarySE(data.country.list[[2]], measurevar="Youth_15_17", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.Work_15_64 <- summarySE(data.country.list[[2]], measurevar="Work_15_64", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.Eldern_65 <- summarySE(data.country.list[[2]], measurevar="Eldern_65", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.STDEV_Age <- summarySE(data.country.list[[2]], measurevar="STDEV_Age", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.head.dem_age <- summarySE(data.country.list[[2]], measurevar="dem_age", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.female <- summarySE(data.country.list[[2]], measurevar="Female", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.male <- summarySE(data.country.list[[2]], measurevar="Male", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.only.female <- summarySE(data.country.list[[2]], measurevar="only.female.case", groupvars=c("idprogres"), na.rm=TRUE) 
  adm2.case.only.male <- summarySE(data.country.list[[2]], measurevar="only.male.case", groupvars=c("idprogres"), na.rm=TRUE) 
  adm2.case.dependency <- summarySE(data.country.list[[2]], measurevar="dependency", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.youthdependency <- summarySE(data.country.list[[2]], measurevar="youthdependency", groupvars=c("idprogres"), na.rm=TRUE)
  adm2.case.elederndependency <- summarySE(data.country.list[[2]], measurevar="elederndependency", groupvars=c("idprogres"), na.rm=TRUE)
  # special case where other data is used: column 'female.headed' is based on column 'dem_sex' which contains invalid values
  # therefore it is for this particular case necessary to use a special cleaned data 
  adm2.case.female.headed <- summarySE(adm.sex.list[[2]], measurevar="female.headed", groupvars=c("idprogres"), na.rm=TRUE) 
  
  
  ############################################################################
  cat("check if dataframe has data and join with geojson\n")
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
  adm1.list[[i]] <- list(mapdata.list.adm1)
  
  
  ## adm2
  if (nrow(adm2.Num_Inds) >= 1){
    #list all processed variables to one list for mapping purpose and delete variables for better variable management
    list.adm2 <- list(adm2.Num_Inds, adm2.Child_0_14, adm2.Youth_15_17, adm2.Work_15_64, adm2.Eldern_65, adm2.case.STDEV_Age, adm2.head.dem_age, adm2.case.female, adm2.case.male, adm2.case.only.female, adm2.case.only.male, adm2.case.female.headed, adm2.case.dependency, adm2.case.youthdependency, adm2.case.elederndependency)
    #adm2.list[[i]] <- list(list.adm2)
    
    n=1
    for (n in 1:length(list.adm2)) {
      map.data <- plyr::join(x=map.data.adm2, y=list.adm2[[n]], by="idprogres")
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
  adm2.list[[i]] <- list(mapdata.list.adm2)
  

}
  

titles <- c("Average # of individuals per case", 
            "Average # of children (0-14) per case",
            "Average # of adolescent (15-17) per case", 
            "Average # of persons (15-65) per case",
            "Average # of elderly (65+) per case",
            "standard deviation of age within cases",
            "Average age of principal applicant per case", 
            "Average # of females per case",
            "Average # of males per case",
            "% of cases with only female members", 
            "% of cases with only male members",
            "% of cases with female principal applicants",
            "% of dependent persons (children < 14 & elderly 65+) / total", 
            "% of dependent children < 14 / total",
            "% of dependent elderly 65 + / total")

## kep ,consistency.table, adm1.list, adm2.list, list.adm1,list.adm2,

rm(adm1,adm1.sex,adm2,adm2.sex,df_CountryAsylum,
   map.data,map.data.adm1,map.data.adm2,mapdata.list.adm1, mapdata.list.adm2,
   original.progrescase,map.data.fortified,
   adm.sex.list,data.country.list,destfilepath,filename,i,iso3,json.raw,n,pcode,pct.adm1,pct.adm2,total)
