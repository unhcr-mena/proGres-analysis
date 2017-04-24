
## clear workspace except necessary data frame function
#keep(data.progrescase, sure = TRUE)

original.progrescase <- read.csv("data/progrescase_maps.csv")
data.progrescase <- original.progrescase


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
## add iso3 countrycodes for merge with geojson later
country.codes <- read.csv("data/countrycodes.csv")

##############################
## create country code conversion table only for existent countries
all.countries <- data.frame(unique(data.progrescase$CountryAsylum))
colnames(all.countries) <- "P_Code"
country <- merge(all.countries, country.codes, all.x=TRUE)
country$ISO_Alpha_3Code <- as.factor(country$ISO_Alpha_3Code)
country$ISO_Alpha_3Code <- factor(country$ISO_Alpha_3Code)

## exceptions in dataframe "country" which would lead to breakup in loop
country<- country[!country$P_Code == "ALG", ] #no idprogres in geojson
country<- country[!country$P_Code == "GCC", ] #
country<- country[!country$P_Code == "MAU", ] #needs to be checked in adm2
country<- country[!country$P_Code == "ISR", ] #remove as long as geojson has different columnnames
country<- country[!country$P_Code == "LBY", ] #no geojson adm2
country<- country[!country$P_Code == "MOR", ] #no geojson adm2
country<- country[!country$P_Code == "TUN", ] #no geojson adm2

## temporally excluded for better performance
country<- country[!country$P_Code == "IRN", ] #no geojson adm2
country<- country[!country$P_Code == "SYR", ] #no geojson adm2
country<- country[!country$P_Code == "TUR", ] #no geojson adm2
country<- country[!country$P_Code == "YEM", ] #no geojson adm2

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
  
  ## 1) country subset for different admin levels
  df_CountryAsylum <- data.progrescase[grep(pcode, data.progrescase$CountryAsylum), ]
  adm1 <- df_CountryAsylum[grep(pcode, df_CountryAsylum$coal1id), ] #only rows where adm0 and adm1 are consistent
  adm2 <- adm1[grep(pcode, adm1$coal2id), ] #only rows where adm1 and adm2 are consistent


  ## 2) let's check how much percent of data is consistent within each country and adminlevel
  ## rows where CountryAsylum has countrycode is considered as total
  ## create empty consistency table and fill with accuracy percentage data
  total <- nrow(df_CountryAsylum)
  pct.adm1 <- round((nrow(adm1)/total)*100, digits = 1) # if nrow country codes is considered as total 
  pct.adm2 <- round((nrow(adm2)/total)*100, digits = 1) # if nrow country codes is considered as total 
  consistency.table <- rbind(consistency.table, data.frame(iso3, total, pct.adm1, pct.adm2))
  
  
  ## 3) adjust column names of key columns for successful join with geojson
  names(adm1)[names(adm1) == 'coal1id'] <- 'idprogres'
  names(adm2)[names(adm2) == 'coal2id'] <- 'idprogres'
  
  
  ## 4) loading geojson
  ## admin level 1
  
  ## create output folder & file path
  mainDir <- "data"
  subDir <- "/mapping/geojson"
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  filename <- paste0(iso3,"_ADM1.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM1.geojson")
  
  ## only download geojson if not existing for better performance
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
  
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
  
  
  ## p-code repository needs to be consistent in case of id's (sometimes idadm1 is used sometimes idadm2)
  ## admin level 2
  filename <- paste0(iso3,"_ADM2.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM2.geojson")
  
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
  
  json.raw <- geojson_read( destfilepath, method="local", what="sp")
  
  proj4string(json.raw) # describes current coordinate reference system: here "+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"
  json.raw <- spTransform(json.raw, CRS("+proj=longlat +datum=WGS84")) # change in this system to match with basemap later
  
  json.raw <- gBuffer(json.raw, byid=TRUE, width=0)
  
  json.raw@data$id = rownames(json.raw@data)
  map.data.fortified <- fortify(json.raw, region = "id")
  map.data <- plyr::join(map.data.fortified, json.raw@data, by="id") #joining dataframe with geojson-data
  map.data.adm2 <- map.data[, c("long", "lat", "group", "hole", "admname", "iso3", "idprogres")] # subset with only needed columns to accelerate performance
  
  
  
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
  
  
  ## create output folder for maps
  mainDir <- "out"
  subDir <- paste0("/maps/",country[i,2],"/adm1/data_viz")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  subDir <- paste0("/maps/",country[i,2],"/adm2/data_viz")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  ## create output folder for maps with only Syrians
  subDir <- paste0("/maps/",country[i,2],"/adm1/only_syrian")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  subDir <- paste0("/maps/",country[i,2],"/adm2/only_syrian")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  ## create output folder for detail maps
  subDir <- paste0("/maps/",country[i,2],"/adm1/detail_maps")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  subDir <- paste0("/maps/",country[i,2],"/adm2/detail_maps")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  ## create output folder for further analysis
  subDir <- paste0("/maps/",country[i,2],"/adm1/analysis")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
  
  subDir <- paste0("/maps/",country[i,2],"/adm2/analysis")
  dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)
}
  


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
      panel.spacing = unit(c(-.1,1,0.02,1), "cm"),
      plot.caption = element_text(size = 9, hjust = 0, color = "#595851"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

## theme for main choropleth map
theme.choropleth <- function(...) {
  theme(
    plot.title = element_text(hjust = 0, color = "black", size = 19, face="bold"),
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
    plot.margin = unit(c(.5,2,1,1.5), "cm"),
    
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