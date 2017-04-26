###########################################################
### This scripts organise folders and load geodata from the repo


#############################################################################
## Create folders

## create output folder & file path
mainDir <- "data"
subDir <- "/mapping/geojson"
dir.create(file.path(mainDir, subDir), showWarnings = FALSE, recursive=TRUE)




## clear workspace except necessary data frame function
#keep(data.progrescase, sure = TRUE)


##############################
## add iso3 countrycodes for merge with geojson later
#country <- read.csv("data/countrycodes.csv")

##############################
## create country code conversion table only for existent countries

#country$ISO_Alpha_3Code <- as.factor(country$ISO_Alpha_3Code)
#country$ISO_Alpha_3Code <- factor(country$ISO_Alpha_3Code)

## exceptions in dataframe "country" which would lead to breakup in loop
#country<- country[!country$P_Code == "ALG", ] #no idprogres in geojson
#country<- country[!country$P_Code == "GCC", ] #
#country<- country[!country$P_Code == "MAU", ] #needs to be checked in adm2
#country<- country[!country$P_Code == "ISR", ] #remove as long as geojson has different columnnames
#country<- country[!country$P_Code == "LBY", ] #no geojson adm2
#country<- country[!country$P_Code == "MOR", ] #no geojson adm2
#country<- country[!country$P_Code == "TUN", ] #no geojson adm2

## temporally excluded for better performance
#country<- country[!country$P_Code == "IRN", ] #no geojson adm2
#country<- country[!country$P_Code == "SYR", ] #no geojson adm2
#country<- country[!country$P_Code == "TUR", ] #no geojson adm2
#country<- country[!country$P_Code == "YEM", ] #no geojson adm2

#country<- country[country$P_Code %in% c("LBN", "JOR", "IRQ", "ARE")   , ]
#############################################################################################
## Donwload geojson from Repo

for (i in 1:nrow(country)) {
  iso3 <- country[i,2]
  filename <- paste0(iso3,"_ADM1.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM1.geojson")
  ## only download geojson if not existing for better performance
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
  
  filename <- paste0(iso3,"_ADM2.geojson")
  destfilepath <- paste0("data/mapping/geojson/", filename )
  geojsonurl <- paste0("http://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",iso3,"/ADM2.geojson")
  
  if(!file.exists(destfilepath)){
    res <- tryCatch(download.file(geojsonurl, destfile=destfilepath ),
                    error=function(e) 1)
  }
  
}

##################################################################################
## create output folder for maps
for (i in 1:nrow(country)) {
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


rm(destfilepath, filename, geojsonurl, i, iso3, mainDir, subDir)