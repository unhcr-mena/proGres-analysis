#############################################################
### Loading required packages & Functions
#############################################################
source("code/0-packages.R")

#############################################################
### Recoding the Case level Information
#############################################################
source("code/1-Recode-data-Case.R")


#############################################################
## Select the country to be mapped

country <- read.csv("data/countrycodes.csv")
country$ISO_Alpha_3Code <- as.factor(country$ISO_Alpha_3Code)
country$ISO_Alpha_3Code <- factor(country$ISO_Alpha_3Code)

country<- country[country$P_Code %in% c("LBN", "JOR", "IRQ", "ARE")   , ]
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




#############################################################
### Organising folders and getting linked geodata from the repo
#############################################################
source("code/8-2-getgeodata.R")

#############################################################
### Aggregating information at admin level and merging with shape
### Results are saved within list
#############################################################
source("code/8-2-Maps-spatial-aggregation.R")

#############################################################
### Generating all maps from the related list for level 1 & Level 2
#############################################################
source("code/8-3-Maps_Basic_Adm1.R")
source("code/8-3-Maps_Basic_Adm2.R")











