## This script allows to generate stratified samples using proportional allocation

#############################################################
## Install & load Required Library
#############################################################

### uncomment the next tow lines at first utilisation
#install.packages("simFrame")
#install.packages("sampling")

library(simFrame)
library(sampling)


#############################################################
## Get data
#############################################################

#rm(progres.case)
progres.case.sp <- read.csv("data/progrescase-1.csv")

dictionnary <- strtable(progres.case.sp)
write.csv(dictionnary, file = "data/dictionnary.csv", na="", row.names = FALSE)
#str(progres.case)


prop.table(table(progres.case.sp$familyprofile, progres.case.sp$CountryAsylum, useNA = "ifany"),2)
prop.table(table(progres.case.sp$familyprofile, progres.case.sp$CountryOrigin, useNA = "ifany"))
prop.table(table(progres.case.sp$familyprofile, progres.case.sp$CountryOriginCategory, useNA = "ifany"))

## Subset on the country we are interested in...
this.country <- "MOR"
progres.case.thiscountry <- progres.case.sp[progres.case.sp$CountryAsylum==this.country, ]


#############################################################
## Work on the variables and the dataset
#############################################################
data <- progres.case.thiscountry 
data.bckp1 <- data
source ("code/recategorise_MOR.R")

#names(data)
## Check proportion in the 4 stratas
#prop.table(table(data$CountryOrigin, useNA = "ifany"))
#prop.table(table(data$coal2Cat, useNA = "ifany"))
#prop.table(table(data$familyprofile, useNA = "ifany"))
#prop.table(table(data$dem_sex, useNA = "ifany"))
#prop.table(table(data$familyprofile, data$dem_sex, useNA = "ifany"))

## Check that categories works well....
#prop.table(table(data$CountryOrigin2, data$coal2Cat2, data$familyprofile2, data$dem_sex2, useNA = "ifany"))
#table(data$CountryOrigin2, data$coal2Cat2, useNA = "ifany")
#prop.table(table(data$CountryOrigin2, data$coal2Cat2, useNA = "ifany"))
#prop.table(table(data$CountryOrigin2, data$coal1Cat2, useNA = "ifany"))
#proportion1 <- as.data.frame(table(data$CountryOrigin2, data$coal2Cat2, data$familyprofile2, data$dem_sex2, useNA = "ifany"))
#proportion2 <- as.data.frame(table(data$CountryOrigin2, data$coal2Cat2,  data$dem_sex2, useNA = "ifany"))

#############################################################
## Check phone issue
#############################################################
## Need some manual cleaning for those without phones
#write.csv(unique(progres.case.thiscountry$phone),"data/nophone.csv", row.names = FALSE )

## now reading the clean
nophone <- read.csv("data/nophone2.csv")
nophone <- as.character(nophone$nophone)

# first we subset the dataset in order to have only observations with a phone
#data <- progres.case.thiscountry2[ which(!(is.na(progres.case.thiscountry2$phone))), ]
data <- data[ !(data$phone %in% nophone) , ]
data <- data[data$phone !='' , ]


#############################################################
## Configure sampling variables
#############################################################

#Confirm the the population size called here N
## This is the total number of people in the group you are trying to reach with the survey. 
N <- nrow(progres.case.thiscountry)

#  Decide on the confidence level
#  It represents the probability of the same result if you re-sampled, all other things equal.
# A measure of how certain you are that your sample accurately reflects the population, within its margin of error.
## Common standards used by researchers are 90%, 95%, and 99%.
cl <- 0.95
z <- abs(qt((1-cl)/2, (N-1)))

# Decide on the margin of error - Precision  
# This percentage describes the variability of the estimate: how closely the answer your sample gave is
# to the "true value" is in your population. 
# The smaller the margin of error is, the closer you are to having the exact answer at a given confidence level.
# A smaller margin of error means that you must have a larger sample size given the same population.
## Common standards used by researchers are: ± 5%, ± 3% , ± 1%)
e <- 0.025

#fill the proportion of the attribute
## Estimate of the prevalence or mean & STDev of the key indicator (e.g. 30% retunr intention). 
#### Prevalence is the total number of cases for a variable of interest that is typically binary 
#### within a population divided by its total population (for instance intention to return). 
#### Mean is the expected value of a variable of interest that is typically continuous 
#### within a prescribed range for a given population (for instance expenditure per case)
p <- 0.5
q <- 1-p

#compute the sample size for a large population
n0 <- (z^2)*p*q/(e^2)
n0 <- round(n0, digits = 0)

#compute the sample size for a finite population
N <- nrow(progres.case.thiscountry)

n <- n0/(1+((n0-1)/N))
n <- round(n, digits = 0)

#############################################################
## Stratify the dataset using proportional allocation
#############################################################
data$stratum <- paste(data$CountryOrigin2, data$coal2Cat2, data$familyprofile2,  data$dem_sex)
#data$stratum <- paste(data$CountryOrigin2, data$coal2Cat2, data$familyprofile2)
#data$stratum <- paste(data$CountryOrigin2, data$coal2Cat2)
data.bckp2 <- data
### checking allocation per stratum
stratum <- as.data.frame(table(data$stratum)) 

### In order to avoid errors during sampling - we need to take out statrum where there's not enough cases
## R strata "arguments imply differing number of rows: 0, 1"
## https://stackoverflow.com/questions/14735411/strata-from-sampling-returns-an-error-arguments-imply-differing-number-of-r
stratum.keep <- as.character(stratum[ stratum$Freq >1, c("Var1") ])
data <- data[ data$stratum %in% stratum.keep, ]

# levels(as.factor(data$stratum))

# We build the 'Strata' object
st <- stratify(data, c("stratum"))
#summary(st)
#str(st)
## Numbers of strata
#nrow(as.data.frame((n_size)))
#max(st@nr)

#compute the sample sizes of the strata using proportional allocation: nh = Nh/N*n for each strata h
n_size <- numeric(max(st@nr))
for (h in 1:max(st@nr)){
  n_size[h] <- st@size[h]/N*n
  n_size[h] <- round(n_size[h], digits = 0)
}
#print(n_size)
#############################################################
## Now pull the sample
#############################################################

#Using various sampling approach to select the sample

stratified_sample.srswor <- strata(data[order(data$stratum),  ], 
                                   stratanames = c("stratum"),
                                   size= n_size,
                                   method="srswor",  ### Simple random sampling without replacement (srswor), 
                                   description=TRUE)

stratified_sample.srswr <- strata(data[order(data$stratum),  ], 
                                   stratanames = c("stratum"),
                                   size= n_size,
                                   method="srswr",  ### Simple random sampling with replacement (srswr), 
                                   description=TRUE)

#stratified_sample.poisson <- strata(data[order(data$stratum),  ], 
#                                   stratanames = c("stratum"),
#                                   size= n_size,
#                                   method="poisson", ### Poisson sampling (poisson), 
#                                   description=TRUE)

#stratified_sample.systematic <- strata(data[order(data$stratum),  ], 
#                                   stratanames = c("stratum"),
#                                   size= n_size,
#                                   method="systematic", ### systematic sampling (systematic);
#                                   description=TRUE)

#traceback()

#summary(stratified_sample)
#names(data)
data.sample <- data[order(data$stratum), c("stratum","CountryOrigin2","coal2Cat2", "familyprofile2","dem_sex2","CaseNo", "phone", "coal1", "coal2" , "coal3", "Num_Inds") ]
rownames(data.sample ) <- NULL
data.sample$ID_unit <- row.names(data.sample)
#names(data.sample)
## check we have unique ID
nrow(data.sample)
nrow(as.data.frame(unique(data$CaseNo)))

## Now merge
#data_sampled <- getdata(data.sample, stratified_sample.srswr)
#names(stratified_sample.srswr)

## If Simple random sampling with replacement, we have duplicated ID
nrow(as.data.frame(unique(stratified_sample.srswr$ID_unit)))
nrow(as.data.frame(unique(stratified_sample.srswor$ID_unit)))

data_sampled <- merge(x=data.sample, y=stratified_sample.srswor, by="ID_unit", all.y=TRUE )
## check we have unique ID
nrow(data_sampled)
nrow(as.data.frame(unique(data_sampled$CaseNo)))
#print(data_sampled)
write.csv(data_sampled, "data_sampled.csv")


