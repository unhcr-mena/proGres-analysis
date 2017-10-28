## This script allows to generate stratified samples using proportional allocation

#rm(progres.case)
progres.case.sp <- read.csv("data/progrescase-1.csv")

dictionnary <- strtable(progres.case.sp)
#str(progres.case)

#############################################################
## STEP 1: Install & load Required Library
#############################################################

### uncomment the next tow lines at first utilisation
#install.packages("simFrame")
#install.packages("sampling")

library(simFrame)
library(sampling)

this.country <- "MOR"
progres.case.thiscountry <- progres.case.sp[progres.case.sp$CountryAsylum==this.country, ]

#############################################################
## STEP 2: Insert your configuration for the sample
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
e <- 0.05

#fill the proportion of the attribute
## Estimate of the prevalence or mean & STDev of the key indicator (e.g. 30% retunr intention). 

#### Prevalence is the total number of cases for a variable of interest that is typically binary 
#### within a population divided by its total population (for instance intention to return). 

#### Mean is the expected value of a variable of interest that is typically continuous 
#### within a prescribed range for a given population (for instance expenditure per case)
p <- 0.5
q <- 1-p


#############################################################
## STEP 3: Generate the variables and the dataset
#############################################################
# PS: note that you can skip that step if you have already your dataset



# estimate, through a logistic regression, the probability/propensity score of having a phone given a set of auxiliary variable known for both respondents and nonrespondentss (sex and age in this case)
logit_model <- glm(phone ~dem_sex+ dem_age,family=binomial(link='logit'),data=progres.case.thiscountry)
summary(logit_model)
#compute the weights as the inverse of the selection probabilites (they will be use in STEP 6)
weights <- 1/predict(logit_model)

dataprogres.case.thiscountry2 <- data.frame(dataprogres.case.thiscountry, weights)

#############################################################
## STEP 4: Calculate the sample size
#############################################################
#

#compute the sample size for a large population
n0 <- (z^2)*p*q/(e^2)
n0 <- round(n0, digits = 0)

#compute the sample size for a finite population
N <- nrow(progres.case.thiscountry)

n <- n0/(1+((n0-1)/N))
n <- round(n, digits = 0)


#############################################################
## STEP 5: Stratify the dataset using proportional allocation
#############################################################

# We build the 'Strata' object
# first we subset the dataset in order to have only observations with a phone
data_with_phone <- data[ which(!(is.na(progres.case.thiscountry$phone))), ]
st <- stratify(data_with_phone, c("Case.size", "needs"))
#summary(st)
str(st)
max(st@nr)

#compute the sample sizes of the strata using proportional allocation: nh = Nh/N*n for each strata h
n_size <- numeric(max(st@nr))
for (h in 1:max(st@nr)){
  n_size[h] <- st@size[h]/N*n
  n_size[h] <- round(n_size[h], digits = 0)
}
print(n_size)

#use a simple random or systematic sample to select your sample
data_with_phone <- data_with_phone[order(data_with_phone$size, data_with_phone$needs),]
stratified_sample <- strata(data_with_phone, c("size", "needs"), size=c(n_size), method="srswor")
summary(stratified_sample)
data_sampled <- getdata(data_with_phone, stratified_sample)
#print(data_sampled)
write.csv(data_sampled, "data_sampled.csv")

#check if the the sample is good by checkin if the proportion of the attribute in the sample is close to its population's counterpart (=p)
return_freq <- table(data_sampled$return)["1"]
return_relfreq <- return_freq / NROW(data_sampled$return)
#this is the first unadjusted estimator computed by assuming that phone ownership is completely random 
#and using only respondents with a phone:
print(return_relfreq)


####################################################################################
## STEP 6: Weight adjustment to correct for selection bias : no ownership of a phone
####################################################################################

#run again the stratified sampling but this time including the individuals who do not have a phone

# We build the 'Strata' object
st2 <- stratify(data, c("size", "needs"))
#summary(st)
str(st2)
max(st2@nr)

#compute the sample sizes of the strata using proportional allocation: nh = Nh/N*n for each strata h
n_size2 <- numeric(max(st2@nr))
for (h in 1:max(st2@nr)){
  n_size2[h] <- st2@size[h]/N*n
  n_size2[h] <- round(n_size2[h], digits = 0)
}
print(n_size2)

#use a simple random or systematic sample to select your sample
data[order(data$size, data$needs),]
stratified_sample2 <- strata(data, c("size", "needs"), size=c(n_size2), method="srswor")
summary(stratified_sample2)
data_sampled2 <- getdata(data, stratified_sample2)
#print(data_sampled)
write.csv(data_sampled2, "data_sampled2.csv")

#check if the the sample is good by checkin if the proportion of the attribute in the sample is close to its population's counterpart (=p)
#compute the estimated proportion of the attribute, by weighting 
return_est <- sum(data_sampled2$weights*data_sampled2$phone*data_sampled2$return, na.rm = TRUE)/sum(data_sampled2$weights*data_sampled2$phone, na.rm = TRUE)
#this is the second estimator, adjusted for the selection bias (ownership of a phone)
print(return_est) 

#you can now compare the 2 estimators: return_relfreq and return_est
