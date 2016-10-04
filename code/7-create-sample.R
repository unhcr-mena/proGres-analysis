

### The objective is to quickly create sample dataset extracted from the datawarehouse.

#install.packages("survey")
#install.packages("sampling")
#install.packages("SamplingStrata")
#install.packages("stratification")

library(sampling)

## https://www.jstatsoft.org/article/view/v061i04/v61i04.pdf
library(SamplingStrata)


############
## Example stratification
## We look at generating a sample dataset that presents stratification for 
## Education, Gender of the Principal Applicant 
############
# the variable "edu_highestcat" has 7 categories ("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"). 
# the variable "dem_sex" has 3 categories ('Male'; 'Female';'Unknown').

## Subset for the country
data.JOR <-data[data$CountryAsylum == "JOR", ]


# computes the population stratum sizes
table(data.JOR$edu_highestcat,data.JOR$dem_sex)
## size of the sample in each stratum is taken in proportion to the size of the stratum.
## This is called proportional allocation
table(droplevels(data.JOR$edu_highestcat),droplevels(data.JOR$dem_sex))

# This allows to define the best method to be used for the stratification among the following
### Simple random sampling without replacement (srswor), 
### Simple random sampling with replacement (srswr), 
### Poisson sampling (poisson), 
### systematic sampling (systematic);


sampled.data.JORa <- strata(data.JOR,c("edu_highestcat","dem_sex"), size=c(30,20,45,30,20,45,30,20,45,30,20,45), method="srswor")
sampled.data.JORb <- strata(data.JOR,c("edu_highestcat","dem_sex"), size=c(30,20,45,30,20,45,30,20,45,30,20,45), method="srswr")
sampled.data.JORc <- strata(data.JOR,c("edu_highestcat","dem_sex"), size=c(30,20,45,30,20,45,30,20,45,30,20,45), method="poisson")
sampled.data.JORd <- strata(data.JOR,c("edu_highestcat","dem_sex"), size=c(30,20,45,30,20,45,30,20,45,30,20,45), method="systematic")

# extracts the observed data
getdata(data.JOR,sampled.data.JOR)
# see the result using a contigency table
table(s$edu_highestcat,s$dem_sex)


####################################################################
### Using http://news.mrdwab.com/post/stratified/
source("code/stratified.R")

# Let's take a 10% sample from all edu_highestcat groups in data.JOR
sampled.data.JOR1 <- stratified(data.JOR, "edu_highestcat", 0.1)

# Let's take a 10% sample from only "Up to Grade 5" and "Grade 6-8" groups from -edu_highestcat- in data.JOR
sampled.data.JOR2 <- stratified(data.JOR, "edu_highestcat", 0.1, select = list(edu_highestcat = c("Up to Grade 5", "Grade 6-8")))

# Use a two-column strata: "edu_highestcat" and "dem_sex" 
## "edu_highestcat" varies more slowly, so it is better to put that first
sampled.data.JOR3 <- stratified(data.JOR, c("edu_highestcat","dem_sex"), size = 0.15)
sampled.data.JOR3 <- stratified(data.JOR, c("edu_highestcat","dem_sex"), size = 10)

# Use a three-column strata: "edu_highestcat","dem_sex" and "occupationcat"
sampled.data.JOR4 <- stratified(data.JOR, c("edu_highestcat","dem_sex", "occupationcat"), size = 2)

# How many samples were taken from each strata?
table(interaction(sampled.data.JOR4[c("edu_highestcat","dem_sex", "occupationcat")]))


# Can we verify the message about group sizes?
names(which(table(interaction(data.JOR[c("edu_highestcat","dem_sex", "occupationcat")])) < 2))
names(which(table(interaction(data.JOR[c("edu_highestcat","dem_sex")])) < 2))
