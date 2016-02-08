
source("code/0-packages.R")

## Exploration of data will provide a quick overview 

#rm(progres.case.sp.dep.rst)
data <- read.csv("data/progrescase2.csv")

## Reorder factor levels
data$dependency <-  factor(data$dependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$youthdependency <-  factor(data$youthdependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$elederndependency <-  factor(data$elederndependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$female.ratio <-  factor(data$female.ratio, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$STDEVAgeclass <-  factor(data$STDEVAgeclass, levels = c( "0", "(0.001,5]", "(5,10]", "(10,15]", "(15,20]", "(20,Inf]"))
data$AVGAgecohort <-  factor(data$AVGAgecohort, levels = c( "0", "(0.1,18]", "(18,25]", "(25,35]", "(35,45]","(45,59]", "(59,Inf]"))
data$season <- factor(data$season, levels = c("Spring", "Summer", "Autumn", "Winter"))
data$CountryOriginCategory <- factor(data$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH"))
data$Montharrival <- factor(data$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))
data$edu_highestcat <- factor(data$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
data$dem_marriagecat <- factor(data$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced","Unknown"))
data$occupationcat <- factor(data$occupationcat, levels = c("Manager-Professional-Technician-Technician", "ServiceMarket",
                                                            "Agricultural", "Craft-Machine", "Elementary", "Military",
                                                            "UnknownOccup", "NoOccup", "Student"))



data.str <- strtable(data, factor.values=as.integer)


################################################################
## Cross tabulation of Household demographics
################################################################

## Cross tabulation between age cohort of head of Household and average age cohort of household
prop.table(table(data$agecohort, data$AVGAgecohort, useNA="always"),1)*100
CrossTable(data$agecohort, data$AVGAgecohort, prop.r=FALSE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE,
           missing.include=TRUE, digits=1, chisq=TRUE)

## Cross tabulation between average age and standard deviation of age cohort of household
prop.table(table(data$AVGAgecohort, data$STDEVAgeclass, useNA="always"),1)*100

## Cross tabulation between Year and Season of arrival
prop.table(table(data$season, data$YearArrivalCategory, useNA="always"),1)*100

## Cross tabulation between gender and marital Status of head of Household
prop.table(table(data$dem_marriage, data$dem_sex, useNA="always"),1)*100

## Cross tabulation of Dependency and Female Ratio
prop.table(table(data$female.ratio, data$dependency, useNA="always"),1)*100

## Cross tabulation of Dependency per type
prop.table(table(data$youthdependency, data$elederndependency, useNA="always"),1)*100

## Cross tabulation of Occupation & Education
prop.table(table(data$edu_highestcat, data$occupationcat, useNA="always"),1)*100
prop.table(table( data$occupationcat, data$edu_highestcat, useNA="always"),1)*100

## Cross tabulation of Occupation & Country of Origin
prop.table(table(data$CountryOriginCategory, data$occupationcat, useNA="always"),1)*100
prop.table(table( data$occupationcat, data$CountryOriginCategory, useNA="always"),1)*100


## Cross tabulation of Occupation & Country of Asylum
prop.table(table(data$CountryAsylum, data$occupationcat, useNA="always"),1)*100
prop.table(table( data$occupationcat, data$CountryAsylum, useNA="always"),1)*100

## Cross tabulation of Education & Country of Asylum
prop.table(table(data$CountryAsylum, data$edu_highestcat, useNA="always"),1)*100
prop.table(table( data$edu_highestcat, data$CountryAsylum, useNA="always"),1)*100


## Cross tabulation of Arrival year & Country of Origin
prop.table(table(data$CountryOriginCategory, data$edu_highestcat, useNA="always"),1)*100
prop.table(table( data$edu_highestcat, data$CountryOriginCategory, useNA="always"),1)*100

## Cross tabulation of Arrival year & Country of Origin
prop.table(table(data$CountryOriginCategory, data$YearArrivalCategory, useNA="always"),1)*100
prop.table(table( data$YearArrivalCategory, data$CountryOriginCategory, useNA="always"),1)*100


################################################################
## Cross tabulation of Specific needs
################################################################

prop.table(table(data$Case.size, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Disability, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Family.unity, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Older.person.at.risk, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Pregnant.or.lactating, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Serious.medical.condition, useNA="always"),1)*100
prop.table(table(data$Case.size, data$SGBV, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Single.parent, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Torture, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Unaccompanied.or.separated.child, useNA="always"),1)*100
prop.table(table(data$Case.size, data$Woman.at.risk, useNA="always"),1)*100



#############################################################################
## Correlation between specific need & Household characteristics
#############################################################################

#   "Child.at.risk", "Disability", "Family.unity", "Older.person.at.risk",
#  "Pregnant.or.lactating", "Serious.medical.condition", "SGBV", "Single.parent",
#  "Specific.legal.physical.protection.needs", "Torture", "Unaccompanied.or.separated.child",
#  "Woman.at.risk"

##############################
## Looking at contengency for Child at risk
prop.table(table(data$female.ratio, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$occupationcat, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$edu_highestcat, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$CountryAsylum, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$CountryOriginCategory, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$YearArrivalCategory, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$dem_marriage, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$dem_sex, data$Child.at.risk, useNA="always"),1)*100



##############################
## Looking at contengency for Woman at risk
prop.table(table(data$female.ratio, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$occupationcat, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$edu_highestcat, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$CountryAsylum, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$CountryOriginCategory, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$YearArrivalCategory, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$dem_marriage, data$Woman.at.risk, useNA="always"),1)*100
prop.table(table(data$dem_sex, data$Woman.at.risk, useNA="always"),1)*100

##############################
## Looking at contengency for Torture
prop.table(table(data$female.ratio, data$Torture, useNA="always"),1)*100
prop.table(table(data$occupationcat, data$Torture, useNA="always"),1)*100
prop.table(table(data$edu_highestcat, data$Torture, useNA="always"),1)*100
prop.table(table(data$CountryAsylum, data$Torture, useNA="always"),1)*100
prop.table(table(data$CountryOriginCategory, data$Torture, useNA="always"),1)*100
prop.table(table(data$YearArrivalCategory, data$Torture, useNA="always"),1)*100
prop.table(table(data$dem_marriage, data$Torture, useNA="always"),1)*100
prop.table(table(data$dem_sex, data$Torture, useNA="always"),1)*100

##############################
## Looking at contengency for Specific.legal.physical.protection.needs
prop.table(table(data$female.ratio, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$occupationcat, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$edu_highestcat, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$CountryAsylum, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$CountryOriginCategory, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$YearArrivalCategory, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$dem_marriage, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$dem_sex, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100

##############################
## Looking at contengency for SGBV
prop.table(table(data$female.ratio, data$SGBV, useNA="always"),1)*100
prop.table(table(data$occupationcat, data$SGBV, useNA="always"),1)*100
prop.table(table(data$edu_highestcat, data$SGBV, useNA="always"),1)*100
prop.table(table(data$CountryAsylum, data$SGBV, useNA="always"),1)*100
prop.table(table(data$CountryOriginCategory, data$SGBV, useNA="always"),1)*100
prop.table(table(data$YearArrivalCategory, data$SGBV, useNA="always"),1)*100
prop.table(table(data$dem_marriage, data$SGBV, useNA="always"),1)*100
prop.table(table(data$dem_sex, data$SGBV, useNA="always"),1)*100





#############################################################################
## Correlation between specific need & Events
#############################################################################
## looking at correlation between categorical variables

table(data$Family.unity, data$rst.Family.reunification,deparse.level=2,  useNA="always")
prop.table(table(data$Family.unity, data$rst.Family.reunification,deparse.level=1,  useNA="always"),1) # affichage % en ligne
ftable(data$Family.unity, data$rst.Family.reunification,deparse.level=2,  useNA="always")



#############################################################################
## Test of independance betwee vw
#############################################################################


chisq.test(data$edu_highestcat, data$occupationcat, correct=FALSE)
chisq.test(data$CountryOriginCategory, data$occupationcat, correct=FALSE)

##fisher.test(data$function_surv, data$skillsurv_surv)

## Testing independance of Case size to other variables

chisq.test(data$Case.size, data$dem_marriage, correct=FALSE)
chisq.test(data$Case.size, data$dem_sex, correct=FALSE)
chisq.test(data$Case.size, data$season, correct=FALSE)
chisq.test(data$Case.size, data$YearArrivalCategory, correct=FALSE)
chisq.test(data$Case.size, data$occupationcat, correct=FALSE)
chisq.test(data$Case.size, data$edu_highestcat, correct=FALSE)
chisq.test(data$Case.size, data$CountryAsylum, correct=FALSE)
chisq.test(data$Case.size, data$CountryOriginCategory, correct=FALSE)
chisq.test(data$Case.size, data$agecohort, correct=FALSE)
chisq.test(data$Case.size, data$AVGAgecohort, correct=FALSE)
chisq.test(data$Case.size, data$STDEVAgeclass, correct=FALSE)
chisq.test(data$Case.size, data$dependency, correct=FALSE)
chisq.test(data$Case.size, data$youthdependency, correct=FALSE)
chisq.test(data$Case.size, data$elederndependency, correct=FALSE)
chisq.test(data$Case.size, data$female.ratio, correct=FALSE)



