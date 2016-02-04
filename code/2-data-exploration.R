
source("code/0-packages.R")

## Exploration of data will provide a quick overview 

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")

data <- progres.case.sp.dep.rst


data.str <- strtable(data, factor.values=as.integer)


### Exploring first correlations

## looking at correlation between categorical variables

table(data$Family.unity, data$rst.Family.reunification,deparse.level=2,  useNA="always")
prop.table(table(data$Family.unity, data$rst.Family.reunification,deparse.level=1,  useNA="always"),1) # affichage % en ligne
ftable(data$Family.unity, data$rst.Family.reunification,deparse.level=2,  useNA="always")

## Cross tabulation of Occupation & Country of Origin
prop.table(table(data$CountryOriginCategory, data$occupationcat, useNA="always"),1)*100
prop.table(table( data$occupationcat, data$CountryOriginCategory, useNA="always"),1)*100

## Cross tabulation of Occupation & Country of Asylum
prop.table(table(data$CountryAsylum, data$occupationcat, useNA="always"),1)*100
prop.table(table( data$occupationcat, data$CountryAsylum, useNA="always"),1)*100

## Cross tabulation of Education & Country of Origin
prop.table(table(data$CountryOriginCategory, data$edu_highestcat, useNA="always"),1)*100
prop.table(table( data$edu_highestcat, data$CountryOriginCategory, useNA="always"),1)*100

## Cross tabulation of Education & Country of Asylum
prop.table(table(data$CountryAsylum, data$edu_highestcat, useNA="always"),1)*100
prop.table(table( data$edu_highestcat, data$CountryAsylum, useNA="always"),1)*100

## Cross tabulation of Education & Country of Origin
prop.table(table(data$CountryOriginCategory, data$YearArrivalCategory, useNA="always"),1)*100
prop.table(table( data$YearArrivalCategory, data$CountryOriginCategory, useNA="always"),1)*100


########
## Cross tabulation of Specific needs
prop.table(table(data$Num_Inds2, data$Child.at.risk, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Disability, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Family.unity, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Older.person.at.risk, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Pregnant.or.lactating, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Serious.medical.condition, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$SGBV, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Single.parent, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Specific.legal.physical.protection.needs, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Torture, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Unaccompanied.or.separated.child, useNA="always"),1)*100
prop.table(table(data$Num_Inds2, data$Woman.at.risk, useNA="always"),1)*100


#   "Child.at.risk", "Disability", "Family.unity", "Older.person.at.risk",
#  "Pregnant.or.lactating", "Serious.medical.condition", "SGBV", "Single.parent",
#  "Specific.legal.physical.protection.needs", "Torture", "Unaccompanied.or.separated.child",
#  "Woman.at.risk"

## Test 
chisq.test(data$edu_highestcat, data$occupationcat, correct=FALSE)
chisq.test(data$CountryOriginCategory, data$occupationcat, correct=FALSE)

##fisher.test(data$function_surv, data$skillsurv_surv)

## Testing independance of Case size to other variables

chisq.test(data$Num_Inds2, data$dem_marriage, correct=FALSE)
chisq.test(data$Num_Inds2, data$dem_sex, correct=FALSE)
chisq.test(data$Num_Inds2, data$season, correct=FALSE)
chisq.test(data$Num_Inds2, data$YearArrivalCategory, correct=FALSE)
chisq.test(data$Num_Inds2, data$occupationcat, correct=FALSE)
chisq.test(data$Num_Inds2, data$edu_highestcat, correct=FALSE)
chisq.test(data$Num_Inds2, data$CountryAsylum, correct=FALSE)
chisq.test(data$Num_Inds2, data$CountryOriginCategory, correct=FALSE)
chisq.test(data$Num_Inds2, data$agecohort, correct=FALSE)
chisq.test(data$Num_Inds2, data$AVGAgecohort, correct=FALSE)
chisq.test(data$Num_Inds2, data$STDEVAgeclass, correct=FALSE)
chisq.test(data$Num_Inds2, data$dependency, correct=FALSE)
chisq.test(data$Num_Inds2, data$youthdependency, correct=FALSE)
chisq.test(data$Num_Inds2, data$elederndependency, correct=FALSE)



