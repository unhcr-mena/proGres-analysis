
## Exploration of data will provide a quick overview 

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")

data <- progres.case.sp.dep.rst

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
prop.table(table(data$CountryOriginCategory, data$edu_highest_t, useNA="always"),1)*100
prop.table(table( data$edu_highest_t, data$CountryOriginCategory, useNA="always"),1)*100

## Cross tabulation of Education & Country of Asylum
prop.table(table(data$CountryAsylum, data$edu_highest_t, useNA="always"),1)*100
prop.table(table( data$edu_highest_t, data$CountryAsylum, useNA="always"),1)*100

## Cross tabulation of Education & Country of Origin
prop.table(table(data$CountryOriginCategory, data$YearArrivalCategory, useNA="always"),1)*100
prop.table(table( data$YearArrivalCategory, data$CountryOriginCategory, useNA="always"),1)*100


## Test 
chisq.test(data$edu_highest_t, data$occupationcat, correct=FALSE)
chisq.test(data$CountryOriginCategory, data$occupationcat, correct=FALSE)

##fisher.test(data$function_surv, data$skillsurv_surv)


