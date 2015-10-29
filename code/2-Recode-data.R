### Recoding the dataset
library("car")
rm(progres.case)
progres.case <- read.csv("data/progrescase.csv")

#str(progres.case)


## Integer as factor
progres.case$Num_Inds2 <- as.factor(progres.case$Num_Inds)
progres.case$YearArrivalf <- as.factor(progres.case$YearArrival)


## Calucalting dependency ration

progres.case$dependency <- (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64
progres.case$youthdependency <- progres.case$Child_0_14 / progres.case$Work_15_64
progres.case$elederndependency <- progres.case$Eldern_65 / progres.case$Work_15_64

## Adding Age cohort

## Aggregating season according to month


progres.case$season <- progres.case$Montharrival
#levels(progres.case$Montharrival)
progres.case$season <- recode(progres.case$season,"'March'='Spring'; 'April'='Spring';    'May'='Spring';
                              'June'='Summer'; 'July'='Summer';  'August'='Summer'; 
                              'September'='Autumn'; 'October'='Autumn'; 'November'='Autumn'; 
                              'January'='Winter';  'February'='Winter'; 'December'='Winter' ")

progres.case$season <- factor(progres.case$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

progres.case$Montharrival <- factor(progres.case$Montharrival, levels = c("March","April","May","June","July","August","September","October","November","December","January","February"))


progres.case$edu_highest_t <- progres.case$edu_highest
progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'01' = 'E1year'; '02' = 'E2year';  
                                     '03' = 'E3year';  '04' = 'E4year';   '05' = 'E5year';  
                                     '06' = 'E6year';   '07' = 'E7year';  '08' = 'E8year';  
                                     '09' = 'E9year';    '10' = 'E10year';  '11' = 'E11year'; 
                                     '12' = 'E12year';    '13' = 'E13year';   '14' = 'E14year';  
                                     'IN' = 'InformEduc';    'NE' = 'NoEduc'; 'U' = 'Unknown'; 
                                     'TC' = 'TechnVoc';     'UG' = 'Univ'; 'PG' = 'PostUniv' ")

progres.case$edu_highest_t <- factor(progres.case$edu_highest_t, levels = c("Unknown", "NoEduc", "InformEduc", "E1year", "E2year", "E3year", "E4year", "E5year", "E6year", "E7year", "E8year", "E9year", "E10year", "E11year", "E12year", "E13year", "E14year", "TechnVoc", "Univ", "PostUniv"))



## Extracting main ocupation category from occupation code 

summary(progres.case$occupationcode)
progres.case$occupationcat <- "UnknownOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0001"] <- "Military"
progres.case$occupationcat[progres.case$occupationcode ==  "None"] <- "NoOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0110"] <- "Student"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "1"] <- "Manager"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "2"] <- "Professional"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "3"] <- "Technician"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "4"] <- "Clerk"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "5"] <- "ServiceMarket"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "6"] <- "Agricultural"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "7"] <- "Craft"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "8"] <- "Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "9"] <- "Elementary"

progres.case$occupationcat <- factor(progres.case$occupationcat, levels = c("Manager", "Professional", "Technician", "Clerk", "ServiceMarket", "Agricultural", "Craft", "Machine", "Elementary", "Military", "UnknownOccup", "NoOccup", "Student"))
#summary(progres.case$occupationcat)



#progres.case$occupationcat <- substr(progres.case$occupationcode, 1,1 )
#str(progres.case$occupationcat)



##########
# Age group:
progres.case$dem_age_grp1 <- ifelse(progres.case$dem_age < 35, 1, 0)

progres.case$dem_age_grp2 <- ifelse((progres.case$dem_age >= 35) &
                                      (progres.case$dem_age < 55), 1, 0)

progres.case$dem_age_grp3 <- ifelse(progres.case$dem_age >= 55, 1, 0)

progres.case$dem_PA_grp0 <- ifelse(progres.case$dem_age < 15, 1, 0)

progres.case$dem_PA_grp1 <- ifelse(progres.case$dem_age < 18, 1, 0)

progres.case$dem_PA_grp2 <- ifelse((progres.case$dem_age > 17) &
                                     (progres.case$dem_age < 60), 1, 0)

progres.case$dem_PA_grp3 <- ifelse(progres.case$dem_age > 59, 1, 0)


progres.case$age.PA1 <- ifelse(progres.case$dem_age < 35, 1, 0)

progres.case$age.PA2 <- ifelse((progres.case$dem_age > 34) &
                                 (progres.case$dem_age < 55), 1, 0)

progres.case$age.PA3 <- ifelse(progres.case$dem_age > 54, 1, 0)

# Percentage of children:
progres.case$p.child.grp1 <- ifelse(progres.case$percentage_0_14 == 0, 1, 0)

progres.case$p.child.grp2 <- ifelse((progres.case$percentage_0_14 > 0) &
                                      (progres.case$percentage_0_14 < 50), 1, 0)

progres.case$p.child.grp3 <- ifelse((progres.case$percentage_0_14 >= 50) &
                                      (progres.case$percentage_0_14 < 75), 1, 0)

progres.case$p.child.grp4 <- ifelse(progres.case$percentage_0_14 >= 75, 1, 0)



##########
# Marital status:
progres.case$mar_widow <- ifelse(progres.case$dem_marriage == "WD Widowed", 1, 0) # niente
progres.case$mar_single <- ifelse(progres.case$dem_marriage == "SN Single", 1, 0) # significativa e buon R2
progres.case$mar_divorced <- ifelse(progres.case$dem_marriage == "DV Divorced", 1, 0) # significativa ma poco R2
progres.case$mar_married <- ifelse(progres.case$dem_marriage == "MA Married", 1, 0) # significative e OK R2
progres.case$mar_engaged <- ifelse(progres.case$dem_marriage == "EG Engaged", 1, 0) # significativa ma poco R2
progres.case$mar_g_divorced <- ifelse((progres.case$dem_marriage == "DV Divorced" | progres.case$dem_marriage == "SR Separated"), 1, 0) # significativa ma poco R2
progres.case$mar_g_married <- ifelse((progres.case$dem_marriage == "MA Married" | progres.case$dem_marriage == "CL Common Law Married" | progres.case$dem_marriage == "EG Engaged"), 1, 0) # significativa e OK R2 ma meno rispetto a married-only


##########
# Ethnicity, religion, birth:
progres.case$ethn_arab <- ifelse(progres.case$dem_ethn == "Arab", 1, 0)
progres.case$rel_sunni <- ifelse(progres.case$dem_religion == "SUN Sunni", 1, 0)
progres.case$bir_syria <- ifelse(progres.case$dem_birth_country == "SYR", 1, 0)

##########
# Gender PA:
progres.case$gender.male <- ifelse(progres.case$dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(progres.case$dem_sex == "Female", 1, 0)

##########
# Educational attainment:
progres.case$edu.highest.grp1 <- ifelse((progres.case$edu_highest == "NE No education" | 
                                           progres.case$edu_highest == "KG Kindergarten" | 
                                           progres.case$edu_highest == "1 year (or Grade 1)" | 
                                           progres.case$edu_highest == "2 years (or Grade 2)" | 
                                           progres.case$edu_highest == "3 years (or Grade 3)" | 
                                           progres.case$edu_highest == "4 years (or Grade 4)" | 
                                           progres.case$edu_highest == "5 years (or Grade 5)"), 1, 0)

progres.case$edu.highest.grp2 <- ifelse((progres.case$edu_highest == "6 years (or Grade 6)" | 
                                           progres.case$edu_highest == "7 years (or Grade 7)" | 
                                           progres.case$edu_highest == "8 years (or Grade 8)"), 1, 0)

progres.case$edu.highest.grp3 <- ifelse((progres.case$edu_highest == "9 years (or Grade 9)" | 
                                           progres.case$edu_highest == "10 years (or Grade 10)" | 
                                           progres.case$edu_highest == "11 years (or Grade 11)") , 1, 0)

progres.case$edu.highest.grp4 <- ifelse((progres.case$edu_highest == "12 years (or Grade 12)" | 
                                           progres.case$edu_highest == "13 years (or Grade 13)" | 
                                           progres.case$edu_highest == "14 years (or Grade 14)") , 1, 0)

progres.case$edu.highest.grp5 <- ifelse((progres.case$edu_highest == "PG Post university level" | 
                                           progres.case$edu_highest == "UG University level"), 1, 0)


