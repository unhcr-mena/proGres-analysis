
source("code/0-packages.R")

###########################################
### Recoding the Case level Information
###########################################

rm(progres.case)
progres.case <- read.csv("data/progrescase.csv")
#str(progres.case)

##############################
## Case size  as factor
progres.case$Case.size <- as.factor(progres.case$Num_Inds)
#Case.size <- as.data.frame(table(progres.case$Case.size))
#rm(Case.size)
progres.case$Case.size <- recode(progres.case$Case.size,"'1'='Case.size.1';
                                                                      '2'='Case.size.2';
                                                                      '3'='Case.size.3';
                                                                      '4'='Case.size.4';
                                                                      '5'='Case.size.5';
                                                                      '6'='Case.size.6';
                                                                      '7'='Case.size.7.and.more';
                                                                      '8'='Case.size.7.and.more';
                                                                      '9'='Case.size.7.and.more';
                                                                      '10'='Case.size.7.and.more';
                                                                      '11'='Case.size.7.and.more';
                                                                      '12'='Case.size.7.and.more';
                                                                      '13'='Case.size.7.and.more';
                                                                      '14'='Case.size.7.and.more';
                                                                      '15'='Case.size.7.and.more';
                                                                      '16'='Case.size.7.and.more';
                                                                      '17'='Case.size.7.and.more';
                                                                      '18'='Case.size.7.and.more';
                                                                      '19'='Case.size.7.and.more';
                                                                      '20'='Case.size.7.and.more'")

##############################
## Calculating dependency ration
progres.case$dependency <-  cut( (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$dependency <- as.character(progres.case$dependency)
progres.case$dependency[is.na(progres.case$dependency)]<- "0"
progres.case$dependency <- as.factor(progres.case$dependency)
progres.case$dependency <-  factor(progres.case$dependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
#<- relevel(sizes, "medium")
# , levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]")

progres.case$youthdependency <- cut(progres.case$Child_0_14 / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$youthdependency <- as.character(progres.case$youthdependency)
progres.case$youthdependency[is.na(progres.case$youthdependency)]<- "0"
progres.case$youthdependency <- as.factor(progres.case$youthdependency)
progres.case$youthdependency <-  factor(progres.case$youthdependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))

progres.case$elederndependency <- cut(progres.case$Eldern_65 / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$elederndependency <- as.character(progres.case$elederndependency)
progres.case$elederndependency[is.na(progres.case$elederndependency)]<- "0"
progres.case$elederndependency <- as.factor(progres.case$elederndependency)
progres.case$elederndependency <-  factor(progres.case$elederndependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))

progres.case$female.ratio <- cut(progres.case$Female / progres.case$Num_Inds, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$female.ratio <- as.character(progres.case$female.ratio)
progres.case$female.ratio[is.na(progres.case$female.ratio)]<- "0"
progres.case$female.ratio <- as.factor(progres.case$female.ratio)
progres.case$female.ratio <-  factor(progres.case$female.ratio, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))



##############################
## Adding Age cohort of PA
progres.case$agecohort <- cut(progres.case$dem_age,c(0,18,25,35,45,59,Inf))

##############################
# Age group
progres.case$dem_age_grp1 <- as.factor(ifelse(progres.case$dem_age < 35, 1, 0))
progres.case$dem_age_grp2 <- as.factor(ifelse((progres.case$dem_age >= 35) &  (progres.case$dem_age < 55), 1, 0))
progres.case$dem_age_grp3 <- as.factor(ifelse(progres.case$dem_age >= 55, 1, 0))

progres.case$dem_PA_grp0 <- as.factor(ifelse(progres.case$dem_age < 15, 1, 0))
progres.case$dem_PA_grp1 <- as.factor(ifelse(progres.case$dem_age < 18, 1, 0))
progres.case$dem_PA_grp2 <- as.factor(ifelse((progres.case$dem_age > 17) & (progres.case$dem_age < 60), 1, 0))
progres.case$dem_PA_grp3 <- as.factor(ifelse(progres.case$dem_age > 59, 1, 0))

progres.case$age.PA1 <- as.factor(ifelse(progres.case$dem_age < 35, 1, 0))
progres.case$age.PA2 <- as.factor(ifelse((progres.case$dem_age > 34) & (progres.case$dem_age < 55), 1, 0))
progres.case$age.PA3 <- as.factor(ifelse(progres.case$dem_age > 54, 1, 0))

##############################
## Adding Age cohort for Average age
progres.case$AVGAgecohort <- cut(progres.case$AVG_Age,c(0.1,18,25,35,45,59,Inf))
progres.case$AVGAgecohort <- as.character(progres.case$AVGAgecohort)
progres.case$AVGAgecohort[is.na(progres.case$AVGAgecohort)]<- "0"
progres.case$AVGAgecohort <- as.factor(progres.case$AVGAgecohort)
progres.case$AVGAgecohort <-  factor(progres.case$AVGAgecohort, levels = c( "0", "(0.1,18]", "(18,25]", "(25,35]", "(35,45]","(45,59]", "(59,Inf]"))

##############################
## Adding class for standard age deviation
#summary(progres.case$STDEV_Age)
progres.case$STDEVAgeclass <- cut(progres.case$STDEV_Age,c(0.001,5,10,15,20,Inf))
progres.case$STDEVAgeclass <- as.character(progres.case$STDEVAgeclass)
progres.case$STDEVAgeclass[is.na(progres.case$STDEVAgeclass)]<- "0"
progres.case$STDEVAgeclass <- as.factor(progres.case$STDEVAgeclass)
progres.case$STDEVAgeclass <-  factor(progres.case$STDEVAgeclass, levels = c( "0", "(0.001,5]", "(5,10]", "(10,15]", "(15,20]", "(20,Inf]"))

##############################
# Percentage of children
progres.case$p.child.grp1 <- as.factor(ifelse(progres.case$Child_0_14/progres.case$Num_Inds == 0, 1, 0))
progres.case$p.child.grp2 <- as.factor(ifelse((progres.case$Child_0_14/progres.case$Num_Inds > 0) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.50), 1, 0))
progres.case$p.child.grp3 <- as.factor(ifelse((progres.case$Child_0_14/progres.case$Num_Inds >= 0.50) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.75), 1, 0))
progres.case$p.child.grp4 <- as.factor(ifelse(progres.case$Child_0_14/progres.case$Num_Inds >= 0.75, 1, 0))

##############################
# Aggregating arrival year
#summary(progres.case$YearArrival)
#YearArrival <- as.data.frame(table(progres.case$YearArrival))
#rm(YearArrival)
progres.case$YearArrivalCategory <- as.factor(recode(progres.case$YearArrival,"'1899'='1900-1980';
                                                                      '1928'='1900-1980';
                                                                      '1932'='1900-1980';
                                                                      '1935'='1900-1980';
                                                                      '1936'='1900-1980';
                                                                      '1937'='1900-1980';
                                                                      '1938'='1900-1980';
                                                                      '1939'='1900-1980';
                                                                      '1940'='1900-1980';
                                                                      '1941'='1900-1980';
                                                                      '1942'='1900-1980';
                                                                      '1943'='1900-1980';
                                                                      '1944'='1900-1980';
                                                                      '1945'='1900-1980';
                                                                      '1946'='1900-1980';
                                                                      '1947'='1900-1980';
                                                                      '1948'='1900-1980';
                                                                      '1949'='1900-1980';
                                                                      '1950'='1900-1980';
                                                                      '1951'='1900-1980';
                                                                      '1952'='1900-1980';
                                                                      '1953'='1900-1980';
                                                                      '1954'='1900-1980';
                                                                      '1955'='1900-1980';
                                                                      '1956'='1900-1980';
                                                                      '1957'='1900-1980';
                                                                      '1958'='1900-1980';
                                                                      '1959'='1900-1980';
                                                                      '1960'='1900-1980';
                                                                      '1961'='1900-1980';
                                                                      '1962'='1900-1980';
                                                                      '1963'='1900-1980';
                                                                      '1964'='1900-1980';
                                                                      '1965'='1900-1980';
                                                                      '1966'='1900-1980';
                                                                      '1967'='1900-1980';
                                                                      '1968'='1900-1980';
                                                                      '1969'='1900-1980';
                                                                      '1970'='1900-1980';
                                                                      '1971'='1900-1980';
                                                                      '1972'='1900-1980';
                                                                      '1973'='1900-1980';
                                                                      '1974'='1900-1980';
                                                                      '1975'='1900-1980';
                                                                      '1976'='1900-1980';
                                                                      '1977'='1900-1980';
                                                                      '1978'='1900-1980';
                                                                      '1979'='1900-1980';
                                                                      '1980'='1900-1980';
                                                                      '1981'='1981-1990';
                                                                      '1982'='1981-1990';
                                                                      '1983'='1981-1990';
                                                                      '1984'='1981-1990';
                                                                      '1985'='1981-1990';
                                                                      '1986'='1981-1990';
                                                                      '1987'='1981-1990';
                                                                      '1988'='1981-1990';
                                                                      '1989'='1981-1990';
                                                                      '1990'='1981-1990';
                                                                      '1991'='1991-2000';
                                                                      '1992'='1991-2000';
                                                                      '1993'='1991-2000';
                                                                      '1994'='1991-2000';
                                                                      '1995'='1991-2000';
                                                                      '1996'='1991-2000';
                                                                      '1997'='1991-2000';
                                                                      '1998'='1991-2000';
                                                                      '1999'='1991-2000';
                                                                      '2000'='1991-2000';
                                                                      '2001'='2001-2005';
                                                                      '2002'='2001-2005';
                                                                      '2003'='2001-2005';
                                                                      '2004'='2001-2005';
                                                                      '2005'='2001-2005';
                                                                      '2006'='2006-2010';
                                                                      '2007'='2006-2010';
                                                                      '2008'='2006-2010';
                                                                      '2009'='2006-2010';
                                                                      '2010'='2006-2010';
                                                                      '2011'='2011';
                                                                      '2012'='2012';
                                                                      '2013'='2013';
                                                                      '2014'='2014';
                                                                      '2015'='2015'"))

##############################
# Aggregating country of Origin
#ctrorigin <- as.data.frame(table(progres.case$CountryOrigin), useNA = "ifany")
#rm(ctrorigin)
#summary(progres.case$CountryOrigin)
#levels(progres.case$CountryOrigin)
progres.case$CountryOriginCategory <- recode(progres.case$CountryOrigin,"'SYR'='SYR';
                                        'IRQ'='IRQ';
                                        'SOM'='HORN';
                                        'AFG'='AFG';
                                        'IRN'='IRN';
                                        'SUD'='HORN';
                                        'ETH'='HORN';
                                        'ERT'='HORN';
                                        'PAL'='MENA';
                                        'TUR'='OTH';
                                        'PAK'='ASIA';
                                        'YEM'='MENA';
                                        'SSD'='AFR';
                                        'NIG'='AFR';
                                        'ICO'='AFR';
                                        'COD'='AFR';
                                        'UGA'='AFR';
                                        'BGD'='ASIA';
                                        'ARE'='MENA';
                                        'CMR'='AFR';
                                        'UZB'='ASIA';
                                        'COB'='AFR';
                                        'TKM'='ASIA';
                                        'MLI'='AFR';
                                        'GUI'='AFR';
                                        'CAR'='AFR';
                                        'LBY'='MENA';
                                        'KGZ'='ASIA';
                                        'LEB'='MENA';
                                        'CHD'='AFR';
                                        'TJK'='ASIA';
                                        'MOR'='AFR';
                                        'JOR'='MENA';
                                        'SEN'='AFR';
                                        'KUW'='MENA';
                                        'MNG'='OTH';
                                        'ALG'='MENA';
                                        'GHA'='AFR';
                                        'TUN'='MENA';
                                        'SLE'='AFR';
                                        'LBR'='AFR';
                                        'RUS'='OTH';
                                        'KEN'='AFR';
                                        'CHI'='ASIA';
                                        'LKA'='ASIA';
                                        'BDI'='AFR';
                                        'KAZ'='ASIA';
                                        'PHI'='ASIA';
                                        'ZZZ'='OTH';
                                        'MYA'='ASIA';
                                        'SAU'='MENA';
                                        'BAH'='MENA';
                                        'RWA'='AFR';
                                        'IND'='ASIA';
                                        'TOG'='AFR';
                                        'DJB'='AFR';
                                        'GAM'='AFR';
                                        'NGR'='AFR';
                                        'ZIM'='AFR';
                                        'ANG'='AFR';
                                        'BKF'='AFR';
                                        'NEP'='ASIA';
                                        'SRV'='OTH';
                                        'U'='OTH';
                                        'BEN'='AFR';
                                        'TAN'='AFR';
                                        'UKR'='OTH';
                                        'MAD'='AFR';
                                        'GAB'='AFR';
                                        'INS'='ASIA';
                                        'MAU'='AFR';
                                        'TIB'='ASIA';
                                        'UAE'='MENA';
                                        'BSN'='OTH';
                                        'COI'='AFR';
                                        'OMN'='MENA';
                                        'GNB'='AFR';
                                        'DOM'='OTH';
                                        'ISR'='MENA';
                                        '-'='OTH';
                                        'ARM'='ASIA';
                                        'AZE'='ASIA';
                                        'BLR'='OTH';
                                        'CUB'='OTH';
                                        'FRA'='OTH';
                                        'HAI'='OTH';
                                        'RSA'='OTH';
                                        'ARG'='OTH';
                                        'GBR'='OTH';
                                        'KRN'='OTH';
                                        'MDA'='OTH';
                                        'MTS'='OTH';
                                        'PER'='OTH';
                                        'SCG'='OTH';
                                        'SOL'='OTH';
                                        'SUR'='OTH';
                                        'YUG'='OTH'")

progres.case$CountryOriginCategory <- factor(progres.case$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH"))


##############################
# Aggregating country of Asylum
#ctrAsylum <- as.data.frame(table(progres.case$CountryAsylum))
#rm(ctrAsylum)
### Not necessary

##############################
# Aggregating season according to month
progres.case$season <- as.character(progres.case$Montharrival)
#levels(progres.case$Montharrival)
progres.case$season <- recode(progres.case$season,"'March'='Spring'; 'April'='Spring';    'May'='Spring';
                              'June'='Summer'; 'July'='Summer';  'August'='Summer'; 
                              'September'='Autumn'; 'October'='Autumn'; 'November'='Autumn'; 
                              'January'='Winter';  'February'='Winter'; 'December'='Winter' ")
progres.case$season <- as.factor(progres.case$season)

progres.case$season <- factor(progres.case$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

progres.case$season <- recode(progres.case$season,"'January'='Jan';  'February'='Febr';'March'='Mar';
                               'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug'; 
                              'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec' ")
progres.case$Montharrival <- factor(progres.case$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))

##############################
# Recoding Education
progres.case$edu_highest_t <- progres.case$edu_highest
progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'01' = 'Grade 1'; '02' = 'Grade 2';  
                                     '03' = 'Grade 3';  '04' = '4 years (or Grade 4)';   '05' = 'Grade 5';  
                                     '06' = 'Grade 6';   '07' = 'Grade 7';  '08' = 'Grade 8';  
                                     '09' = 'Grade 9';    '10' = 'Grade 10';  '11' = 'Grade 11'; 
                                     '12' = 'Grade 12';    '13' = 'Grade 13';   '14' = 'Grade 14';  
                                     'IN' = 'Informal Education';    'NE' = 'No education'; 'U' = 'Unknown'; 
                                     'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
                                     'KG' = 'Kindergarten'")

progres.case$edu_highest_t <- factor(progres.case$edu_highest_t, levels = c("Unknown", "No education", "Informal Education","Kindergarten",
                                                                            "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5",
                                                                            "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10",
                                                                            "Grade 11", "Grade 12", "Grade 13", "Grade 14",
                                                                            "Techn Vocational", "University level", "Post university level"))

progres.case$edu_highestcat <- recode(progres.case$edu_highest_t,"'Unknown'='Unknown';
                                      'Informal Education'='Other';
                                      'Techn Vocational'='Other';
                                      'No education'='Up to Grade 5';
                                      'Kindergarten'='Up to Grade 5';
                                      'Grade 1'='Up to Grade 5';
                                      'Grade 2'='Up to Grade 5';
                                      'Grade 3'='Up to Grade 5';
                                      'Grade 4'='Up to Grade 5';
                                      'Grade 5'='Up to Grade 5';
                                      'Grade 6'='Grade 6-8';
                                      'Grade 7'='Grade 6-8';
                                      'Grade 8'='Grade 6-8';
                                      'Grade 9'='Grade 9-11';
                                      'Grade 10'='Grade 9-11';
                                      'Grade 11'='Grade 9-11';
                                      'Grade 12'='Grade 12-14';
                                      'Grade 13'='Grade 12-14';
                                      'Grade 14'='Grade 12-14';
                                      'University level'='Higher Education';
                                      'Post university level'='Higher Education'")

progres.case$edu_highestcat <- as.character(progres.case$edu_highestcat)
#table(progres.case$edu_highestcat, useNA="always")

progres.case$edu_highestcat[is.na(progres.case$edu_highestcat)]<- "Unknown"
#table(progres.case$edu_highestcat, useNA="always")

progres.case$edu_highestcat <- factor(progres.case$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
#table(progres.case$edu_highestcat, useNA="always")
##############################
# Educational attainment
progres.case$edu.highest.grp1 <- as.factor(ifelse((  progres.case$edu_highest_t == "No education" | 
                                           progres.case$edu_highest_t == "Kindergarten" | 
                                           progres.case$edu_highest_t == "Grade 1" | 
                                           progres.case$edu_highest_t == "Grade 2" | 
                                           progres.case$edu_highest_t == "Grade 3" | 
                                           progres.case$edu_highest_t == "Grade 4" | 
                                           progres.case$edu_highest_t == "Grade 5"), 1, 0))

progres.case$edu.highest.grp2 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 6" | 
                                           progres.case$edu_highest_t == "Grade 7" | 
                                           progres.case$edu_highest_t == "Grade 8"), 1, 0))

progres.case$edu.highest.grp3 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 9" | 
                                           progres.case$edu_highest_t == "Grade 10" | 
                                           progres.case$edu_highest_t == "Grade 11") , 1, 0))

progres.case$edu.highest.grp4 <- as.factor(ifelse((progres.case$edu_highest_t == "Grade 12" | 
                                           progres.case$edu_highest_t == "or Grade 13" | 
                                           progres.case$edu_highest_t == "Grade 14") , 1, 0))

progres.case$edu.highest.grp5 <- as.factor(ifelse((progres.case$edu_highest_t == "Post university level" | 
                                           progres.case$edu_highest_t == "University level"), 1, 0))

##############################
# Extracting main ocupation category from occupation code 
summary(progres.case$occupationcode)
progres.case$occupationcat <- "UnknownOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0001"] <- "Military"
progres.case$occupationcat[progres.case$occupationcode ==  "None"] <- "NoOccup"
progres.case$occupationcat[progres.case$occupationcode ==  "0110"] <- "Student"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "1"] <- "Manager"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "1"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "2"] <- "Professional"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "2"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "3"] <- "Technician"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "3"] <- "Manager-Professional-Technician-Clerk"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "4"] <- "Clerk"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "4"] <- "Manager-Professional-Technician-Clerk"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "5"] <- "ServiceMarket"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "6"] <- "Agricultural"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "7"] <- "Craft-Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "8"] <- "Craft-Machine"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "7"] <- "Craft"
#progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "8"] <- "Machine"
progres.case$occupationcat[substr(progres.case$occupationcode, 1,1 )== "9"] <- "Elementary"

progres.case$occupationcat[is.na(progres.case$occupationcat)]<- "UnknownOccup"


#occupationcat <- as.data.frame(table(progres.case$occupationcat, useNA = "ifany"))


progres.case$occupationcat <- factor(progres.case$occupationcat, levels = c("Manager-Professional-Technician-Clerk", "ServiceMarket",
                                                                            "Agricultural", "Craft-Machine", "Elementary", "Military",
                                                                            "UnknownOccup", "NoOccup", "Student"))

#progres.case$occupationcat <- factor(progres.case$occupationcat, levels = c("Manager", "Professional", "Technician", "Clerk", "ServiceMarket",
#                                                                            "Agricultural", "Craft", "Machine", "Elementary", "Military",
#                                                                            "UnknownOccup", "NoOccup", "Student"))

#summary(progres.case$occupationcat)
#progres.case$occupationcat <- substr(progres.case$occupationcode, 1,1 )
#str(progres.case$occupationcat)
##  International Standard Classification of Occupations (ISCO) 
# http://www.ilo.org/public/english/bureau/stat/isco/isco08/
#ISCO.08 <- read.csv("data/ISCO-08.csv")
#names(ISCO.08)
#corrtab88.08 <- read.csv("data/corrtab88-08.csv")
#names(corrtab88.08)
#isco <- merge(x=ISCO.08, y=corrtab88.08, by.x="ISCO08Code", by.y="ISCO.08.Code")
#write.csv(isco, "out/isco.csv")
#names(isco)


##############################
# Marital status
#dem_marriage <- as.data.frame(table(data$dem_marriage))
#rm(dem_marriage)
progres.case$dem_marriagecat <- recode(progres.case$dem_marriage,"'WD'='Widowed'; 'SN'='Single';'DV'='Divorced';'MA'='Married'; 'EG'='Engaged'; 'SR'='Separated'; 'CL'='Married'")

progres.case$dem_marriagecat <- as.character(progres.case$dem_marriagecat)
progres.case$dem_marriagecat[is.na(progres.case$dem_marriagecat)]<- "Unknown"
progres.case$dem_marriagecat <- as.factor(progres.case$dem_marriagecat)
progres.case$dem_marriagecat <- as.factor(progres.case$dem_marriagecat)
progres.case$dem_marriagecat <- factor(progres.case$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced","Unknown"))

progres.case$mar_widow <- as.factor(ifelse(progres.case$dem_marriage == "WD", 1, 0)) 
progres.case$mar_single <- as.factor(ifelse(progres.case$dem_marriage == "SN", 1, 0))
progres.case$mar_divorced <- as.factor(ifelse(progres.case$dem_marriage == "DV", 1, 0)) 
progres.case$mar_married <- as.factor(ifelse(progres.case$dem_marriage == "MA", 1, 0)) 
progres.case$mar_engaged <- as.factor(ifelse(progres.case$dem_marriage == "EG", 1, 0)) 
progres.case$mar_g_divorced <- as.factor(ifelse((progres.case$dem_marriage == "DV" | progres.case$dem_marriage == "SR"), 1, 0))
progres.case$mar_g_married <- as.factor(ifelse((progres.case$dem_marriage == "MA" | progres.case$dem_marriage == "L" | progres.case$dem_marriage == "EG"), 1, 0)) 

##############################
# Ethnicity, religion, birth
progres.case$ethn_arab <- as.factor(ifelse(progres.case$dem_ethn == "Arab", 1, 0))
progres.case$rel_sunni <- as.factor(ifelse(progres.case$dem_religion == "SUN Sunni", 1, 0))
progres.case$bir_syria <- as.factor(ifelse(progres.case$dem_birth_country == "SYR", 1, 0))

##############################
# Gender PA

#table(progres.case$dem_sex, useNA = "ifany")
progres.case$dem_sex <- recode(progres.case$dem_sex,"'M'='Male'; 'F'='Female';'U'='Unknown'")

progres.case$gender.male <- ifelse(progres.case$dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(progres.case$dem_sex == "Female", 1, 0)


###########################################
### Recoding specific needs at the case level
###########################################
progres.specificneed <- read.csv("data/progresspecificneed.csv")

#summary(progres.specificneed)
## Let's recode the vulnerability Text
progres.specificneed$VulnerabilityText <- recode(progres.specificneed$VulnerabilityText,'"Child at risk" = "Child.at.risk"; 
                                                    "Family unity"= "Family.unity";
                                                    "Older person at risk" = "Older.person.at.risk";
                                                    "Pregnant or lactating" = "Pregnant.or.lactating";
                                                    "Single parent"= "Single.parent";
                                                    "Serious medical condition" = "Serious.medical.condition";  
                                                    "Specific legal and physical protection needs" = "Specific.legal.physical.protection.needs"; 
                                                    "Unaccompanied or separated child" = "Unaccompanied.or.separated.child";
                                                    "Woman at risk" = "Woman.at.risk"')


## sound slike there are duplicate for individuals
progres.specificneed.unique <- unique(progres.specificneed[,c("CaseNo","IndividualID","VulnerabilityCode","VulnerabilityText")])
#progres.specificneed.unique.case <- unique(progres.specificneed[,c("CaseNo","VulnerabilityCode","VulnerabilityText")])
#progres.specificneed.unique.case1 <- as.data.frame(unique(progres.specificneed[,c("CaseNo")]))

## let's build the summary per case
progres.specificneed.case <-  melt(progres.specificneed.unique, id.vars = c("CaseNo","VulnerabilityText"),
                                   variable.name = "VulnerabilityText", 
                                   value.name = "value", na.rm = TRUE)

progres.specificneed.case2 <- dcast(progres.specificneed.case, CaseNo ~ VulnerabilityText)
#names(progres.specificneed.case2)


### merging back with progres case info

progres.case.sp <- merge(x=progres.case, y=progres.specificneed.case2, all.x=TRUE)

progres.case.sp$Child.at.risk <- as.factor(ifelse(progres.case.sp$Child.at.risk>=1, "yes", "no"))  
progres.case.sp$Child.at.risk <- as.character(progres.case.sp$Child.at.risk)
progres.case.sp$Child.at.risk[is.na(progres.case.sp$Child.at.risk)]<- "no"
progres.case.sp$Child.at.risk <- as.factor(progres.case.sp$Child.at.risk)

progres.case.sp$Disability <- as.factor(ifelse(progres.case.sp$Disability>=1, "yes", "no")) 
progres.case.sp$Disability <- as.character(progres.case.sp$Disability)
progres.case.sp$Disability[is.na(progres.case.sp$Disability)]<- "no"
progres.case.sp$Disability <- as.factor(progres.case.sp$Disability)

progres.case.sp$Family.unity <- as.factor(ifelse(progres.case.sp$Family.unity>=1, "yes", "no"))
progres.case.sp$Family.unity <- as.character(progres.case.sp$Family.unity)
progres.case.sp$Family.unity[is.na(progres.case.sp$Family.unity)]<- "no"
progres.case.sp$Family.unity <- as.factor(progres.case.sp$Family.unity)


progres.case.sp$Older.person.at.risk <- as.factor(ifelse(progres.case.sp$Older.person.at.risk>=1, "yes", "no"))
progres.case.sp$Older.person.at.risk <- as.character(progres.case.sp$Older.person.at.risk)
progres.case.sp$Older.person.at.risk[is.na(progres.case.sp$Older.person.at.risk)]<- "no"
progres.case.sp$Older.person.at.risk <- as.factor(progres.case.sp$Older.person.at.risk)


progres.case.sp$Pregnant.or.lactating <- as.factor(ifelse(progres.case.sp$Pregnant.or.lactating>=1, "yes", "no")) 
progres.case.sp$Pregnant.or.lactating <- as.character(progres.case.sp$Pregnant.or.lactating)
progres.case.sp$Pregnant.or.lactating[is.na(progres.case.sp$Pregnant.or.lactating)]<- "no"
progres.case.sp$Pregnant.or.lactating <- as.factor(progres.case.sp$Pregnant.or.lactating)


progres.case.sp$Serious.medical.condition <- as.factor(ifelse(progres.case.sp$Serious.medical.condition>=1, "yes", "no"))
progres.case.sp$Serious.medical.condition <- as.character(progres.case.sp$Serious.medical.condition)
progres.case.sp$Serious.medical.condition[is.na(progres.case.sp$Serious.medical.condition)]<- "no"
progres.case.sp$Serious.medical.condition <- as.factor(progres.case.sp$Serious.medical.condition)


progres.case.sp$SGBV <- as.factor(ifelse(progres.case.sp$SGBV>=1, "yes", "no"))
progres.case.sp$SGBV <- as.character(progres.case.sp$SGBV)
progres.case.sp$SGBV[is.na(progres.case.sp$SGBV)]<- "no"
progres.case.sp$SGBV <- as.factor(progres.case.sp$SGBV)


progres.case.sp$Single.parent <- as.factor(ifelse(progres.case.sp$Single.parent>=1, "yes", "no"))
progres.case.sp$Single.parent <- as.character(progres.case.sp$Single.parent)
progres.case.sp$Single.parent[is.na(progres.case.sp$Single.parent)]<- "no"
progres.case.sp$Single.parent <- as.factor(progres.case.sp$Single.parent)


progres.case.sp$Specific.legal.physical.protection.needs <- as.factor(ifelse(progres.case.sp$Specific.legal.physical.protection.needs>=1, "yes", "no"))
progres.case.sp$Specific.legal.physical.protection.needs <- as.character(progres.case.sp$Specific.legal.physical.protection.needs)
progres.case.sp$Specific.legal.physical.protection.needs[is.na(progres.case.sp$Specific.legal.physical.protection.needs)]<- "no"
progres.case.sp$Specific.legal.physical.protection.needs <- as.factor(progres.case.sp$Specific.legal.physical.protection.needs)


progres.case.sp$Torture <- as.factor(ifelse(progres.case.sp$Torture>=1, "yes", "no"))
progres.case.sp$Torture <- as.character(progres.case.sp$Torture)
progres.case.sp$Torture[is.na(progres.case.sp$Torture)]<- "no"
progres.case.sp$Torture <- as.factor(progres.case.sp$Torture)


progres.case.sp$Unaccompanied.or.separated.child <- as.factor(ifelse(progres.case.sp$Unaccompanied.or.separated.child>=1, "yes", "no"))
progres.case.sp$Unaccompanied.or.separated.child <- as.character(progres.case.sp$Unaccompanied.or.separated.child)
progres.case.sp$Unaccompanied.or.separated.child[is.na(progres.case.sp$Unaccompanied.or.separated.child)]<- "no"
progres.case.sp$Unaccompanied.or.separated.child <- as.factor(progres.case.sp$Unaccompanied.or.separated.child)


progres.case.sp$Woman.at.risk <- as.factor(ifelse(progres.case.sp$Woman.at.risk>=1, "yes", "no"))
progres.case.sp$Woman.at.risk <- as.character(progres.case.sp$Woman.at.risk)
progres.case.sp$Woman.at.risk[is.na(progres.case.sp$Woman.at.risk)]<- "no"
progres.case.sp$Woman.at.risk <- as.factor(progres.case.sp$Woman.at.risk)




###########################################
### Recoding Voluntary return Events
###########################################
progres.event <- read.csv("data/progresevent.csv")
#ReasonCode <- as.data.frame(levels(as.factor(progres.event$ReasonCode)))
progres.event$EventReasonText <- ""
progres.event$EventReasonText[progres.event$ReasonCode=='SP1'] <- 'ret.Returned.to.territory.of.origin'
progres.event$EventReasonText[progres.event$ReasonCode=='SP2'] <- 'ret.Departed.to.3rd.country'
progres.event$EventReasonText[progres.event$ReasonCode=='3rdCO'] <- 'ret.Departed.to.3rd.country'
progres.event$EventReasonText[progres.event$ReasonCode=='SP4'] <- 'ret.Whereabouts.unknown'
progres.event$EventReasonText <- as.factor(progres.event$EventReasonText)

# plot(progres.event$EventReasonText)

progres.event.case <-  melt(progres.event, id.vars = c("CaseNo","EventLogEffectiveDate","EventReasonText"), na.rm = TRUE)
progres.event.case2 <- dcast(progres.event.case, CaseNo + EventLogEffectiveDate ~ EventReasonText)

progres.case.sp.dep <- merge(x=progres.case.sp, y=progres.event.case2, all.x=TRUE)

###########################################
### Recoding RST accepted
###########################################
progres.eventrst <- read.csv("data/progreseventrst.csv")
#summary(progres.eventrst)
#str(progres.eventrst)
#EventReasonText <- as.data.frame(levels(as.factor(progres.eventrst$EventReasonText)))
ReasonCode <- as.data.frame(levels(as.factor(progres.eventrst$ReasonCode)))
progres.eventrst$EventReasonText <- ""
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='FAM01'] <- 'rst.Family.reunification'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='LPN01'] <- 'rst.Legal.physical.protection.needs'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='MED01'] <- 'rst.Medical needs'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='OLD01'] <- 'rst.elderly.refugees'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='AWR01'] <- 'rst.Woman.at.risk'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='RLI01'] <- 'rst.Refugee.without.local.integration.prospects'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='SVT01'] <- 'rst.Survivor.of.violence.and.torture'
progres.eventrst$EventReasonText[progres.eventrst$ReasonCode=='CHL01'] <- 'rst.Child.or.adolescent'
progres.eventrst$EventReasonText <- as.factor(progres.eventrst$EventReasonText)

## let's build the summary per case
progres.eventrst.case <-  melt(progres.eventrst, id.vars = c("CaseNo","EventLogEffectiveDate","EventReasonText"), na.rm = TRUE)
progres.eventrst.case2 <- dcast(progres.eventrst.case, CaseNo + EventLogEffectiveDate ~ EventReasonText)

progres.case.sp.dep.rst <- merge(x=progres.case.sp.dep, y=progres.eventrst.case2, all.x=TRUE)

## sound slike there are duplicate for individuals
#progres.eventrst.unique <- unique(progres.eventrst[,c("CaseNo","CountryCode","ReasonCode","EventReasonText")])

###########################################
### Recoding Assistance at the case level
###########################################
assistancecase <- read.csv("data/assistancecase.csv")
#summary(assistancecase)

###################################################
######## Saving reworked case information
###################################################

write.csv(progres.case.sp.dep.rst, file = "data/progrescase2.csv",na="")

rm(assistancecase)
rm(progres.case)
rm(progres.case.sp)
rm(progres.case.sp.dep)
rm(progres.event)
rm(progres.event.case)
rm(progres.event.case2)
rm(progres.eventrst)
rm(progres.eventrst.case)
rm(progres.eventrst.case2)
rm(progres.specificneed)
rm(progres.specificneed.case)
rm(progres.specificneed.case2)
rm(progres.specificneed.unique)
rm(ReasonCode)

data <- progres.case.sp.dep.rst

#rm(progres.case.sp.dep.rst)

### Cleaning data

## Remove observation where we do not have PA Age or Arrival Date or Sex of PA
data <- data[!rowSums(is.na(data["dem_age"])), ]
data <- data[!rowSums(is.na(data["AVG_Age"])), ]
data <- data[!rowSums(is.na(data["YearArrival"])), ]
data <- data[data$dem_sex !="Unknown", ]


## Description of all variables 
data.str <- strtable(data, factor.values=as.integer)