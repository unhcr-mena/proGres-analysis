
source("code/0-packages.R")

###########################################
### Recoding the Individual level Information
###########################################



rm(progres.spneedevent)
progres.spneedevent <- read.csv("data/spneedevent.csv")
#str(progres.spneedevent)


## Description of all variables 
indidata.str <- strtable(progres.spneedevent, factor.values=as.integer)



### Recoding children at risk, UC and SC
progres.spneedevent$Child.risk.unacc.separated <- NULL
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child.at.risk >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_associated_with_armed_forces_or_groups >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_carer >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_headed_household >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_in_conflict_with_the_law >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_parent >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_spouse >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_engaged_in_other_forms_of_child_labour >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_engaged_in_worst_forms_of_child_labour >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_riskMinor_spouse >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_of_not_attending_school >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_with_special_education_needs >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Child_at_risk_Teenage_pregnancy >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_Single_Child_headed_household >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_Child_in_foster_care >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_Child_in_institutional_care >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_Neglected_child_with_extended_family >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_child_Separated_child >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_child_Unaccompanied_child >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_child_Child_in_foster_care2 >0 ] <- "yes"
progres.spneedevent$Child.risk.unacc.separated[progres.spneedevent$Unaccompanied_or_separated_child_Unaccompanied_minor >0 ] <- "yes" 

progres.spneedevent$Child.risk.unacc.separated <- as.character(progres.spneedevent$Child.risk.unacc.separated)
progres.spneedevent$Child.risk.unacc.separated[is.na(progres.spneedevent$Child.risk.unacc.separated)]<- "no"
progres.spneedevent$Child.risk.unacc.separated <- as.factor(progres.spneedevent$Child.risk.unacc.separated)

summary(progres.spneedevent$Child.risk.unacc.separated)


##############################
# Gender PA

#table(progres.spneedevent$dem_sex, useNA = "ifany")
progres.spneedevent$Sex <- recode(progres.spneedevent$Sex,"'M'='Male'; 'F'='Female';'U'='Unknown'")

##############################
## Case size  as factor
progres.spneedevent$Case.size <- as.factor(progres.spneedevent$CurrentSize)
#Case.size <- as.data.frame(table(progres.spneedevent$Case.size))
#rm(Case.size)
progres.spneedevent$Case.size <- recode(progres.spneedevent$Case.size,"'1'='Case.size.1';
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
# Recoding all date

progres.spneedevent$ArrivalDate <- as.Date(as.character(progres.spneedevent$ArrivalDate), "%d/%m/%Y")
progres.spneedevent$RegisteredOn <- as.Date(as.character(progres.spneedevent$RegisteredOn), "%d/%m/%Y")
progres.spneedevent$RefugeeStatusDate <- as.Date(as.character(progres.spneedevent$RefugeeStatusDate), "%d/%m/%Y")
progres.spneedevent$CaseCreatedOn <- as.Date(as.character(progres.spneedevent$CaseCreatedOn), "%d/%m/%Y")
progres.spneedevent$DOB <- as.Date(as.character(progres.spneedevent$DOB), "%d/%m/%Y")

progres.spneedevent$rst01_considered <- as.Date(as.character(progres.spneedevent$rst01_considered), "%d/%m/%Y")
progres.spneedevent$rst04_notqualified <- as.Date(as.character(progres.spneedevent$rst04_notqualified), "%d/%m/%Y")
progres.spneedevent$rst06_Interview <- as.Date(as.character(progres.spneedevent$rst06_Interview), "%d/%m/%Y")
progres.spneedevent$rst09_Assessment_Complete <- as.Date(as.character(progres.spneedevent$rst09_Assessment_Complete), "%d/%m/%Y")
progres.spneedevent$rst18_CaseSubmitted <- as.Date(as.character(progres.spneedevent$rst18_CaseSubmitted), "%d/%m/%Y")
progres.spneedevent$rst21_CaseResbmitted <- as.Date(as.character(progres.spneedevent$rst21_CaseResbmitted), "%d/%m/%Y")
progres.spneedevent$rst19_CaseAccepted <- as.Date(as.character(progres.spneedevent$rst19_CaseAccepted), "%d/%m/%Y")
progres.spneedevent$rst43_Referred_to_Hub <- as.Date(as.character(progres.spneedevent$rst43_Referred_to_Hub), "%d/%m/%Y")
progres.spneedevent$reg38_SpontaneousDeparture <- as.Date(as.character(progres.spneedevent$reg38_SpontaneousDeparture), "%d/%m/%Y")
progres.spneedevent$vol26_VolrepDepartureConfirmed <- as.Date(as.character(progres.spneedevent$vol26_VolrepDepartureConfirmed), "%d/%m/%Y")


##############################
# Aggregating arrival year
#summary(progres.spneedevent$ArrivalDate)
progres.spneedevent$YearArrival <- as.numeric(format(progres.spneedevent$ArrivalDate, "%Y")) 
## Parsing as date
## Change date format
progres.spneedevent$YearArrivalCategory <- progres.spneedevent$YearArrival

#YearArrival <- as.data.frame(table(progres.spneedevent$ArrivalDate))
#rm(YearArrival)
progres.spneedevent$YearArrivalCategory <- as.factor(recode(progres.spneedevent$YearArrival,"'1899'='1900-1980';
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
# Aggregating season according to month
progres.spneedevent$season <- as.character(format(progres.spneedevent$ArrivalDate, "%B"))
#levels(progres.spneedevent$Montharrival)
progres.spneedevent$season <- recode(progres.spneedevent$season,"'March'='Spring'; 'April'='Spring';    'May'='Spring';
                                     'June'='Summer'; 'July'='Summer';  'August'='Summer'; 
                                     'September'='Autumn'; 'October'='Autumn'; 'November'='Autumn'; 
                                     'January'='Winter';  'February'='Winter'; 'December'='Winter' ")
progres.spneedevent$season <- as.factor(progres.spneedevent$season)

progres.spneedevent$season <- factor(progres.spneedevent$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

progres.spneedevent$Montharrival <- recode(progres.spneedevent$Montharrival,"'January'='Jan';  'February'='Febr';'March'='Mar';
                                     'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug'; 
                                     'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec' ")

progres.spneedevent$Montharrival <- as.character(format(progres.spneedevent$ArrivalDate, "%B"))
progres.spneedevent$Montharrival <- factor(progres.spneedevent$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))


##############################
# Aggregating country of Origin
#ctrorigin <- as.data.frame(table(progres.spneedevent$CountryOrigin), useNA = "ifany")
#rm(ctrorigin)
#summary(progres.spneedevent$CountryOrigin)
#levels(progres.spneedevent$CountryOrigin)
progres.spneedevent$CountryOriginCategory <- recode(progres.spneedevent$CoO,"'SYR'='SYR';
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

progres.spneedevent$CountryOriginCategory <- factor(progres.spneedevent$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH"))


##############################
# Recoding Education
progres.spneedevent$edu_highest_t <- progres.spneedevent$EducationLevelCode
progres.spneedevent$edu_highest_t <- recode(progres.spneedevent$edu_highest_t,"'01' = 'Grade 1'; '02' = 'Grade 2';  
                                            '03' = 'Grade 3';  '04' = '4 years (or Grade 4)';   '05' = 'Grade 5';  
                                            '06' = 'Grade 6';   '07' = 'Grade 7';  '08' = 'Grade 8';  
                                            '09' = 'Grade 9';    '10' = 'Grade 10';  '11' = 'Grade 11'; 
                                            '12' = 'Grade 12';    '13' = 'Grade 13';   '14' = 'Grade 14';  
                                            'IN' = 'Informal Education';    'NE' = 'No education'; 'U' = 'Unknown'; 
                                            'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
                                            'KG' = 'Kindergarten'")

progres.spneedevent$edu_highest_t <- factor(progres.spneedevent$edu_highest_t, levels = c("Unknown", "No education", "Informal Education","Kindergarten",
                                                                                          "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5",
                                                                                          "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10",
                                                                                          "Grade 11", "Grade 12", "Grade 13", "Grade 14",
                                                                                          "Techn Vocational", "University level", "Post university level"))

progres.spneedevent$edu_highestcat <- recode(progres.spneedevent$edu_highest_t,"'Unknown'='Unknown';
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

progres.spneedevent$edu_highestcat <- as.character(progres.spneedevent$edu_highestcat)
#table(progres.spneedevent$edu_highestcat, useNA="always")

progres.spneedevent$edu_highestcat[is.na(progres.spneedevent$edu_highestcat)]<- "Unknown"
#table(progres.spneedevent$edu_highestcat, useNA="always")

progres.spneedevent$edu_highestcat <- factor(progres.spneedevent$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
#table(progres.spneedevent$edu_highestcat, useNA="always")

##############################
# Extracting main ocupation category from occupation code 
summary(progres.spneedevent$OccupationCode)
progres.spneedevent$occupationcat <- "UnknownOccup"
progres.spneedevent$occupationcat[progres.spneedevent$OccupationCode ==  "0001"] <- "Military"
progres.spneedevent$occupationcat[progres.spneedevent$OccupationCode ==  "None"] <- "NoOccup"
progres.spneedevent$occupationcat[progres.spneedevent$OccupationCode ==  "0110"] <- "Student"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "1"] <- "Manager-Professional-Technician-Clerk"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "2"] <- "Manager-Professional-Technician-Clerk"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "3"] <- "Manager-Professional-Technician-Clerk"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "4"] <- "Manager-Professional-Technician-Clerk"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "5"] <- "ServiceMarket"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "6"] <- "Agricultural"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "7"] <- "Craft-Machine"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "8"] <- "Craft-Machine"
progres.spneedevent$occupationcat[substr(progres.spneedevent$OccupationCode, 1,1 )== "9"] <- "Elementary"

progres.spneedevent$occupationcat[is.na(progres.spneedevent$occupationcat)]<- "UnknownOccup"


#occupationcat <- as.data.frame(table(progres.spneedevent$occupationcat, useNA = "ifany"))


progres.spneedevent$occupationcat <- factor(progres.spneedevent$occupationcat, levels = c("Manager-Professional-Technician-Clerk", "ServiceMarket",
                                                                                          "Agricultural", "Craft-Machine", "Elementary", "Military",
                                                                                          "UnknownOccup", "NoOccup", "Student"))



##############################
# Marital status
#MarriageStatusCode <- as.data.frame(table(data$MarriageStatusCode))
#rm(MarriageStatusCode)
progres.spneedevent$dem_marriagecat <- recode(progres.spneedevent$MarriageStatusCode,"'WD'='Widowed'; 'SN'='Single';'DV'='Divorced';'MA'='Married'; 'EG'='Engaged'; 'SR'='Separated'; 'CL'='Married'")

progres.spneedevent$dem_marriagecat <- as.character(progres.spneedevent$dem_marriagecat)
progres.spneedevent$dem_marriagecat[is.na(progres.spneedevent$dem_marriagecat)]<- "Unknown"
progres.spneedevent$dem_marriagecat <- as.factor(progres.spneedevent$dem_marriagecat)
progres.spneedevent$dem_marriagecat <- as.factor(progres.spneedevent$dem_marriagecat)
progres.spneedevent$dem_marriagecat <- factor(progres.spneedevent$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced","Unknown"))





write.csv(progres.specificneed, file = "data/progresspecificneed2.csv",na="")



###########################################
### Recoding Spontaneous Departure Events
###########################################
progres.event <- read.csv("data/progresevent.csv")
#ReasonCode <- as.data.frame(levels(as.factor(progres.event$ReasonCode)))
progres.event$EventReasonText[progres.event$ReasonCode=='-'] <- 'ret.NoReason'
progres.event$EventReasonText[progres.event$ReasonCode=='SP1'] <- 'ret.Returned.to.territory.of.origin'
progres.event$EventReasonText[progres.event$ReasonCode=='SP2'] <- 'ret.Departed.to.3rd.country'
progres.event$EventReasonText[progres.event$ReasonCode=='3rdCO'] <- 'ret.Departed.to.3rd.country'
progres.event$EventReasonText[progres.event$ReasonCode=='SP4'] <- 'ret.Whereabouts.unknown'
progres.event$EventReasonText <- as.factor(progres.event$EventReasonText)

# plot(progres.event$EventReasonText)

progres.event.case <-  melt(progres.event, id.vars = c("CaseNo","EventLogEffectiveDate","EventReasonText","CountryCode"), na.rm = TRUE)
progres.event.case2 <- dcast(progres.event.case, CaseNo + EventLogEffectiveDate+ CountryCode ~ EventReasonText )

## Case where EventLogEffectiveDate & CountryCode are different within the same case 
unique(progres.event$CaseNo)
unique(progres.event.case2$CaseNo)

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



rm(progres.case.sp.dep)
rm(progres.event)
rm(progres.event.case)
rm(progres.event.case2)
rm(progres.eventrst)
rm(progres.eventrst.case)
rm(progres.eventrst.case2)

rm(ReasonCode)
