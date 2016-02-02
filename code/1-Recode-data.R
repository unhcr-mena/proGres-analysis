
###########################################
### Recoding the Case level Information
###########################################

rm(progres.case)
progres.case <- read.csv("data/progrescase.csv")

#str(progres.case)


## Integer as factor
progres.case$Num_Inds2 <- as.factor(progres.case$Num_Inds)
progres.case$YearArrival <- as.factor(progres.case$YearArrival) 

## Calculating dependency ration

progres.case$dependency <- (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64
progres.case$youthdependency <- progres.case$Child_0_14 / progres.case$Work_15_64
progres.case$elederndependency <- progres.case$Eldern_65 / progres.case$Work_15_64

## Adding Age cohort
progres.case$agecohort <- cut(progres.case$dem_age,c(0,18,25,35,59,Inf))

##########
# Aggregating arrival year
#summary(progres.case$YearArrival)
#YearArrival <- as.data.frame(table(progres.case$YearArrival))
#rm(YearArrival)

progres.case$YearArrivalCategory <- recode(progres.case$YearArrival,"'1899'='1900-1980';
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
                                                                      '2015'='2015'")

##########
# Aggregating country of Origin

#ctrorigin <- as.data.frame(table(progres.case$CountryOrigin))
#rm(ctrorigin)
#summary(progres.case$CountryOrigin)
#levels(progres.case$CountryOrigin)


progres.case$CountryOriginCategory <- recode(progres.case$CountryOrigin,"'SYR'='SYR';
                                        'IRQ'='IRQ';
                                        'SOM'='SOM';
                                        'AFG'='AFG';
                                        'IRN'='IRN';
                                        'SUD'='SUD';
                                        'ETH'='ETH';
                                        'ERT'='ERT';
                                        'PAL'='PAL';
                                        'TUR'='TUR';
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

##########
# Aggregating country of Asylum

#ctrAsylum <- as.data.frame(table(progres.case$CountryAsylum))
#rm(ctrAsylum)
### Not necessary

##########
# Aggregating season according to month

progres.case$season <- progres.case$Montharrival
#levels(progres.case$Montharrival)
progres.case$season <- recode(progres.case$season,"'March'='Spring'; 'April'='Spring';    'May'='Spring';
                              'June'='Summer'; 'July'='Summer';  'August'='Summer'; 
                              'September'='Autumn'; 'October'='Autumn'; 'November'='Autumn'; 
                              'January'='Winter';  'February'='Winter'; 'December'='Winter' ")

progres.case$season <- factor(progres.case$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

progres.case$season <- recode(progres.case$season,"'January'='Jan';  'February'='Febr';'March'='Mar';
                               'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug'; 
                              'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec' ")
progres.case$Montharrival <- factor(progres.case$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))

##########
# Recoding Education
progres.case$edu_highest_t <- progres.case$edu_highest
progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'01' = 'E1year'; '02' = 'E2year';  
                                     '03' = 'E3year';  '04' = 'E4year';   '05' = 'E5year';  
                                     '06' = 'E6year';   '07' = 'E7year';  '08' = 'E8year';  
                                     '09' = 'E9year';    '10' = 'E10year';  '11' = 'E11year'; 
                                     '12' = 'E12year';    '13' = 'E13year';   '14' = 'E14year';  
                                     'IN' = 'InformEduc';    'NE' = 'NoEduc'; 'U' = 'Unknown'; 
                                     'TC' = 'TechnVoc';     'UG' = 'Univ'; 'PG' = 'PostUniv' ")

progres.case$edu_highest_t <- factor(progres.case$edu_highest_t, levels = c("Unknown", "NoEduc", "InformEduc", "E1year", "E2year", "E3year", "E4year", "E5year", "E6year", "E7year", "E8year", "E9year", "E10year", "E11year", "E12year", "E13year", "E14year", "TechnVoc", "Univ", "PostUniv"))


##########
# Extracting main ocupation category from occupation code 

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
summary(progres.case$occupationcat)

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



##########
# Age group
progres.case$dem_age_grp1 <- ifelse(progres.case$dem_age < 35, 1, 0)
progres.case$dem_age_grp2 <- ifelse((progres.case$dem_age >= 35) &  (progres.case$dem_age < 55), 1, 0)
progres.case$dem_age_grp3 <- ifelse(progres.case$dem_age >= 55, 1, 0)

progres.case$dem_PA_grp0 <- ifelse(progres.case$dem_age < 15, 1, 0)
progres.case$dem_PA_grp1 <- ifelse(progres.case$dem_age < 18, 1, 0)
progres.case$dem_PA_grp2 <- ifelse((progres.case$dem_age > 17) & (progres.case$dem_age < 60), 1, 0)
progres.case$dem_PA_grp3 <- ifelse(progres.case$dem_age > 59, 1, 0)

progres.case$age.PA1 <- ifelse(progres.case$dem_age < 35, 1, 0)
progres.case$age.PA2 <- ifelse((progres.case$dem_age > 34) & (progres.case$dem_age < 55), 1, 0)
progres.case$age.PA3 <- ifelse(progres.case$dem_age > 54, 1, 0)


##########
# Percentage of children
progres.case$p.child.grp1 <- ifelse(progres.case$Child_0_14/progres.case$Num_Inds == 0, 1, 0)
progres.case$p.child.grp2 <- ifelse((progres.case$Child_0_14/progres.case$Num_Inds > 0) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.50), 1, 0)
progres.case$p.child.grp3 <- ifelse((progres.case$Child_0_14/progres.case$Num_Inds >= 0.50) & (progres.case$Child_0_14/progres.case$Num_Inds < 0.75), 1, 0)
progres.case$p.child.grp4 <- ifelse(progres.case$Child_0_14/progres.case$Num_Inds >= 0.75, 1, 0)


##########
# Marital status
progres.case$mar_widow <- ifelse(progres.case$dem_marriage == "WD Widowed", 1, 0) 
progres.case$mar_single <- ifelse(progres.case$dem_marriage == "SN Single", 1, 0) 
progres.case$mar_divorced <- ifelse(progres.case$dem_marriage == "DV Divorced", 1, 0) 
progres.case$mar_married <- ifelse(progres.case$dem_marriage == "MA Married", 1, 0) 
progres.case$mar_engaged <- ifelse(progres.case$dem_marriage == "EG Engaged", 1, 0) 
progres.case$mar_g_divorced <- ifelse((progres.case$dem_marriage == "DV Divorced" | progres.case$dem_marriage == "SR Separated"), 1, 0) 
progres.case$mar_g_married <- ifelse((progres.case$dem_marriage == "MA Married" | progres.case$dem_marriage == "CL Common Law Married" | progres.case$dem_marriage == "EG Engaged"), 1, 0) 


##########
# Ethnicity, religion, birth
progres.case$ethn_arab <- ifelse(progres.case$dem_ethn == "Arab", 1, 0)
progres.case$rel_sunni <- ifelse(progres.case$dem_religion == "SUN Sunni", 1, 0)
progres.case$bir_syria <- ifelse(progres.case$dem_birth_country == "SYR", 1, 0)

##########
# Gender PA
progres.case$gender.male <- ifelse(progres.case$dem_sex == "Male", 1, 0)
progres.case$gender.female <- ifelse(progres.case$dem_sex == "Female", 1, 0)

##########
# Educational attainment
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







###########################################
### Recoding specific needs at the case level
###########################################

progres.specificneed <- read.csv("data/progresspecificneed.csv")

summary(progres.specificneed)

## sound slike there are duplicate for individuals
progres.specificneed.unique <- unique(progres.specificneed[,c("CaseNo","IndividualID","VulnerabilityCode","VulnerabilityText")])


## Let's recode the vulnerability Text
library("car")
progres.specificneed.unique$VulnerabilityText <- recode(progres.specificneed.unique$VulnerabilityText,'"Child at risk" = "Child.at.risk"; 
                                                    "Family unity"= "Family.unity";
                                                    "Older person at risk" = "Older.person.at.risk";
                                                    "Pregnant or lactating" = "Pregnant.or.lactating";
                                                    "Single parent"= "Single.parent";
                                                    "Serious medical condition" = "Serious.medical.condition";  
                                                    "Specific legal and physical protection needs" = "Specific.legal.physical.protection.needs"; 
                                                    "Unaccompanied or separated child" = "Unaccompanied.or.separated.child";
                                                    "Woman at risk" = "Woman.at.risk"')

## let's build the summary per case
progres.specificneed.case <-  melt(progres.specificneed.unique, id.vars = c("CaseNo","VulnerabilityText"))


progres.specificneed.case2 <- dcast(progres.specificneed.case, CaseNo ~ VulnerabilityText)

### merging back with progres case info

progres.case.sp <- merge(x=progres.case, y=progres.specificneed.case2, all.x=TRUE)


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

progres.event.case <-  melt(progres.event, id.vars = c("CaseNo","EventLogEffectiveDate","EventReasonText"))
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
progres.eventrst.case <-  melt(progres.eventrst, id.vars = c("CaseNo","EventLogEffectiveDate","EventReasonText"))
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
