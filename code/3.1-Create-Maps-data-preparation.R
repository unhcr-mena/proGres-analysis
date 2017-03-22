source("code/0-packages.R")


###########################################
### Recoding the Case level Information
###########################################

original.progrescase <- read.csv("data/progrescase.csv")
data.progrescase <- original.progrescase

# summary(original.progrescase)


### Data cleaning ###########################
## let's have a look at how many complete cases we have 
nrows <- nrow(data.progrescase) # how many rows of data
ncomplete <- sum(complete.cases(data.progrescase)) # how many complete rows
incomplete.rows <- 1-(ncomplete/nrows) # shows how much percent of the data are complete
incomplete.rows 
## removing of all incomplete rows would eliminate ~40% of the data
#
# -> add level 'noData' to categorical data to show on every map how much percent of data is included
# -> remove missing data in numerical data within creation of map and only in relevant columns to lose as less data as possible

# data.progrescase.complete <- na.omit(data.progrescase) # would delete all incomplete rows



### Recoding of categorical data with adding of category 'noData' for NA or empty cells
##############################
## Case size  as factor
data.progrescase$Case.size <- as.factor(recode(data.progrescase$Num_Inds,"'-'='Unknown';
                                               '1'='Case.size.1';
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
                                               '20'='Case.size.7.and.more';
                                               '21'='Case.size.7.and.more';
                                               '22'='Case.size.7.and.more';
                                               '23'='Case.size.7.and.more';
                                               '24'='Case.size.7.and.more';
                                               NA = 'noData';
                                               ''='noData'"))
data.progrescase$Case.size <- factor(data.progrescase$Case.size, levels = c("Case.size.1", "Case.size.2", "Case.size.3", "Case.size.4", "Case.size.5", "Case.size.6", "Case.size.7.and.more", "Unknown", "noData"))




##############################
# Aggregating year of Arrival
data.progrescase$YearArrivalCategory <- as.factor(recode(data.progrescase$YearArrival,"'-'='Unknown';
                                                         '1899'='1900-1980';
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
                                                         '2015'='2015';
                                                         '2016' = '2016';
                                                         '2017' = '2017';
                                                         '2018' = '2018';
                                                         '2019' = '2019';
                                                         '2020' = '2020';
                                                         NA = 'noData';
                                                         ''='noData'"))
data.progrescase$YearArrivalCategory <- factor(data.progrescase$YearArrivalCategory, levels = c("1900-1980", "1981-1990", "1991-2000", "2001-2005", "2006-2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "Unknown", "noData"))

##############################
# Aggregating country of Origin
data.progrescase$CountryOriginCategory <- as.factor(recode(data.progrescase$CountryOrigin,"'SYR'='SYR';
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
                                                           'YUG'='OTH';
                                                           '-'='Unknown';
                                                           'U'='Unknown';
                                                           NA = 'noData';
                                                           ''= 'noData'"))
data.progrescase$CountryOriginCategory <- factor(data.progrescase$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH", "Unknown", "noData"))




##############################
# Aggregating country of Asylum
#ctrAsylum <- as.data.frame(table(data.progrescase$CountryAsylum))
#rm(ctrAsylum)
### Not necessary

##############################
# Aggregating season according to month
data.progrescase$season <- as.factor(recode(data.progrescase$Montharrival,"'March'='Spring'; 'April'='Spring';    'May'='Spring';
                                            'June'='Summer'; 'July'='Summer';  'August'='Summer'; 
                                            'September'='Autumn'; 'October'='Autumn'; 'November'='Autumn'; 
                                            'January'='Winter';  'February'='Winter'; 'December'='Winter'; '-'='Unknown'; NA='noData'; ''='noData'"))
data.progrescase$season <- factor(data.progrescase$season, levels = c("Spring", "Summer", "Autumn", "Winter", "Unknown", "noData"))

data.progrescase$Montharrival <- recode(data.progrescase$Montharrival,"'January'='Jan';  'February'='Febr';'March'='Mar';
                                        'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug'; 
                                        'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec'; '-'='Unknown'; NA='noData'; ''='noData'")
data.progrescase$Montharrival <- factor(data.progrescase$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec", "Unknown", "noData"))




##############################
# Recoding Education
data.progrescase$edu_highest_t <- as.factor(recode(data.progrescase$edu_highest,"
                                                   '1 year (or Grade 1)' = 'Grade 1'; 
                                                   '2 year (or Grade 2)' = 'Grade 2';  
                                                   '3 year (or Grade 3)' = 'Grade 3';  
                                                   '4 year (or Grade 4)' = 'Grade 4';
                                                   '5 year (or Grade 5)' = 'Grade 5';  
                                                   '6 year (or Grade 6)' = 'Grade 6';
                                                   '7 year (or Grade 7)' = 'Grade 7';  
                                                   '8 year (or Grade 8)' = 'Grade 8';  
                                                   '9 year (or Grade 9)' = 'Grade 9';
                                                   '10 year (or Grade 10)' = 'Grade 10';  '11 year (or Grade 11)' = 'Grade 11'; 
                                                   '12 year (or Grade 12)' = 'Grade 12';    '13 year (or Grade 13)' = 'Grade 13';
                                                   '14 year (or Grade 14)' = 'Grade 14';  
                                                   'IN' = 'Informal Education'; 'Informal Educaiton' = 'Informal Education';
                                                   'NE' = 'No education';
                                                   'U' = 'Unknown'; 
                                                   '-' = 'Unknown'; 
                                                   'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
                                                   'KG' = 'Kindergarten'; NA='noData'; ''='noData'"))
data.progrescase$edu_highest_t <- factor(data.progrescase$edu_highest_t, levels = c("No education", "Informal Education","Kindergarten",
                                                                                    "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5",
                                                                                    "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10",
                                                                                    "Grade 11", "Grade 12", "Grade 13", "Grade 14",
                                                                                    "Techn Vocational", "University level", "Post university level", "Unknown", "noData"))

data.progrescase$edu_highestcat <- as.factor(recode(data.progrescase$edu_highest_t,"'-'='Unknown'; ''='noData'; NA='noData';
                                                    'Unknown'='Unknown';
                                                    'Informal Educaton'='Other';
                                                    'Techn Vocational'='Other';
                                                    'No education'='No education';
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
                                                    'Post university level'='Higher Education'"))
data.progrescase$edu_highestcat <- factor(data.progrescase$edu_highestcat, levels = c("No education", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education", "Other", "Unknown", "noData"))



##############################
# Extracting main ocupation category from occupation code 
data.progrescase$occupationcat <- 'noData'
data.progrescase$occupationcat[data.progrescase$occupationcode ==  "0001"] <- "Military"
data.progrescase$occupationcat[data.progrescase$occupationcode ==  "None"] <- "NoOccup"
data.progrescase$occupationcat[data.progrescase$occupationcode ==  "0110"] <- "Student"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "1"] <- "Manager-Professional-Technician-Clerk"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "2"] <- "Manager-Professional-Technician-Clerk"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "3"] <- "Manager-Professional-Technician-Clerk"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "4"] <- "Manager-Professional-Technician-Clerk"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "5"] <- "ServiceMarket"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "6"] <- "Agricultural"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "7"] <- "Craft-Machine"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "8"] <- "Craft-Machine"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "9"] <- "Elementary"
data.progrescase$occupationcat[substr(data.progrescase$occupationcode, 1,1 )== "-"] <- "Unknown"

data.progrescase$occupationcat <- factor(data.progrescase$occupationcat, levels = c("Manager-Professional-Technician-Clerk", "ServiceMarket",
                                                                                    "Agricultural", "Craft-Machine", "Elementary", "Military", "NoOccup", "Student", "Unknown", "noData"))
##############################
# Marital status
data.progrescase$dem_marriagecat <- as.factor(recode(data.progrescase$dem_marriage,"'WD'='Widowed'; 'SN'='Single';'DV'='Divorced';'MA'='Married'; 'EG'='Engaged'; 'SR'='Separated'; 'CL'='Married'; '-' = 'Unknown'; NA='noData'; ''='noData'"))
data.progrescase$dem_marriagecat <- factor(data.progrescase$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced", "Unknown", "noData"))


##############################
# Cases with only female members
data.progrescase$only.female.case <- '0'
data.progrescase$only.female.case[data.progrescase$Female ==  "1"] <- "1"
data.progrescase$only.female.case[data.progrescase$Male ==  "1"] <- "0"

##############################
# Cases with only male members
data.progrescase$only.male.case <- '0'
data.progrescase$only.male.case[data.progrescase$Male ==  "1"] <- "1"
data.progrescase$only.male.case[data.progrescase$Female ==  "1"] <- "0"

##############################
# Female headed cases
data.progrescase$female.headed <- '0'
data.progrescase$female.headed[data.progrescase$dem_sex ==  "F"] <- "1"


write.csv(data.progrescase, file = "data/progrescase_maps.csv",na="")


# ####################################################################################################
# 
# 
# ### Apply rules to handle inconsistent values (e.g. age > 110)
# E <- editfile("data/edits.txt")
# plot(E)
# ve <- data.frame(violatedEdits(E, data.progrescase))
# plot(ve)
# 
# 
# ###########################################
# ### Recoding specific needs at the case level
# ###########################################
# original.progresspecificneed <- read.csv("data/progresspecificneed.csv")
# data.progresspecificneed <- original.progresspecificneed
# 
# ## Let's recode the Vulnerability Text and Vulnerability Details Text
# data.progresspecificneed$VulnerabilityText <- gsub(' ','.',data.progresspecificneed$VulnerabilityText)
# #data.progresspecificneed$VulnerabilityDetailsText <- gsub(' ','.',data.progresspecificneed$VulnerabilityDetailsText)
# 
# melt.VulnerabilityText <- dcast((melt(data.progresspecificneed, id = c("CaseNo"), measure=c('VulnerabilityText'))), CaseNo ~ value  )
# #melt.VulnerabilityDetailsText <- dcast((melt(data.progresspecificneed, id = c("CaseNo"), measure=c('VulnerabilityDetailsText'))), CaseNo ~ value  )
# 
# #melt.VulnerabilityText$Serious.medical.condition <- melt.VulnerabilityText$Serious.medical.condition/(sum(melt.VulnerabilityText$Serious.medical.condition))*100
# 
# 
# 
# 
# 
# ### merging back with progres case info
# 
# data.progrescase.specificneed <- merge(x=data.progrescase, y=melt.VulnerabilityText, all.x=TRUE)
# #data.progrescase.specificneed <- merge(x=data.progrescase.specificneed, y=melt.VulnerabilityDetailsText, all.x=TRUE)
# 
# # unique(data.progresspecificneed$VulnerabilityText)
# # 
# # 
# # data.progrescase.specificneed$Serious.medical.condition <- data.progrescase.specificneed$Serious.medical.condition/(sum(data.progrescase.specificneed$Serious.medical.condition))*100
# # data.progrescase.specificneed$Specific.legal.and.physical.protection.needs <- 
# # data.progrescase.specificneed$Child.at.risk <- 
# # data.progrescase.specificneed$Torture <- 
# # data.progrescase.specificneed$Unaccompanied.or.separated.child <- 
# # data.progrescase.specificneed$Family.unity <- 
# # data.progrescase.specificneed$Disability <- 
# # data.progrescase.specificneed$Woman.at.risk <- 
# # data.progrescase.specificneed$SGBV <-  
# # data.progrescase.specificneed$Older.person.at.risk <- 
# # data.progrescase.specificneed$Single.parent <- 
# # data.progrescase.specificneed$Pregnant.or.lactating <- 
# 
# 
# write.csv(data.progrescase.specificneed, file = "data/progrescase_maps.csv",na="")
# 
# 
# # summary(data.progrescase)






















