
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
                                                                      '20'='Case.size.7.and.more';
                                                                      '20'='Case.size.7.and.more';
                                                                      '21'='Case.size.7.and.more';
                                                                      '22'='Case.size.7.and.more';
                                                                      '23'='Case.size.7.and.more';
                                                                      '24'='Case.size.7.and.more';
                                                                      '25'='Case.size.7.and.more';
                                                                      '26'='Case.size.7.and.more';
                                                                      '27'='Case.size.7.and.more';
                                                                      '36'='Case.size.7.and.more';
                                                                      '37'='Case.size.7.and.more';
                                                                      '42'='Case.size.7.and.more';
                                                                      '42'='Case.size.7.and.more';
                                                                      '46'='Case.size.7.and.more';
                                                                      '55'='Case.size.7.and.more';
                                                                      '59'='Case.size.7.and.more';
                                                                      '65'='Case.size.7.and.more';
                                                                      '67'='Case.size.7.and.more';
                                                                      '83'='Case.size.7.and.more';
                                                                      '42'='Case.size.7.and.more'")

prop.table(table(progres.case$Case.size, useNA = "ifany"))

##############################
## Calculating dependency ration
#progres.case$dependency1 <-   (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64
#hist(progres.case$dependency1)
progres.case$dependency <-  cut( (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
# progres.case$dependency <-  cut( (progres.case$Child_0_14+progres.case$Eldern_65) / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
#levels(progres.case$dependency)
#prop.table(table(progres.case$dependency, useNA = "ifany"))
progres.case$dependency <- as.character(progres.case$dependency)
progres.case$dependency[is.na(progres.case$dependency)]<- "1.no.dependant"

progres.case$dependency <- as.factor(recode(progres.case$dependency,"'(0.0001,0.99]'='2.few.dependant';
                                                                      '(0.99,1.1]'='3.half.dependant';
                                                                      '(1.1,Inf]'='4.majority.dependant'"))
#prop.table(table(progres.case$dependency, useNA = "ifany"))

#View(progres.case[ ,c("dependency","Child_0_14","Eldern_65","Work_15_64" )])
#progres.case$dependency <- as.factor(progres.case$dependency)
#progres.case$dependency <-  factor(progres.case$dependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))

#<- relevel(sizes, "medium")
# , levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]")

prop.table(table(progres.case$dependency, useNA = "ifany"))

#progres.case$youthdependency <- cut(progres.case$Child_0_14 / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$youthdependency <- cut(progres.case$Child_0_14 / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
progres.case$youthdependency <- as.character(progres.case$youthdependency)
progres.case$youthdependency[is.na(progres.case$youthdependency)]<- "1.no.dependant"
progres.case$youthdependency <- as.factor(recode(progres.case$youthdependency,"'(0.0001,0.99]'='2.few.dependant';
                                                                      '(0.99,1.1]'='3.half.dependant';
                                                                      '(1.1,Inf]'='4.majority.dependant'"))
#progres.case$youthdependency <-  factor(progres.case$youthdependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))

prop.table(table(progres.case$youthdependency, useNA = "ifany"))

#progres.case$elederndependency <- cut(progres.case$Eldern_65 / progres.case$Work_15_64, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$elederndependency <- cut(progres.case$Eldern_65 / progres.case$Work_15_64, c(0.0001,0.99,1.1,Inf))
progres.case$elederndependency <- as.character(progres.case$elederndependency)
progres.case$elederndependency[is.na(progres.case$elederndependency)]<- "1.no.dependant"
progres.case$elederndependency <- as.factor(recode(progres.case$elederndependency,"'(0.0001,0.99]'='2.few.dependant';
                                                                      '(0.99,1.1]'='3.half.dependant';
                                                      '(1.1,Inf]'='4.majority.dependant'"))
#progres.case$elederndependency <-  factor(progres.case$elederndependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))

prop.table(table(progres.case$elederndependency, useNA = "ifany"))

#progres.case$female.ratio <- cut(progres.case$Female / progres.case$Num_Inds, c(0.001,0.2,0.4,0.6,0.8,Inf))
progres.case$female.ratio <- cut(progres.case$Female / progres.case$Num_Inds, c(0.0001,0.99,1.1,Inf))
prop.table(table(progres.case$female.ratio, useNA = "ifany"))
progres.case$female.ratio <- as.character(progres.case$female.ratio)
progres.case$female.ratio[is.na(progres.case$female.ratio)]<- "1.no.female"
progres.case$female.ratio <- as.factor(recode(progres.case$female.ratio,"'(0.0001,0.99]'='2.few.female'; '(0.99,1.1]'='3.half.female';
                                       '(1.1,Inf]'='4.female'"))
View(progres.case[ ,c("Female","Male","Num_Inds","female.ratio" )])
#progres.case$female.ratio <-  factor(progres.case$female.ratio, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))


### Checking proportion to recategorise accordingly
prop.table(table(progres.case$female.ratio, useNA = "ifany"))

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
progres.case$YearArrivalCategory2 <- as.factor(recode(progres.case$YearArrival,"'1899'='1900-1980';
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


progres.case$YearArrivalCategory <- as.factor(recode(progres.case$YearArrival,"'1899'='before.2011.or.unkown';
                                                     '1900'='before.2011.or.unkown';
                                                     '1902'='before.2011.or.unkown';
                                                     '1905'='before.2011.or.unkown';
                                                     '1911'='before.2011.or.unkown';
                                                     '1926'='before.2011.or.unkown';
                                                     '1927'='before.2011.or.unkown';
                                                     '1930'='before.2011.or.unkown';
                                                     '1931'='before.2011.or.unkown';
                                                     '1933'='before.2011.or.unkown';
                                                     '1934'='before.2011.or.unkown';
                                                     '1928'='before.2011.or.unkown';
                                                     '1932'='before.2011.or.unkown';
                                                     '1935'='before.2011.or.unkown';
                                                     '1936'='before.2011.or.unkown';
                                                     '1937'='before.2011.or.unkown';
                                                     '1938'='before.2011.or.unkown';
                                                     '1939'='before.2011.or.unkown';
                                                     '1940'='before.2011.or.unkown';
                                                     '1941'='before.2011.or.unkown';
                                                     '1942'='before.2011.or.unkown';
                                                     '1943'='before.2011.or.unkown';
                                                     '1944'='before.2011.or.unkown';
                                                     '1945'='before.2011.or.unkown';
                                                     '1946'='before.2011.or.unkown';
                                                     '1947'='before.2011.or.unkown';
                                                     '1948'='before.2011.or.unkown';
                                                     '1949'='before.2011.or.unkown';
                                                     '1950'='before.2011.or.unkown';
                                                     '1951'='before.2011.or.unkown';
                                                     '1952'='before.2011.or.unkown';
                                                     '1953'='before.2011.or.unkown';
                                                     '1954'='before.2011.or.unkown';
                                                     '1955'='before.2011.or.unkown';
                                                     '1956'='before.2011.or.unkown';
                                                     '1957'='before.2011.or.unkown';
                                                     '1958'='before.2011.or.unkown';
                                                     '1959'='before.2011.or.unkown';
                                                     '1960'='before.2011.or.unkown';
                                                     '1961'='before.2011.or.unkown';
                                                     '1962'='before.2011.or.unkown';
                                                     '1963'='before.2011.or.unkown';
                                                     '1964'='before.2011.or.unkown';
                                                     '1965'='before.2011.or.unkown';
                                                     '1966'='before.2011.or.unkown';
                                                     '1967'='before.2011.or.unkown';
                                                     '1968'='before.2011.or.unkown';
                                                     '1969'='before.2011.or.unkown';
                                                     '1970'='before.2011.or.unkown';
                                                     '1971'='before.2011.or.unkown';
                                                     '1972'='before.2011.or.unkown';
                                                     '1973'='before.2011.or.unkown';
                                                     '1974'='before.2011.or.unkown';
                                                     '1975'='before.2011.or.unkown';
                                                     '1976'='before.2011.or.unkown';
                                                     '1977'='before.2011.or.unkown';
                                                     '1978'='before.2011.or.unkown';
                                                     '1979'='before.2011.or.unkown';
                                                     '1980'='before.2011.or.unkown';
                                                     '1981'='before.2011.or.unkown';
                                                     '1982'='before.2011.or.unkown';
                                                     '1983'='before.2011.or.unkown';
                                                     '1984'='before.2011.or.unkown';
                                                     '1985'='before.2011.or.unkown';
                                                     '1986'='before.2011.or.unkown';
                                                     '1987'='before.2011.or.unkown';
                                                     '1988'='before.2011.or.unkown';
                                                     '1989'='before.2011.or.unkown';
                                                     '1990'='before.2011.or.unkown';
                                                     '1991'='before.2011.or.unkown';
                                                     '1992'='before.2011.or.unkown';
                                                     '1993'='before.2011.or.unkown';
                                                     '1994'='before.2011.or.unkown';
                                                     '1995'='before.2011.or.unkown';
                                                     '1996'='before.2011.or.unkown';
                                                     '1997'='before.2011.or.unkown';
                                                     '1998'='before.2011.or.unkown';
                                                     '1999'='before.2011.or.unkown';
                                                     '2000'='before.2011.or.unkown';
                                                     '2001'='before.2011.or.unkown';
                                                     '2002'='before.2011.or.unkown';
                                                     '2003'='before.2011.or.unkown';
                                                     '2004'='before.2011.or.unkown';
                                                     '2005'='before.2011.or.unkown';
                                                     '2006'='before.2011.or.unkown';
                                                     '2007'='before.2011.or.unkown';
                                                     '2008'='before.2011.or.unkown';
                                                     '2009'='before.2011.or.unkown';
                                                     '2010'='before.2011.or.unkown';
                                                     '2011'='2011';
                                                     '2012'='2012';
                                                     '2013'='2013';
                                                     '2014'='2014';
                                                     '2015'='2015';
                                                     '2016'='2016.and.2017';
                                                     '2017'='2016.and.2017'"))

progres.case$YearArrivalCategory[is.na(progres.case$YearArrivalCategory)] <- "before.2011.or.unkown"

prop.table(table(progres.case$YearArrivalCategory, useNA = "ifany"))

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


###########################################################################
#### Recategorised "cool1","coal1", -- in each country of asylum if the category 

## classes with low numbers are aggregated. Unknown adresses are recorded as 'other'.
progres.case$cool1Cat <- progres.case$cool1
progres.case$coal1Cat <- progres.case$coal1
#table(progres.case$cool1,progres.case$CountryAsylum) 


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

progres.case$Montharrival <- recode(progres.case$Montharrival,"'January'='Jan';  'February'='Febr';'March'='Mar';
                               'April'='Apr';  'May'='May'; 'June'='Jun'; 'July'='Jul';  'August'='Aug'; 
                              'September'='Sept'; 'October'='Oct'; 'November'='Nov'; 'December'='Dec' ")
progres.case$Montharrival <- factor(progres.case$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))

##############################
# Recoding Education
progres.case$edu_highest_t <- progres.case$edu_highest
#progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'01' = 'Grade 1'; '02' = 'Grade 2';  
#                                     '03' = 'Grade 3';  '04' = '4 years (or Grade 4)';   '05' = 'Grade 5';  
#                                     '06' = 'Grade 6';   '07' = 'Grade 7';  '08' = 'Grade 8';  
#                                     '09' = 'Grade 9';    '10' = 'Grade 10';  '11' = 'Grade 11'; 
#                                     '12' = 'Grade 12';    '13' = 'Grade 13';   '14' = 'Grade 14';  
#                                     'IN' = 'Informal Education';    'NE' = 'No education'; 'U' = 'Unknown'; 
#                                     'TC' = 'Techn Vocational';     'UG' = 'University level'; 'PG' = 'Post university level';
#                                     'KG' = 'Kindergarten'")

progres.case$edu_highest_t <- recode(progres.case$edu_highest_t,"'1 year (or Grade 1)' = 'Grade 1'; '2 year (or Grade 2)' = 'Grade 2';  
                                     '3 year (or Grade 3)' = 'Grade 3';  '04' = '4 year (or Grade 4)';
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
                                     'KG' = 'Kindergarten'")

progres.case$edu_highest_t <- factor(progres.case$edu_highest_t, levels = c("Unknown", "No education", "Informal Education","Kindergarten",
                                                                            "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Grade 5",
                                                                            "Grade 6", "Grade 7", "Grade 8", "Grade 9", "Grade 10",
                                                                            "Grade 11", "Grade 12", "Grade 13", "Grade 14",
                                                                            "Techn Vocational", "University level", "Post university level"))

progres.case$edu_highestcat <- recode(progres.case$edu_highest_t,"'Unknown'='Unknown';
                                      'Informal Educaiton'='Other';
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
                                      'Post university level'='Higher Education'")

progres.case$edu_highestcat <- as.character(progres.case$edu_highestcat)
#table(progres.case$edu_highestcat, useNA="always")

progres.case$edu_highestcat[is.na(progres.case$edu_highestcat)]<- "Unknown"
#table(progres.case$edu_highestcat, useNA="always")

progres.case$edu_highestcat <- factor(progres.case$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
#table(progres.case$edu_highestcat, useNA="always")


progres.case$edu_highestcat <- recode(progres.case$edu_highest_t,"
                                      'Up to Grade 5'='Other';
                                      'Grade 6-8'='Other';
                                      'Grade 9-11'='Other';
                                      'Grade 12-14'='Other'")

prop.table(table(progres.case$edu_highestcat, useNA = "ifany"))

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

### load re-encoding file for specific needs
library(readxl)
SpecificNeedsCodesV2 <- read_excel("D:/R-project/proGres-analysis/data/SpecificNeedsCodesV2.xlsx", 
                                   sheet = "Revised")
names(SpecificNeedsCodesV2)

## Extract from DWH
progres.specificneed <- read.csv("data/progresspecificneed.csv")
progres.specificneed <- progres.specificneed[,c("CaseNo","IndividualID","code")]

#summary(progres.specificneed)
names(progres.specificneed)

## Check if duplicates
progres.specificneed.unique <- unique(progres.specificneed[,c("CaseNo","IndividualID","code")])
rm(progres.specificneed.unique)

progres.specificneed.case <- merge(x=progres.specificneed, y=SpecificNeedsCodesV2, by="code", all.x=TRUE)

#progres.specificneed.unique.case <- unique(progres.specificneed[,c("CaseNo","VulnerabilityCode","VulnerabilityText")])
#progres.specificneed.unique.case1 <- as.data.frame(unique(progres.specificneed[,c("CaseNo")]))

## let's build the summary per case
# progres.specificneed.case <-  melt(progres.specificneed.unique, id.vars = c("CaseNo","VulnerabilityText"),
#                                   variable.name = "VulnerabilityText", 
#                                   value.name = "value", na.rm = TRUE)

progres.specificneed.case2 <- dcast(progres.specificneed.case, CaseNo ~ newcat)
#names(progres.specificneed.case2)
#str(progres.specificneed.case2)
rm(progres.specificneed)
rm(progres.specificneed.case)



## Let's recode the vulnerability Text


# "At.Risk", Child.Labour"                         
# "Child.marriage..parent.or.pregnancy"   "Family.Needs"                          "Marginalised"                         
# "Medical"                               "Need.of.Care"                          "Problem.with.violence.law.recruitment"
# "Separated.Child"                       "Single.Parent"                         "Unaccompanied"                        
# "Victim.of.Violence"                    "Woman.at.Risk"  
progres.specificneed.case2$At.Risk.count <- progres.specificneed.case2$At.Risk
progres.specificneed.case2$At.Risk <- as.factor(ifelse(progres.specificneed.case2$At.Risk>=1, "yes", "no"))  
progres.specificneed.case2$At.Risk <- as.character(progres.specificneed.case2$At.Risk)
progres.specificneed.case2$At.Risk[is.na(progres.specificneed.case2$At.Risk)]<- "no"
progres.specificneed.case2$At.Risk <- as.factor(progres.specificneed.case2$At.Risk)

progres.specificneed.case2$Child.Labour.count <- progres.specificneed.case2$Child.Labour
progres.specificneed.case2$Child.Labour <- as.factor(ifelse(progres.specificneed.case2$Child.Labour>=1, "yes", "no"))  
progres.specificneed.case2$Child.Labour <- as.character(progres.specificneed.case2$Child.Labour)
progres.specificneed.case2$Child.Labour[is.na(progres.specificneed.case2$Child.Labour)]<- "no"
progres.specificneed.case2$Child.Labour <- as.factor(progres.specificneed.case2$Child.Labour)


progres.specificneed.case2$Child.marriage..parent.or.pregnancy.count <- progres.specificneed.case2$Child.marriage..parent.or.pregnancy
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.factor(ifelse(progres.specificneed.case2$Child.marriage..parent.or.pregnancy>=1, "yes", "no"))  
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.character(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)
progres.specificneed.case2$Child.marriage..parent.or.pregnancy[is.na(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)]<- "no"
progres.specificneed.case2$Child.marriage..parent.or.pregnancy <- as.factor(progres.specificneed.case2$Child.marriage..parent.or.pregnancy)


progres.specificneed.case2$Family.Needs.count <- progres.specificneed.case2$Family.Needs
progres.specificneed.case2$Family.Needs <- as.factor(ifelse(progres.specificneed.case2$Family.Needs>=1, "yes", "no"))  
progres.specificneed.case2$Family.Needs <- as.character(progres.specificneed.case2$Family.Needs)
progres.specificneed.case2$Family.Needs[is.na(progres.specificneed.case2$Family.Needs)]<- "no"
progres.specificneed.case2$Family.Needs <- as.factor(progres.specificneed.case2$Family.Needs)


progres.specificneed.case2$Marginalised.count <- progres.specificneed.case2$Marginalised
progres.specificneed.case2$Marginalised <- as.factor(ifelse(progres.specificneed.case2$Marginalised>=1, "yes", "no"))  
progres.specificneed.case2$Marginalised <- as.character(progres.specificneed.case2$Marginalised)
progres.specificneed.case2$Marginalised[is.na(progres.specificneed.case2$Marginalised)]<- "no"
progres.specificneed.case2$Marginalised <- as.factor(progres.specificneed.case2$Marginalised)


progres.specificneed.case2$Medical.count <- progres.specificneed.case2$Medical
progres.specificneed.case2$Medical <- as.factor(ifelse(progres.specificneed.case2$Medical>=1, "yes", "no"))  
progres.specificneed.case2$Medical <- as.character(progres.specificneed.case2$Medical)
progres.specificneed.case2$Medical[is.na(progres.specificneed.case2$Medical)]<- "no"
progres.specificneed.case2$Medical <- as.factor(progres.specificneed.case2$Medical)


progres.specificneed.case2$Need.of.Care.count <- progres.specificneed.case2$Need.of.Care
progres.specificneed.case2$Need.of.Care <- as.factor(ifelse(progres.specificneed.case2$Need.of.Care>=1, "yes", "no"))  
progres.specificneed.case2$Need.of.Care <- as.character(progres.specificneed.case2$Need.of.Care)
progres.specificneed.case2$Need.of.Care[is.na(progres.specificneed.case2$Need.of.Care)]<- "no"
progres.specificneed.case2$Need.of.Care <- as.factor(progres.specificneed.case2$Need.of.Care)


progres.specificneed.case2$Problem.with.violence.law.recruitment.count <- progres.specificneed.case2$Problem.with.violence.law.recruitment
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.factor(ifelse(progres.specificneed.case2$Problem.with.violence.law.recruitment>=1, "yes", "no"))  
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.character(progres.specificneed.case2$Problem.with.violence.law.recruitment)
progres.specificneed.case2$Problem.with.violence.law.recruitment[is.na(progres.specificneed.case2$Problem.with.violence.law.recruitment)]<- "no"
progres.specificneed.case2$Problem.with.violence.law.recruitment <- as.factor(progres.specificneed.case2$Problem.with.violence.law.recruitment)


progres.specificneed.case2$Separated.Child.count <- progres.specificneed.case2$Separated.Child
progres.specificneed.case2$Separated.Child <- as.factor(ifelse(progres.specificneed.case2$Separated.Child>=1, "yes", "no"))  
progres.specificneed.case2$Separated.Child <- as.character(progres.specificneed.case2$Separated.Child)
progres.specificneed.case2$Separated.Child[is.na(progres.specificneed.case2$Separated.Child)]<- "no"
progres.specificneed.case2$Separated.Child <- as.factor(progres.specificneed.case2$Separated.Child)


progres.specificneed.case2$Single.Parent.count <- progres.specificneed.case2$Single.Parent
progres.specificneed.case2$Single.Parent <- as.factor(ifelse(progres.specificneed.case2$Single.Parent>=1, "yes", "no"))  
progres.specificneed.case2$Single.Parent <- as.character(progres.specificneed.case2$Single.Parent)
progres.specificneed.case2$Single.Parent[is.na(progres.specificneed.case2$Single.Parent)]<- "no"
progres.specificneed.case2$Single.Parent <- as.factor(progres.specificneed.case2$Single.Parent)


progres.specificneed.case2$Unaccompanied.count <- progres.specificneed.case2$Unaccompanied
progres.specificneed.case2$Unaccompanied <- as.factor(ifelse(progres.specificneed.case2$Unaccompanied>=1, "yes", "no"))  
progres.specificneed.case2$Unaccompanied <- as.character(progres.specificneed.case2$Unaccompanied)
progres.specificneed.case2$Unaccompanied[is.na(progres.specificneed.case2$Unaccompanied)]<- "no"
progres.specificneed.case2$Unaccompanied <- as.factor(progres.specificneed.case2$Unaccompanied)


progres.specificneed.case2$Victim.of.Violence.count <- progres.specificneed.case2$Victim.of.Violence
progres.specificneed.case2$Victim.of.Violence <- as.factor(ifelse(progres.specificneed.case2$Victim.of.Violence>=1, "yes", "no"))  
progres.specificneed.case2$Victim.of.Violence <- as.character(progres.specificneed.case2$Victim.of.Violence)
progres.specificneed.case2$Victim.of.Violence[is.na(progres.specificneed.case2$Victim.of.Violence)]<- "no"
progres.specificneed.case2$Victim.of.Violence <- as.factor(progres.specificneed.case2$Victim.of.Violence)



progres.specificneed.case2$Woman.at.Risk.count <- progres.specificneed.case2$Woman.at.Risk
progres.specificneed.case2$Woman.at.Risk <- as.factor(ifelse(progres.specificneed.case2$Woman.at.Risk>=1, "yes", "no"))  
progres.specificneed.case2$Woman.at.Risk <- as.character(progres.specificneed.case2$Woman.at.Risk)
progres.specificneed.case2$Woman.at.Risk[is.na(progres.specificneed.case2$Woman.at.Risk)]<- "no"
progres.specificneed.case2$Woman.at.Risk <- as.factor(progres.specificneed.case2$Woman.at.Risk)




###################################################
### merging back with progres case info 
###################################################

progres.case.sp <- merge(x=progres.case, y=progres.specificneed.case2, all.x=TRUE)
rm(progres.specificneed.case2)

#names(progres.case.sp)


###################################################
######## Recode the address information 
###################################################

#names(data)

## if the admin unit is not in the correct country -- then fill with "Data Entry Mistake"
## if the admin unit is not available, then "Not reported"

#table(progres.case.sp$cool1id)

#CountryOrigin"               
#"cool1id"  
#"cool2id"   
#"cool3id"   
#"cool4id"  

#"CountryAsylum" 
#"coal1id"  
#"coal2id" 
#"coal3id"   
#"coal4id" 

###################################################
######## Saving reworked case information
###################################################

write.csv(progres.case.sp, file = "data/progrescase-1.csv",na="")



#data <- progres.case.sp

rm(assistancecase)
rm(progres.case)
rm(progres.case.sp)
rm(progres.specificneed)
rm(progres.specificneed.case)
rm(progres.specificneed.case2)
rm(progres.specificneed.unique)

# rm(progres.case.sp)

### Cleaning data

## Remove observation where we do not have PA Age or Arrival Date or Sex of PA
#data <- data[!rowSums(is.na(data["dem_age"])), ]
#data <- data[!rowSums(is.na(data["AVG_Age"])), ]
#data <- data[!rowSums(is.na(data["YearArrival"])), ]
#data <- data[data$dem_sex !="Unknown", ]

#write.csv(data, file = "data/progrescase2.csv",na="")

## Description of all variables 
#data.str <- strtable(data, factor.values=as.integer)