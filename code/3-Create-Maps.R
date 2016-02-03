
source("code/0-packages.R")

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")

data <- progres.case.sp.dep.rst


locationasylum1 <- unique(data[,c("CountryAsylum","coal1")])
locationasylum2 <- unique(data[,c("CountryAsylum","coal1","coal2")])
locationasylum3 <- unique(data[,c("CountryAsylum","coal1","coal2","coal2")])
write.csv(locationasylum, file = "data/locationasylum.csv",na="")