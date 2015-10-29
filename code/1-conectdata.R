########### Access to proGres registration
library(RODBC)

## Connecting to SQL server using ODBC
db <- readline("Give the username:")
user <- readline("Give the username:")
passw <- readline("Give the password:")
dbhandle <- odbcConnect(db, uid=user, pwd=passw)


## fetching the view containing information aggregated at the case level
progres.case <-  sqlFetch(dbhandle, "caseprofile")

## backup in CSV 
write.csv(progres.case, file = "data/progrescase.csv",na="")
