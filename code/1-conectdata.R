########### Access to proGres registration
library(RODBC)

## Connecting to SQL server using ODBC
progres <- readline("Give the odbc db name:")
user <- readline("Give the username:")
passw <- readline("Give the password:")
dbhandleprogres <- odbcConnect(progres, uid=user, pwd=passw)

## fetching the view containing information aggregated at the case level and the event
progres.case <-  sqlFetch(dbhandleprogres, "caseprofile")


progres.event <- sqlFetch(dbhandleprogres, "caseprofile")


## backup in CSV 
write.csv(progres.case, file = "data/progrescase.csv",na="")


## Db handle for RAIS
rais <- readline("Give the odbc db name:")
dbhandlerais <- odbcConnect(rais, uid=user, pwd=passw)

## fetching the view containing assistance information aggregated at the case level
assistance.case <- sqlQuery(dbhandlerais, 'SELECT * FROM [RaisRepository].[Assistance].[vAssistanceSearch]')
#print(assistance.case)
## backup in CSV 
write.csv(assistance.case, file = "data/assistancecase.csv",na="")

