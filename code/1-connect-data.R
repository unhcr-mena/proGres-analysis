########### Access to proGres registration
library(RODBC)

## Connecting to SQL server using ODBC

#########################################
## Db handle for progres Data warehouse
#########################################

progres <- readline("Give the odbc db name:")
user <- readline("Give the username:")
passw <- readline("Give the password:")
dbhandleprogres <- odbcConnect(progres, uid=user, pwd=passw)

## fetching the view containing information aggregated at the case level and the event
progres.case <-  sqlFetch(dbhandleprogres, "caseprofile")
## backup in CSV 
write.csv(progres.case, file = "data/progrescase.csv",na="")

## Specific need per individual
progres.specificneed <- sqlFetch(dbhandleprogres, "casespecificneed")
write.csv(progres.specificneed, file = "data/progresspecificneed.csv",na="")

## Focusing on REG38	Spontaneous Departure event
progres.event        <- sqlFetch(dbhandleprogres, "caseevent")
write.csv(progres.event, file = "data/progresevent.csv",na="")

progres.eventrst        <- sqlFetch(dbhandleprogres, "caseeventrst19")
write.csv(progres.eventrst, file = "data/progreseventrst.csv",na="")


#################################
## Db handle for RAIS
#################################
rais <- readline("Give the odbc db name:")
dbhandlerais <- odbcConnect(rais, uid=user, pwd=passw)

## fetching the view containing assistance information aggregated at the case level
assistance.case <- sqlQuery(dbhandlerais, 'SELECT * FROM [RaisRepository].[Assistance].[vAssistanceSearch]')
write.csv(assistance.case, file = "data/assistancecase.csv",na="")

