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

# Specific need per individual


## With general needs
progres.specificneed <- sqlFetch(dbhandleprogres, "T_SPneedsBreak")
write.csv(progres.specificneed, file = "data/progresspecificneed.csv",na="")


## With Detailled Specific needs
progres.specificneed.case <- sqlFetch(dbhandleprogres, "T_SPneedsCaseLevel")
write.csv(progres.specificneed.case, file = "data/progresspecificneedcase.csv",na="")

## Event @ individual level

## Event with Specific need at Individual level
progres.spneedevent        <- sqlFetch(dbhandleprogres, "T_SPneedsEvents")
write.csv(progres.spneedevent, file = "data/spneedevent.csv",na="")

## Event with Specific need at Case level
progres.spneedeventCase        <- sqlFetch(dbhandleprogres, "T_SPneedsEventsCase")
write.csv(progres.spneedeventCase, file = "data/spneedeventCase.csv",na="")


#################################
## Db handle for RAIS
#################################
rais <- readline("Give the odbc db name:")
dbhandlerais <- odbcConnect(rais, uid=user, pwd=passw)

## fetching the view containing assistance information aggregated at the case level
assistance.case <- sqlQuery(dbhandlerais, 'SELECT * FROM [RaisRepository].[Assistance].[vAssistanceSearch]')
write.csv(assistance.case, file = "data/assistancecase.csv",na="")

rm(dbhandleprogres,passw,progres,user)