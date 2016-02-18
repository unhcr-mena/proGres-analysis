
source("code/0-packages.R")

## Exploration of data will provide a quick overview 

rm(progres.spneedevent)
progres.spneedevent <- read.csv("data/spneedevent.csv")
#str(progres.spneedevent)


## Description of all variables 
indidata.str <- strtable(progres.spneedevent, factor.values=as.integer)

################################################################
## Cross tabulation of Individuals Event
################################################################



prop.table(table(progres.spneedevent$CoO,progres.spneedevent$CoA),1)*100
