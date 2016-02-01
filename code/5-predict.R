
rm(progres.case.sp)
progres.case.sp <- read.csv("data/progrescase2.csv")


###########################################
## predictig specific needs
###########################################
#progres.case.Family.unity <- progres.case.sp[ progres.case.sp$Family.unity >= 1, ]
progres.case.sp$Family.unity.yes[progres.case.sp$Family.unity >= 1, ] <- 'yes'
str(progres.case.sp)
cor(progres.case.sp)    

boxplot(progres.case.sp$Family.unity)

reg.Family.unity <- glm(Family.unity ~ edu_highest_t + 
                          dem_marriage +
                          YearArrival + 
                          CountryOrigin  +
                          CountryAsylum +
                          occupationcat ,
                        family=binomial(logit),
                        # family=binomial,
           data=progres.case.sp)
summary(reg.Family.unity)

# AUTOMATED STEPWISE REGRESSIONS
##########################################################################
## Allow to double check explanatory power - rsquared-  of the variables for the expenditure per capita
## Caution: this is a heavy calculation -> we need to split the Df to make it manageable

options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 1
while (i<650)
{
  reg <- glm(Family.unity ~ progres.case.sp[,i], data=progres.case.sp);
  rsquared <- summary(reg)$r.squared;
  col1 <- as.character (i);
  col2 <- as.character (colnames(hve.model)[i]);
  col3 <- rsquared;
  hvAR <- rbind(hvAR,c(col1, col2, col3))
  i <- i + 1;
}

## Order the hVar dataframe by descending rsquared
hvAR <- hvAR[order(- rsquared),]

## We keep a backup of this:
write.csv(hvAR, file = "out/hvAR.csv")

## Let's check the top 10 variables with the biggest Rsquarred -- This will give us the variable to be included for further regression
head(hvAR, n=10)






###########################################
### Predicting RST case accepted



#############################################
### Predicting Spontaneous Departure



