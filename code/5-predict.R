
source("code/0-packages.R")

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")


data.original <- progres.case.sp.dep.rst

###########################################
## Preparing the data
###########################################


# Output the number of missing values for each column
sapply(data.original,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(data.original, function(x) length(unique(x)))

# A visual way to check for missing data using Amelia Library
#missmap(data.original, main = "Missing values vs observed")

###########################################
###### Dataset for modelling - 
###########################################

# 
data <- data.orginal[ , c( "Num_Inds2", "dem_marriage", "dem_sex",
                           "dependency", "youthdependency", "elederndependency",
                           "agecohort", "AVGAgecohort", "STDEVAgeclass",
                           "YearArrivalCategory", "season",
                           "CountryOriginCategory","CountryAsylum",
                           "occupationcat", "edu_highestcat")]


# Train test splitting
train <- data[1:500000,]
test <- data[500001:887000,]

###########################################
# Model fitting
###########################################

model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)

# Analysis of deviance
anova(model,test="Chisq")

# McFadden R^2
pR2(model)

# measuring the productive ability of the model
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(model,newdata=subset(test,select=c(2,3,4,5,6,7,8)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Survived)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
confusionMatrix(data=fitted.results, reference=test$Survived)

# ROC and AUC
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Survived)
# TPR = sensitivity, FPR=specificity
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


###########################################
## predictig specific needs
###########################################
#progres.case.Family.unity <- data[ data$Family.unity >= 1, ]
data$Family.unity.yes[data$Family.unity >= 1, ] <- 'yes'
str(data)
cor(data)    



reg.Family.unity <- glm(Family.unity ~ edu_highest_t + 
                          dem_marriage +
                          YearArrival + 
                          CountryOrigin  +
                          CountryAsylum +
                          occupationcat ,
                        family=binomial(logit),
                        # family=binomial,
           data=data)
summary(reg.Family.unity)


##################
### Partioning
## ?rpart

Family.unity.part <- rpart(Family.unity,data=data,control=rpart.control(minsplit=20,cp=0))

rpart.plot(Family.unity.part,extra=4)

## trying 3 classification
fit <- rpart(Family.unity ~ edu_highest_t + 
               dem_marriage +
               YearArrival, data = data)
rpart.plot(fit)

fit2 <- rpart(Family.unity ~ edu_highest_t + 
                dem_marriage +
                YearArrival + 
                CountryOrigin  +
                CountryAsylum +
                occupationcat, data = data,
              parms = list(prior = c(.65,.35), split = "information"))
fit3 <- rpart(Family.unity ~ edu_highest_t + 
                dem_marriage +
                YearArrival + 
                CountryOrigin  +
                CountryAsylum +
                occupationcat, data = data,
              control = rpart.control(cp = 0.05))


## displaying decision three
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)


par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
rpart.plot(fit,extra=4)
rpart.plot(fit2,extra=4)


# AUTOMATED STEPWISE REGRESSIONS
##########################################################################
## Allow to double check explanatory power - rsquared-  of the variables for the expenditure per capita
## Caution: this is a heavy calculation -> we need to split the Df to make it manageable

options(stringsAsFactors=FALSE)
hvAR <- data.frame("ColumnNumber" = character(250), "ColumnName" = character(250), "rsquared" = numeric(5))
i <- 1
while (i<650)
{
  reg <- glm(Family.unity ~ data[,i], data=data);
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



