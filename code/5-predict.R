
source("code/0-packages.R")


data <- read.csv("data/progrescase2.csv")

## Reorder factor levels
data$dependency <-  factor(data$dependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$youthdependency <-  factor(data$youthdependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$elederndependency <-  factor(data$elederndependency, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$female.ratio <-  factor(data$female.ratio, levels = c( "0", "(0.001,0.2]", "(0.2,0.4]", "(0.4,0.6]", "(0.6,0.8]", "(0.8,Inf]"))
data$STDEVAgeclass <-  factor(data$STDEVAgeclass, levels = c( "0", "(0.001,5]", "(5,10]", "(10,15]", "(15,20]", "(20,Inf]"))
data$AVGAgecohort <-  factor(data$AVGAgecohort, levels = c( "0", "(0.1,18]", "(18,25]", "(25,35]", "(35,45]","(45,59]", "(59,Inf]"))
data$season <- factor(data$season, levels = c("Spring", "Summer", "Autumn", "Winter"))
data$CountryOriginCategory <- factor(data$CountryOriginCategory, levels = c("SYR","IRQ","AFG","IRN","HORN","AFR", "MENA", "ASIA", "OTH"))
data$Montharrival <- factor(data$Montharrival, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct","Nov","Dec"))
data$edu_highestcat <- factor(data$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
data$dem_marriagecat <- factor(data$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced","Unknown"))
data$occupationcat <- factor(data$occupationcat, levels = c("Manager-Professional-Technician-Technician", "ServiceMarket",
                                                            "Agricultural", "Craft-Machine", "Elementary", "Military",
                                                            "UnknownOccup", "NoOccup", "Student"))


#str(data)

###########################################
## Preparing the data
###########################################

# Output the number of missing values for each column
sapply(data,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(data, function(x) length(unique(x)))

# A visual way to check for missing data using Amelia Library
#missmap(data.original, main = "Missing values vs observed")

###########################################
###### Dataset for modelling - 
###########################################


#   "Child.at.risk", "Disability", "Family.unity", "Older.person.at.risk",
#  "Pregnant.or.lactating", "Serious.medical.condition", "SGBV", "Single.parent",
#  "Specific.legal.physical.protection.needs", "Torture", "Unaccompanied.or.separated.child",

data.JOR <-data[ data$CountryAsylum == "JOR", ]


data.Woman.at.risk <- data.JOR[sample(1:nrow(data.JOR), 10000,replace=FALSE),  c( "Case.size", "dem_marriage", "dem_sex",
                           "dependency", "youthdependency", "elederndependency","female.ratio",
                           "agecohort", "AVGAgecohort", "STDEVAgeclass",
                           "YearArrivalCategory", "season",
                           "CountryOriginCategory",
                           "occupationcat", "edu_highestcat","Woman.at.risk"
                           )]
#summary(data.Woman.at.risk)

# Output the number of missing values for each column
sapply(data.Woman.at.risk,function(x) sum(is.na(x)))

# Check categorical variables encoding for better understanding of the fitted model
contrasts(data$Woman.at.risk)
contrasts(data$Case.size)

# Train test splitting
train <- data.Woman.at.risk[1:10000,]
test <- data.Woman.at.risk[5001:10000,]

###########################################
# Model fitting
###########################################

model.Woman.at.risk <- glm(Woman.at.risk ~.,family=binomial(link='logit'),data=train)
summary(model.Woman.at.risk)

#### Interpreting the results
## https://www.youtube.com/watch?v=nubin7hq4-s

# Analysis of deviance
anova(model.Woman.at.risk,test="Chisq")

# McFadden R^2
pR2(model.Woman.at.risk)

# measuring the productive ability of the model
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results.Woman.at.risk <- predict(model.Woman.at.risk,newdata=subset(test),type='response')
fitted.results.Woman.at.risk <- ifelse(fitted.results.Woman.at.risk > 0.5,1,0)

misClasificError <- mean(fitted.results != test$Woman.at.risk)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
confusionMatrix(data=fitted.results, reference=test$Woman.at.risk)

# ROC and AUC
p <- predict(model, newdata=subset(test,select=c(2,3,4,5,6,7,8)), type="response")
pr <- prediction(p, test$Woman.at.risk)
# TPR = sensitivity, FPR=specificity
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc


   


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



