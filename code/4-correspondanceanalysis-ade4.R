source("code/0-packages.R")

#rm(data.sp.dep.rst)
#data <- read.csv("data/progrescase2.csv")


data <- data.sp.dep.rst


####################################################################################
#### Apply MCA (Multiple Correspondance Analysis) to Categorical data
####################################################################################
## http://rpubs.com/gaston/MCA

data.JOR <-data[ data$CountryAsylum == "JOR", ]
## We need to generate sample in order to create the MCA object
data.JOR.sample <- data.JOR[sample(1:nrow(data.JOR), 1000,replace=FALSE),
                            c("Num_Inds2", "dem_marriage", "dem_sex",
                              "dependency", "youthdependency", "elederndependency",
                              "agecohort", "AVGAgecohort", "STDEVAgeclass",
                              "YearArrivalCategory", "season",
                              "CountryOriginCategory","CountryAsylum",
                              "occupationcat", "edu_highestcat")]
data.sample <- droplevels(data.JOR.sample)



### MCA with ade4

library(ade4)

acm <- dudi.acm(data.JOR.sample, scan = FALSE)

warning(scatter(acm, col = rep(c("black", "red3", "darkblue"), 2)))

summaryacm <- data.frame(
  EIG = acm$eig,
  PCTVAR = 100 * acm$eig / sum(acm$eig),
  CUMPCTVAR = cumsum(100 * acm$eig / sum(acm$eig))
)
barplot(summaryacm$PCTVAR,
        xlab = "Componants",
        ylab = "Percentage of variance (inertia)",
        names = paste("C", seq(1, nrow(summaryacm), 1)),
        col = "black",
        border = "white")

plot(acm$li, pch = 20, col = "grey40")
abline(h=0, v=0)
#points(acm$co, type = "o", pch = 18, col = "black")
text(acm$co,
     labels = row.names(acm$co),
     cex = 0.8,
     pos = c(rep(4, times = 3), 1, rep(4, times = 4), 3))

contribacm <- inertia.dudi(acm, row.inertia = TRUE, col.inertia = TRUE)

par(mfrow = c(1, 2), mar = c(5, 4, 2, 0))

barplot(acm$cr[, 1], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(progres.case.test),
        las = 1, main = "First factor", col = "lightblue", xlab = "Correlation")
barplot(acm$cr[, 2], horiz = TRUE, xlim = c(0, 1), names.arg = colnames(progres.case.test),
        las = 1, main = "Second factor", col = "lightblue", xlab = "Correlation")

boxplot(dudi.acm(progres.case.test, scan = FALSE))

##############################

## increase memorey limit for the clusterisation
memory.size(max = FALSE)
memory.limit(size = 4095)

chiDist <- dist.dudi(acm, amongrow = TRUE)
clustered <- ward.cluster(chiDist,
                          #peso = apply(d, 1, sum),
                          plots = TRUE, h.clust = 1)

d$cluster <- paste("Cluster", cutree(clustered, k = 5))
