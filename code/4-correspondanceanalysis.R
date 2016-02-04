source("code/0-packages.R")

#rm(data.sp.dep.rst)
#data.sp.dep.rst <- read.csv("data/progrescase2.csv")


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

#str(data.JOR.sample)

#####################################
#### Loading the required R module FactoMiner

data.JOR.mca <- MCA(data.JOR.sample)
summary(data.JOR.mca)
#print(data.mca, nb.dec = 3, nbelements=10, ncp = 3, align.names=TRUE, file = "out/summary-mca.txt")


#####################################
# Plotting the MCA with better readibility

pdf("out/mca1.pdf")
plot.MCA(data.JOR.mca, axes=c(1, 2), col.ind="black", col.ind.sup="blue", col.var="darkred", col.quali.sup="darkgreen", label=c("ind", "ind.sup", "quali.sup", "var", "quanti.sup"), invisible=c("none", new.plot=TRUE))
dev.off()

pdf("out/mca2.pdf")
plot.MCA(data.JOR.mca, axes=c(1, 2), choix="var", col.var="darkred", col.quali.sup="darkgreen", label=c("var", "quali.sup"), invisible=c("none", new.plot=TRUE))
dev.off()

pdf("out/mca3.pdf")
plot.MCA(data.JOR.mca, axes=c(1, 2), choix="quanti.sup", col.quanti.sup="blue", label=c("quanti.sup", new.plot=TRUE))
dev.off()

#plotellipses(data.mca)

#####################################
### Describing axis
round(data.JOR.mca$eig,2)
dimdesc(data.JOR.mca,axes=1:2)
dimdesc(data.JOR.mca,axes=1:2,proba=0.05)
dimdesc(data.JOR.mca,axes=1:2,proba=0.20)
dimdesc(data.JOR.mca,axes=1:2,proba=0.30)
dimdesc(data.JOR.mca,axes=1:2,proba=0.50)

#####################################
### Category description
res.catdes <- catdes(data.JOR.mca, num.var=2, proba = 0.05)
plot(res.catdes)

#####################################
### Hierarchical Clustering on Principal Components
data.JOR.mca.hcpc <- HCPC(data.JOR.mca, nb.clust=-1, min=3, max=5, graph=FALSE, order=FALSE, consol=TRUE)
#data.JOR.mca.hcpc$desc.var
#data.JOR.mca.hcpc$desc.axes
#data.JOR.mca.hcpc$desc.ind
#data.JOR.mca.hcpc$data.clust

print (data.JOR.mca.hcpc, "out/descriptioncluster.txt")

pdf("out/cluster1.pdf")
plot(data.JOR.mca.hcpc)
dev.off()

pdf("out/cluster1.pdf")
plot(data.JOR.mca.hcpc, choice="map")
dev.off()


library(cluster)
classif = agnes(datamca.mca$ind$coord,method="ward")
plot(classif,main="Dendrogram",ask=F,which.plots=2,labels=FALSE)

#ind1.enmca<-ENMCA(ind1, report=FALSE)
#ind3.semantic <- ENmarking(ind3,1)

###################
#  clear graphics settings so that it works with multiple windows
dev.off()
Sys.setenv("DISPLAY"=":0.0")

capabilities()
sessionInfo()
options(device="X11")

