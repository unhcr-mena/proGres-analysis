source("code/0-packages.R")

data <- read.csv("data/progrescase2.csv")


# names(data)


####################################################################################
#### Apply MCA (Multiple Correspondance Analysis) to Categorical data
####################################################################################
## http://rpubs.com/gaston/MCA

data.JOR <-data[ data$CountryAsylum == "JOR", ]
## We need to generate sample in order to create the MCA object
data.JOR.sample <- data.JOR[sample(1:nrow(data.JOR), 3000,replace=FALSE),
                                    c("Case.size", "dem_marriage", "dem_sex",
                                      "dependency", "youthdependency", "elederndependency",
                                      "agecohort", "AVGAgecohort", "STDEVAgeclass",
                                      "YearArrivalCategory", "season",
                                      "CountryOriginCategory","CountryAsylum",
                                      "occupationcat", "edu_highestcat")]
data.JOR.sample <- droplevels(data.JOR.sample)

#str(data.JOR.sample)

#####################################
#### Loading the required R module FactoMiner

data.JOR.mca <- MCA(data.JOR.sample, graph=FALSE)
summary(data.JOR.mca)
#print(data.mca, nb.dec = 3, nbelements=10, ncp = 3, align.names=TRUE, file = "out/summary-mca.txt")

###################################
## Plotting MC with factoextra

# http://www.sthda.com/english/wiki/factoextra-r-package-quick-multivariate-data-analysis-pca-ca-mca-and-visualization-r-software-and-data-mining 
png(file = "out/mcaviz1.png", bg = "transparent")
fviz_mca_biplot(data.JOR.mca) + theme_minimal()
dev.off()

#  Graph of individuals
png(file = "out/mcaviz-individual.png", bg = "transparent")
fviz_mca_ind(data.JOR.mca, col.ind = "blue", label="none", addEllipses = TRUE, ellipse.level = 0.95,
            # alpha.ind="contrib",
             jitter = list(what= "point")) + 
            #scale_color_gradient2(low="white", mid="blue", high="red", midpoint=0.4, space = "Lab") +
        theme_minimal()
dev.off()

# Variable
png(file = "out/mcaviz-variable.png", bg = "transparent")
fviz_mca_var(data.JOR.mca, labelsize = 4, pointsize = 2, 
                select.var = list(contrib = 20)) +
               theme_minimal()
dev.off()

## Contribution of top 10 variables
png(file = "out/mcaviz-contrib1.png", bg = "transparent")
fviz_contrib(data.JOR.mca, choice = "var", axes = 1, top = 20)+  coord_flip()
dev.off()

png(file = "out/mcaviz-contrib2.png", bg = "transparent")
fviz_contrib(data.JOR.mca, choice = "var", axes = 2, top = 20) +  coord_flip()
dev.off()



#####################################
# Plotting the MCA with better readibility

png(file = "out/mcaviz-base1.png", bg = "transparent")
#pdf("out/mca1.pdf")
plot.MCA(data.JOR.mca, axes=c(1, 2), col.ind="black", col.ind.sup="blue", col.var="darkred", 
         col.quali.sup="darkgreen", label=c("ind", "ind.sup", "quali.sup", "var", "quanti.sup"), #autoLab = "yes",
         invisible=c("none", new.plot=TRUE))
dev.off()


png(file = "out/mcaviz-base2.png", bg = "transparent")
#pdf("out/mca2.pdf")
plot.MCA(data.JOR.mca, axes=c(1, 2), choix="var", col.var="darkred", select = "contrib 10",  cex=0.7,
         col.quali.sup="darkgreen", label=c("var", "quali.sup"), autoLab = "yes",
         invisible=c("none", new.plot=TRUE))
dev.off()


png(file = "out/mcaviz-base3.png", bg = "transparent")
#pdf("out/mca3.pdf")
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
data.JOR.mca.hcpc <- HCPC(data.JOR.mca, nb.clust=-1, min=3, max=4, graph=FALSE, order=FALSE, consol=TRUE)
data.JOR.mca.hcpc$desc.var

data.JOR.mca.hcpc.desc.var <- as.data.frame(data.JOR.mca.hcpc$desc.var$category$`1`)


data.JOR.mca.hcpc$desc.var$category$`1` 

data.JOR.mca.hcpc$desc.axes
data.JOR.mca.hcpc$desc.ind
data.JOR.mca.hcpc$data.clust

#print (data.JOR.mca.hcpc, "out/descriptioncluster.txt")

png(file = "out/mcaviz-cluster3d.png", bg = "transparent")
#pdf("out/cluster1.pdf")
plot(data.JOR.mca.hcpc)
dev.off()

# Plot the dendrogram only
png(file = "out/mcaviz-dendro.png", bg = "transparent")
plot(data.JOR.mca.hcpc, choice ="tree", cex = 0.6)
dev.off()

png(file = "out/mcaviz-cluster.png", bg = "transparent")
#pdf("out/cluster2.pdf")
plot(data.JOR.mca.hcpc, choice="map")
dev.off()




#png(file = "out/mcaviz-cluster2.png", bg = "transparent")
#fviz_cluster(data.JOR.mca.hcpc)
#dev.off()

## other classif 

data.JOR.classif <- agnes(data.JOR.mca$ind$coord,method="ward")

pdf("out/cluster3.pdf")
plot(data.JOR.classif,main="Dendrogram",ask=F,which.plots=2,labels=FALSE)
dev.off()

#ind1.enmca<-ENMCA(ind1, report=FALSE)
#ind3.semantic <- ENmarking(ind3,1)

###################
#  clear graphics settings so that it works with multiple windows

capabilities()
sessionInfo()

