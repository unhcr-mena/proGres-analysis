####################################################################################
#### Apply MCA (Multiple Correspondance Analysis) to Categorical data
####################################################################################
## http://rpubs.com/gaston/MCA



### creatting a smaller dataset
progres.case.test <-progres.case[ progres.case$coal1 == "Irbid", c("CountryOrigin",
                                                                   "season",
                                                                   "YearArrival" ,
                                                                   "Num_Inds2",
                                                                   "dem_marriage",
                                                                   "dem_sex",
                                                                   "dem_ethn",
                                                                   "dem_religion",
                                                                   "cool1",
                                                                   "coal2",
                                                                   "edu_highest_t", "occupationcode")  ]
str(progres.case.test)
progres.case.test <- droplevels(progres.case.test)


## We need to generate sample in order to create the MCA object
progres.case.sample <- progres.case[sample(1:nrow(progres.case), 5000,replace=FALSE),
                                    c("CountryOrigin",
                                    "season",
                                     "YearArrival" ,
                                     "Num_Inds2",
                                     "dem_marriage",
                                     "dem_sex",
                                     "dem_ethn",
                                     "dem_religion",
                                    # "cool1",
                                    # "coal2",
                                     "edu_highest_t")
                                    ]
progres.case.sample$YearArrival <- as.factor(progres.case.sample$YearArrival)

progres.case.sample <- droplevels(progres.case.sample)

str(progres.case.sample)

#### Loading the required R module
library("FactoMineR")

progres.case.mca <- MCA(progres.case.sample)
summary(progres.case.mca)

print(progres.case.mca, nb.dec = 3, nbelements=10, ncp = 3, align.names=TRUE, file = "out/summary-mca.txt")


#########
# Plotting the MCA with better readibility
#########
plot.MCA(progres.case.mca, axes=c(1, 2), col.ind="black", col.ind.sup="blue", col.var="darkred", col.quali.sup="darkgreen", label=c("ind", "ind.sup", "quali.sup", "var", "quanti.sup"), invisible=c("none", new.plot=TRUE))
dev.off()
plot.MCA(progres.case.mca, axes=c(1, 2), choix="var", col.var="darkred", col.quali.sup="darkgreen", label=c("var", "quali.sup"), invisible=c("none", new.plot=TRUE))
dev.off()
plot.MCA(progres.case.mca, axes=c(1, 2), choix="quanti.sup", col.quanti.sup="blue", label=c("quanti.sup", new.plot=TRUE))

#plotellipses(progres.case.mca)


### Describing axis
round(progres.case.mca$eig,2)
dimdesc(progres.case.mca,axes=1:2)
dimdesc(progres.case.mca,axes=1:2,proba=0.05)
dimdesc(progres.case.mca,axes=1:2,proba=0.20)
dimdesc(progres.case.mca,axes=1:2,proba=0.30)
dimdesc(progres.case.mca,axes=1:2,proba=0.50)

### Category description
res.catdes <- catdes(progres.case.mca, num.var=2, proba = 0.05)
plot(res.catdes)


## Hierarchical clustering

progres.case.hcpc<-HCPC(progres.case.mca ,nb.clust=-1,consol=TRUE,min=3,max=6,graph=TRUE)
progres.case.hcpc$desc.var
progres.case.hcpc$desc.axes
progres.case.hcpc$desc.ind



ind1.enmca<-ENMCA(ind1, report=FALSE)


ind3.semantic <- ENmarking(ind3,1)



###################
#  clear graphics settings so that it works with multiple windows
dev.off()
Sys.setenv("DISPLAY"=":0.0")

capabilities()
sessionInfo()
options(device="X11")

