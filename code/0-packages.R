##

################################################################
### Uncomment to load the packages used in this analysis
#lab.packages <- c("lattice", "gmodels", "car","ggplot2","extrafont","ggthemes","zoo","reshape2",
#"maptools","rgdal","rgeos","ggmap","sp",")
#install.packages(pkgs=lab.packages)

packages <- c("ggplot2", # package for elegant data visualization using the Grammar of Graphics
              "Hmisc", # generate a detailled describtion of a given dataset 
             # "AER",  # interesting datasets
              "lattice",
              "tufte",
              "VGAM",
              "aod",
              "fields", 
              "stringr", # manipulation of string data
              "ellipse",
              "pastecs","XML",
              "devtools", # package used to load packages hosted in github -- install CURL before and separately
              "plyr","hexbin",
              "vcd", # Visualisation of categorical data
              "reshape2", # package to easily melt data to long form
              "RColorBrewer", # a package offering color palette from 
              "viridis", # another color palette,
              "extrafont", ##" load additional font
              "sp","maptools","rgdal","rgeos","ggmap","cartography", "SpatialPosition",
             "sqldf", "geojsonio",  #PBSmapping", ## packages used for the maps --
              ## install gdal and geos separately before  http://robinlovelace.net/r/2013/11/26/installing-rgdal-on-ubuntu.html
              "raster","classInt","lubridate","date","gdata","gridExtra","scales",
              "ggthemes", ## load different custmised theme: excel, stata, economist, tufte, wall street journal...
              "xkcd", ## Style from the xkcd comics 
              "Amelia",
              "rattle",
              "gvlma", "glmulti",
              "scatterplot3d", "cluster", 
              "ade4",  "psych", 
              "ada", "ade4", "arules", "arulesViz", "boot",
              "C50", "car", "caret", #"CHAID",
              "combinat","cluster",
              "corrplot", "doSNOW", "e1071", "extraTrees",
              "FactoMineR", "foreach", "foreign", "gbm", 
              "prabclus",
              "glmnet", "gmodels", "grplasso", "ipred",
              "kernlab", "leaps", "LiblineaR",
              "MASS", "missForest", "nnet", "plsRglm", "misc3d",
              "prim", "pROC", "pscl",
              "questionr", "randomForest",
              "randtoolbox", "rgl", #"rgrs",
              "ROCR", 
              "rpart", "rpart.plot", # "sas7bdat", 
              "snow", "speedglm", "tree",
             "animation","spacetime",
             "gridExtra","directlabels",
              "prettyR","xtable","knitr","pander",
             "files", #for creation of new folders
              "formatR" #, "gWidgetsRGtk2" # used to format the code
              #"XLConnect" ## Read and write excel files
              
)
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

rm(packages)

# install.packages("devtools")
# library("devtools")
# install_github("kassambara/factoextra")
# install_github('Rapporter/pander')

#devtools::install_github("wilkelab/cowplot", force=TRUE)
library(cowplot)

# loads packages into memory
#library(prettyR)
library(plyr)
library(reshape2) ## Restructure data between wide and long format before plotting them - melt and cast
#gpclibPermit()
library(zoo) ## Manage reformatting of date
library(date)
library(lubridate)

############################################
#### graphic 
library(ggplot2) ## The grammar of graphics!
library(directlabels)
library(extrafont) ## Additional fonts
library(ggthemes) ## Additional themes for gplot2
library(gdata)
library(gridExtra)
#library(scales)

#library(lattice)

############################################
#### Spatial Packages
library(maptools) ## Create maps
library(rgdal) ## Open geographic files
library(rgeos)
library(ggmap) ## get background map from google map
library(sp) ## Spatial library
#library(raster) ## Managing raster dataset
library(classInt) ## Classififcation
library(hexbin) ## Hexa binning
library(geojsonio)
library(cartography)
library(SpatialPosition)

############################################
## Code reformatting
#library(formatR)
#library(RGtk2)
#library(gWidgetsRGtk2)
## tidy.gui('RGtk2')
library(xtable)
library(knitr)
library(pander)  ## library to format regression output in Rmd
library(tufte)
library(sqldf)  ## enables sql queries

############################################
#### Regressions

#library(Amelia) ## For missing data

#library(rattle)
#library(ada)
#library(ade4)
#library(arules)
#library(arulesViz)
#library(boot)
#library(C50)
library(car)
library(caret)
#library(CHAID)
#library(combinat)
library(cluster)
library(corrplot)
#library(doSNOW)
#library(e1071)
#library(extraTrees)
library(FactoMineR) ## Multiple correspondance analysis and classification
library(factoextra)
#library(foreach)
#library(gbm)
library(glmnet)
#library(glmulti)
library(gmodels)
#library(grplasso)
#library(ipred)
#library(kernlab)
#library(leaps)
#library(LiblineaR)
#library(MASS)
#library(missForest)
#library(nnet)
#library(plsRglm)
#library(prim)
#library(pscl) ## used to verify the prediction power of a logistic model
library(pROC)
#library(questionr)
library(randomForest)
#library(randtoolbox)
#library(rgl)
#library(rgrs)
#library(ROCR)
#library(rpart)
library(rpart.plot)
#library(snow)
#library(speedglm)
#library(tree)

########################################
### Read other format
#library(foreign)
#library(sas7bdat)
library(gridExtra)


############################################
## Color palette
library(RColorBrewer) 
library(viridis) 

############################################
## Folder creation
library(files)

#display.brewer.all()
# Choose a qualitative color palette with blue and red
#display.brewer.pal(2, 'Set1')
# Warning tells us we need to request 3+ color levels;
# Just save the first two levels: first blue, then red
#cbQualColors = brewer.pal(3, 'Set1')[c(2, 1)]
#cbQualColors # saved as character strings of hex values
# Use a sequential color scheme for the 7 MONTHS values;
# first few are too light, so request more colors and only use later ones
#display.brewer.pal(9, 'YlGn')
#cbSeqColors = brewer.pal(9, 'YlGn')[3:9]

### Customised theme
### http://docs.ggplot2.org/dev/vignettes/themes.html
# theme_edouard()

theme_edouard <- function(base_size = 12) {
  structure(list(
    axis.line =         theme_blank(),
    axis.text.x =       theme_text(size = base_size * 0.6 , lineheight = 0.9, vjust = 1),element_text(family="Helvetica"),
    axis.text.y =       theme_text(size = base_size * 0.6, lineheight = 0.9, hjust = 1),element_text(family="Helvetica"),
    axis.ticks =        theme_segment(colour = "black", size = 0.2),
    axis.title.x =      theme_text(size = base_size, vjust = 1),
    axis.title.y =      theme_text(size = base_size, angle = 90, vjust = 0.5),
    axis.ticks.length = unit(0.3, "lines"),
    axis.ticks.margin = unit(0.5, "lines"),
    
    legend.background = theme_rect(colour=NA), 
    legend.key =        theme_rect(colour = "grey80"),
    legend.key.size =   unit(1.2, "lines"),
    legend.text =       theme_text(size = base_size * 0.7),element_text(family="Helvetica"),
    legend.title =      theme_text(size = base_size * 0.8, face = "bold", hjust = 0),element_text(family="Helvetica"),
    legend.position =   "right",
    
    panel.background =  theme_rect(fill = "white", colour = NA), 
    panel.border =      theme_rect(fill = NA, colour="grey50"), 
    panel.grid.major =  theme_line(colour = "grey90", size = 0.2),
    panel.grid.minor =  theme_line(colour = "grey98", size = 0.5),
    panel.margin =      unit(0.25, "lines"),
    
    strip.background =  theme_rect(fill = "grey80", colour = "grey50"), 
    strip.text.x =      theme_text(size = base_size * 0.6),element_text(family="Helvetica", face="italic"),
    strip.text.y =      theme_text(size = base_size * 0.6, angle = -90),element_text(family="Helvetica", face="italic"),
    
    plot.background =   theme_rect(colour = NA),
    plot.title =        theme_text(size = base_size * 1.2),element_text(family="Helvetica", face="bold"),
    plot.margin =       unit(c(1, 1, 0.5, 0.5), "lines")
  ), class = "options")
}



########### Likert Analysis
## http://jason.bryer.org/likert/
#library(devtools)
#install_github('jbryer/likert')
#library(likert)

#
format_si <- function(...) {
  # Format a vector of numeric values according
  # to the International System of Units.
  # http://en.wikipedia.org/wiki/SI_prefix
  #
  # Based on code by Ben Tupper
  # https://stat.ethz.ch/pipermail/r-help/2012-January/299804.html
  # Args:
  #   ...: Args passed to format()
  #
  # Returns:
  #   A function to format a vector of strings using
  #   SI prefix notation
  #
  
  function(x) {
    limits <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12,
                1e-9,  1e-6,  1e-3,  1e0,   1e3,
                1e6,   1e9,   1e12,  1e15,  1e18,
                1e21,  1e24)
    prefix <- c("y",   "z",   "a",   "f",   "p",
                "n",   "µ",   "m",   " ",   "k",
                "M",   "G",   "T",   "P",   "E",
                "Z",   "Y")
    
    # Vector with array indices according to position in intervals
    i <- findInterval(abs(x), limits)
    
    # Set prefix to " " for very small values < 1e-24
    i <- ifelse(i==0, which(limits == 1e0), i)
    
    paste(format(round(x/limits[i], 1),
                 trim=TRUE, scientific=FALSE, ...),
          prefix[i])
  }
}


# Function that will sum values even if we have NA
psum <- function(..., na.rm=FALSE) {
  x <- list(...)
  rowSums(matrix(unlist(x), ncol=length(x)), na.rm=na.rm)
}




IntersectPtWithPoly <- function(x, y) {
  # Extracts values from a SpatialPolygonDataFrame with
  # SpatialPointsDataFrame, and appends table (similar to
  # ArcGIS intersect)
  # Args:
  #   x: SpatialPoints*Frame
  #   y: SpatialPolygonsDataFrame
  # Returns:
  # SpatialPointsDataFrame with appended table of polygon attributes
  
  # Set up overlay with new column of join IDs in x
  z <- over(y, x)
  
  # Bind captured data to points dataframe
  x2 <- cbind(x, z)
  
  # Make it back into a SpatialPointsDataFrame
  # Account for different coordinate variable names
  if(("coords.x1" %in% colnames(x2)) & ("coords.x2" %in% colnames(x2))) {
    coordinates(x2) <- ~coords.x1 + coords.x2
  } else if(("x" %in% colnames(x2)) & ("x" %in% colnames(x2))) {
    coordinates(x2) <- ~x + y
  }
  
  # Reassign its projection if it has one
  if(is.na(CRSargs(x@proj4string)) == "FALSE") {
    x2@proj4string <- x@proj4string
  }
  return(x2)
}

#' Creates a \code{data.frame} version of the str function for data.frames.
#' 
#' Note that this function only works with \code{data.frames}. The function
#' will throw an error for any other object types.
#' 
#' @param n the first n element to show
#' @param width maximum width in characters for the examples to show
#' @param n.levels the first n levels of a factor to show.
#' @param width.levels maximum width in characters for the number of levels to show.
#' @param factor.values function defining how factor examples should be printed.
#'        Possible values are \code{as.character} or \code{as.integer}.
#' @export
#' @examples
#' data(iris)
#' str(iris)
#' strtable(iris)
#' strtable(iris, factor.values=as.integer)
strtable <- function(df, n=4, width=60, 
                     n.levels=n, width.levels=width, 
                     factor.values=as.character) {
  stopifnot(is.data.frame(df))
  tab <- data.frame(variable=names(df),
                    class=rep(as.character(NA), ncol(df)),
                    levels=rep(as.character(NA), ncol(df)),
                    examples=rep(as.character(NA), ncol(df)),
                    stringsAsFactors=FALSE)
  collapse.values <- function(col, n, width) {
    result <- NA
    for(j in 1:min(n, length(col))) {
      el <- ifelse(is.numeric(col),
                   paste0(col[1:j], collapse=', '),
                   paste0('"', col[1:j], '"', collapse=', '))
      if(nchar(el) <= width) {
        result <- el
      } else {
        break
      }
    }
    if(length(col) > n) {
      return(paste0(result, ', ...'))
    } else {
      return(result)
    }
  }
  
  for(i in seq_along(df)) {
    if(is.factor(df[,i])) {
      tab[i,]$class <- paste0('Factor w/ ', nlevels(df[,i]), ' levels')
      tab[i,]$levels <- collapse.values(levels(df[,i]), n=n.levels, width=width.levels)
      tab[i,]$examples <- collapse.values(factor.values(df[,i]), n=n, width=width)
    } else {
      tab[i,]$class <- class(df[,i])[1]
      tab[i,]$examples <- collapse.values(df[,i], n=n, width=width)
    }
    
  }
  
  class(tab) <- c('strtable', 'data.frame')
  return(tab)
}

#' Prints the results of \code{\link{strtable}}.
#' @param x result of code \code{\link{strtable}}.
#' @param ... other parameters passed to \code{\link{print.data.frame}}.
#' @export
print.strtable <- function(x, ...) {
  NextMethod(x, row.names=FALSE, ...)
}



## Function to summarize data, source (http://www.cookbook-r.com/Manipulating_data/Summarizing_data/):
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  
  return(datac)
}




##################################################################
## styling theme for maps
## theme for general text and borders which is the same in all maps
theme.base <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Calibri", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.spacing = unit(c(-.1,1,0.02,1), "cm"),
      plot.caption = element_text(size = 9, hjust = 0, color = "#595851"),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      ...
    )
}

## theme for main choropleth map
theme.choropleth <- function(...) {
  theme(
    plot.title = element_text(hjust = 0, color = "black", size = 19, face="bold"),
    plot.subtitle = element_text(hjust = 0, color = "black", size = 13, debug = F),
    
    legend.direction = "horizontal",
    legend.position = "bottom", #c(0.5, 0.01),
    legend.text.align = 1,                                                #postition of label: 0=left, 1=right
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.text = element_text(size = 12, hjust = 0, color = "black"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    legend.key.height = unit(4, units = "mm"),                             #height of legend
    legend.key.width = unit(100/length(labels), units = "mm"),             #width of legend
    legend.title = element_text(size = 0),           #put to 0 to remove title but keep space
    
    panel.grid.major = element_line(color = "#f5f5f2", size = 0.2),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = unit(c(.5,2,1,1.5), "cm"),
    
    ...
  )
}

## theme for bubble map absolute number of datarows
theme.symbol <-  function(...) {
  theme(
    legend.title = element_text(size=12, face="bold", color = "black", hjust = 1),  
    plot.title = element_text(size=12, face="bold", color = "black", hjust = 1), 
    plot.subtitle = element_text(size=17, face="bold", color = "black", hjust = 1),  
    legend.direction = "vertical",
    legend.position = "right", #c(0.5, 0.01),
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12, color = "black"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.margin = unit(c(0.3,0.3,0.3,0,3), "cm"),
    
    ...
  )
}

## theme for choropleth map margin of error
theme.confidence <- function(...) {
  theme(
    legend.title = element_text(size=12, face="bold", color = "black"), 
    legend.direction = "vertical",
    legend.position = "right", #c(0.15, 1),                              
    legend.background = element_rect(fill = "white", color = NA),
    legend.text = element_text(size = 12, color = "black"),
    legend.margin = unit(c(1,.5,0.2,.5), "cm"),
    legend.key.height = unit(7, units = "mm"),            #height of legend
    legend.key.width = unit(6, units = "mm"),             #width of legend
    legend.key.size = unit(1.5, 'lines'),
    
    panel.background = element_rect(fill = "white", color = NA), 
    plot.background = element_rect(fill = "white", color = NA), 
    panel.grid.major = element_line(color = "white", size = 0.2),
    plot.margin = unit(c(0.3,1.05,0.3,0,3), "cm"),
    
    ...
  )
}
