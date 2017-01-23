source("code/0-packages.R")

#install.packages("ggsublot", "maps", "plyr")

### load required packages
require(maptools) ## Create maps
require(rgdal) ## Open geographic files
require(rgeos)
require(hexbin)
require(sp)
require(ggplot2)



### Plotting map with mapbox background https://github.com/milafrerichs/plotmapbox
#library(devtools)
#install_github( "milafrerichs/plotmapbox")
#require(plotmapbox)


library(rgeos)
library(sp) 
library(maptools)
library(rgdal)
library(sp)

library(dplyr)
library(geojsonio)

##possible Countries: "DZA", "EGY", "ESH", "GCC", "IRN", "IRQ", "JOR", "LBN", "LBY", "MAR", "MRT", "PSE", "SYR", "TUN", "TUR", "YEM"
mena_countries <- c("DZA", "EGY", "ESH", "GCC", "IRN", "IRQ", "JOR", "LBN", "LBY", "MAR", "MRT", "PSE", "SYR", "TUN", "TUR", "YEM")
#possible admin-levels: Admin-level 1 = "1" and admin-level2 = "2"


##country=JORDAN
#admin-level1
rm(ctrcode)
rm(admlevel)
ctrcode <- mena_countries[7]
admlevel <- "1"
#read geojson from github repository 'p-codes'
geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",ctrcode,"/ADM",admlevel,".geojson")
destfilepath <- paste0("geo/geojson/",ctrcode,"/ADM",admlevel,".geojson" )
download.file( geojsonurl, destfile=destfilepath )
adm1geo <- geojson_read( destfilepath, method="local", what="sp" )
plot(adm1geo)

##country=JORDAN
#admin-level2
rm(admlevel, geojsonurl, destfilepath)
admlevel <- "2"
geojsonurl <- paste0("https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/",ctrcode,"/ADM",admlevel,".geojson")
destfilepath <- paste0("geo/geojson/",ctrcode,"/ADM",admlevel,".geojson" )
download.file( geojsonurl, destfile=destfilepath )
adm2geo <- geojson_read( destfilepath, method="local", what="sp" )
plot(adm2geo)


###data preparation for mapping
#load data from file (please consider to run previous code-files (""1-Recode-data..."))
df <- read.csv("data/progrescase2.csv")
data <- df

## Subset for the country
data <-data[data$CountryAsylum == ctrcode, ]

##mapping integer
#aggregate individual numbers by adminlevel for maps
names(data)
str(data)
data.asylum <- aggregate(cbind(Num_Inds  ) ~ coal1 + CountryAsylum , data = data, FUN = sum, na.rm = TRUE)
data.asylum.AVG_Age <- aggregate(cbind(AVG_Age, STDEV_Age  ) ~ coal1 + CountryAsylum , data = data, FUN = mean, na.rm = TRUE)




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
data$YearArrivalCategory <- factor (data$YearArrivalCategory, levels = c("1900-1980","1981-1990","1991-2000","2001-2005","2006-2010","2011","2012","2013","2014","2015"))
data$edu_highestcat <- factor(data$edu_highestcat, levels = c("Unknown", "Other", "Up to Grade 5", "Grade 6-8", "Grade 9-11", "Grade 12-14", "Higher Education"))
data$dem_marriagecat <- factor(data$dem_marriagecat, levels = c("Engaged", "Single", "Married", "Widowed", "Separated", "Divorced","Unknown"))
data$occupationcat <- factor(data$occupationcat, levels = c("Manager-Professional-Technician-Clerk", "ServiceMarket",
                                                            "Agricultural", "Craft-Machine", "Elementary", "Military",
                                                            "UnknownOccup", "NoOccup", "Student"))





#data.month.year.arrival <- aggregate(cbind(Num_Inds  ) ~ coal1 + CountryAsylum + YearArrivalCategory + Montharrival, data = data, FUN = sum, na.rm = TRUE)
###Potential variables to be mapped.

# dependency 
# youthdependency
# elederndependency
# female.ratio
# STDEVAgeclass 
# AVGAgecohort 
# season 
# CountryOriginCategory 
# Montharrival 
# edu_highestcat 
# dem_marriagecat 
# occupationcat

#install.packages("reshape2") ## install the package in you station - do it only once
#
library("reshape2") ## load the package in your session

data.asylum.Case.size <- dcast((melt(data, id=c(13), measure=c(45))), coal1 ~ value )
data.asylum.CountryOriginCategory <- dcast((melt(data, id=c(13), measure=c(68))), coal1 ~ value )
data.asylum.YearArrivalCategory <- dcast((melt(data, id=c(13), measure=c(67))), coal1 ~ value )
data.asylum.edu_highestcat <- dcast((melt(data, id=c(13), measure=c(71))), coal1 ~ value )
data.asylum.occupationcat <- dcast((melt(data, id=c(13), measure=c(77))), coal1 ~ value )
data.asylum.dem_marriage <- dcast((melt(data, id=c(13), measure=c(31))), coal1 ~ value )
data.asylum.dem_sex <- dcast((melt(data, id=c(13), measure=c(35))), coal1 ~ value )
data.asylum.dem_ethn <- dcast((melt(data, id=c(13), measure=c(36))), coal1 ~ value )
data.asylum.dem_religion <- dcast((melt(data, id=c(13), measure=c(37))), coal1 ~ value )

data.asylum.season <- dcast((melt(data, id=c(13), measure=c(69))), coal1 ~ value )
data.asylum.dependency <- dcast((melt(data, id=c(13), measure=c(46))), coal1 ~ value )


#colnames(data.asylum$coal1) <- "name"
colnames(data.asylum)[1] <- "name"
data.origin <- aggregate(cbind(Num_Inds, CountryOrigin) ~ cool1 , data = data, FUN = sum, na.rm = TRUE)






# Fortify them
adm1geo@data$id = rownames(adm1geo@data)

rm(adm1geo_f)
adm1geo_f <- fortify(adm1geo, region="id")
#adm1geo_f <- merge(adm1geo_f, adm1geo@data, by.x="id",by.y="row.names")
adm1geo_f <-join (adm1geo_f, adm1geo@data, by="id")
adm1geo_f <-join(x=adm1geo_f, y=data.asylum, by="name")


### basic map
rm(maplevel1)

#centroids of adm1geo_f polygons for position of labels
distcenters <- aggregate(cbind(long, lat) ~ name, data=adm1geo_f, 
                    FUN=function(x)mean(range(x))) 

#distcenters <- ddply(adm1geo_f, .(name), summarize, clat = mean(lat), clong = mean(long))

maplevel1 <-  ggplot(adm1geo_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data = adm1geo_f, aes(x = long, y = lat, group = group), alpha = 0.3) +
  geom_path(data = adm1geo_f, aes(x = long, y = lat, group = group), color="gray90", size=0.25)+
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  # geom_text(data=distcenters, aes(clong, clat, label = name), size=1) +
  geom_text(data=distcenters, aes(long, lat, label = name), size=1) +
  ggtitle("Governorates")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),legend.position = "none")

ggsave("out/map/maplevel1.png", maplevel1, width=4, height=3,units="in", dpi=300)


#class intervalls for #Individuals --> #refugees
intervalleref <- classIntervals(data.asylum$Num_Inds, n = 5, style = "jenks")
#use the breaks from above to decide the break points
adm1geo_f$ref.breaks <- cut(adm1geo_f$Num_Inds, breaks = c(intervalleref$brks), dig.lab = 2)
levels(adm1geo_f$ref.breaks) <- rev(levels(adm1geo_f$ref.breaks))


#choropleth map: governorates shaded by no. of refugees (classes from above)
rm(maplevel1_reff)
maplevel1_reff <-  ggplot(adm1geo_f, aes(x = long, y = lat, group = group, fill=ref.breaks)) + 
  coord_equal() +
  geom_polygon(data = adm1geo_f, aes(fill=ref.breaks), alpha = 0.3) +
  geom_path(data = adm1geo_f, aes(x = long, y = lat, group = group), color="gray90", size=0.25) +
  scale_fill_brewer(palette="YlOrBr", name="Refugees") + 


  theme_nothing(legend=TRUE)+
  
  ggtitle("Refugees #") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),legend.position = "right")

ggsave("out/map/maplevel1_reff.png", maplevel1_reff, width=4, height=3,units="in", dpi=300)



# maplevel1_reff <- maplevel1_reff +
#   geom_point(data=distcenters, aes(size = total, long, lat, group = group), alpha = 0.3, color="coral1")+  #add proportional symbol at centroids
#   scale_size(name="Total IDP Ind.",labels=format_si())
# ggsave("out/map/maplevel1_total_idp.png", maplevel1_reff, width=4, height=3,units="in", dpi=300)
# 
# maplevel1_reff <- maplevel1_reff +
#   geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group, hjust=0.5, vjust=-0.5), size = 2.5)+ #add labels at centroids
#   ggtitle("Ratio IDPs / Refugees")      
# 
# ggsave("out/map/maplevel1_reff.png", maplevel1_reff, width=4, height=3,units="in", dpi=300)


############################################################
### Same for level 2

### Now create table for mapping purpose
names(data)
str(data)


adm2.data.asylum <- aggregate(cbind(Num_Inds  ) ~ coal2 + CountryAsylum, data = data, FUN = sum, na.rm = TRUE)


adm2.data.asylum.Case.size <- dcast((melt(data, id=c(15), measure=c(45))), coal2 ~ value )
adm2.data.asylum.CountryOriginCategory <- dcast((melt(data, id=c(15), measure=c(68))), coal2 ~ value )
adm2.data.asylum.YearArrivalCategory <- dcast((melt(data, id=c(15), measure=c(67))), coal2 ~ value )
adm2.data.asylum.edu_highestcat <- dcast((melt(data, id=c(15), measure=c(71))), coal2 ~ value )
adm2.data.asylum.occupationcat <- dcast((melt(data, id=c(15), measure=c(77))), coal2 ~ value )
adm2.data.asylum.dem_marriage <- dcast((melt(data, id=c(15), measure=c(31))), coal2 ~ value )
adm2.data.asylum.dem_sex <- dcast((melt(data, id=c(15), measure=c(35))), coal2 ~ value )
adm2.data.asylum.dem_ethn <- dcast((melt(data, id=c(15), measure=c(36))), coal2 ~ value )
adm2.data.asylum.dem_religion <- dcast((melt(data, id=c(15), measure=c(37))), coal2 ~ value )


colnames(adm2.data.asylum)[1] <- "adm2name"

# adm2geo <- readOGR("geo/geojson/JOR/ADM2.geojson", "OGRGeoJSON")

#read geojson from github repository 'p-codes'
url <- "https://raw.githubusercontent.com/unhcr-mena/p-codes/gh-pages/geojson/JOR/ADM2.geojson"
download.file( url, destfile="geo/geojson/JOR/ADM2.geojson" )
adm2geo <- geojson_read( "geo/geojson/JOR/ADM2.geojson", method="local", what="sp" )
#plot(adm2geo)

# Fortify them
adm2geo@data$id = rownames(adm2geo@data)

rm(adm2geo_f)
adm2geo_f <- fortify(adm2geo, region="id")

adm2geo_f <-join (adm2geo_f, adm2geo@data, by="id")
adm2geo_f <-join(x=adm2geo_f, y=adm2.data.asylum, by="adm2name")

### basic map
rm(maplevel2)

#centroids of adm2geo_f polygons for position of labels
distcenters2 <- aggregate(cbind(long, lat) ~ adm2name, data=adm2geo_f, 
                         FUN=function(x)mean(range(x))) 

maplevel2 <-  ggplot(adm2geo_f, aes(long, lat)) + coord_equal()+
  geom_polygon(data = adm2geo_f, aes(x = long, y = lat, group = group), alpha = 0.3) +
  geom_path(data = adm2geo_f, aes(x = long, y = lat, group = group), color="white", size=0.25)+
  # geom_text(aes(label = short, x = Longitude_c, y = Latitude_c, group = group)) + #add labels at centroids
  # geom_text(data=distcenters2, aes(clong, clat, label = name), size=1) +
  # geom_text(data=distcenters2, aes(long, lat, label = adm2name), size=1) +
  
  ggtitle("Districts")+
  theme_tufte(base_family="Helvetica")+
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),legend.position = "none")

ggsave("out/map/maplevel2.png", maplevel2, width=4, height=3,units="in", dpi=300)




#choropleth map: governorates shaded by no. of refugees (classes from above)

#class intervalls for #Individuals --> #refugees
adm2.intervalleref <- classIntervals(adm2.data.asylum$Num_Inds, n = 5, style = "jenks")
#use the breaks from above to decide the break points
adm2geo_f$ref.breaks <- cut(adm2geo_f$Num_Inds, breaks = c(adm2.intervalleref$brks), dig.lab = 2)
levels(adm2geo_f$ref.breaks) <- rev(levels(adm2geo_f$ref.breaks))

#choropleth map
rm(maplevel2_reff)
maplevel2_reff <-  ggplot(adm2geo_f, aes(x = long, y = lat, group = group, fill=ref.breaks)) + 
  coord_equal() +
  geom_polygon(data = adm2geo_f, aes(fill=ref.breaks), alpha = 0.3) +
  geom_path(data = adm2geo_f, aes(x = long, y = lat, group = group), color="gray90", size=0.25) +
  scale_fill_brewer(palette="YlOrBr", name="Refugees") + 
  
  theme_nothing(legend=TRUE)+
  
  ggtitle("Refugees #") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title=element_text(face="bold", size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_blank(),axis.ticks = element_blank(),
        axis.text.y = element_blank(),legend.position = "none")

ggsave("out/map/maplevel2_reff.png", maplevel2_reff, width=4, height=3,units="in", dpi=300)



###############################################################
### Getting a map background
##############################################################
## Jordan bounding box NE 33.374828, 39.301128 ; SW 29.184090, 34.960232
#bounding <- bbox(datsp)
jordanbox <- c(34.8, 29.1, 39.5, 33.5) 


### Bounding can also be done in the ggplot2 object through the following

#  scale_x_continuous(limits = c(34.8, 39.5)) +
#  scale_y_continuous(limits = c(29.1, 33.5)) +


## Center on north 32.2607107,36.5205146,9


require("ggmap")
googleterrain <- get_map(location = c(lon =37.22, lat = 31.32),
                         color = "color", source = "google",maptype = "terrain", zoom = 7)
googleeterrain <- ggmap(googleterrain)

googleroad <- get_map(location = jordanbox,
                      color = "bw", source = "google",maptype = "road", zoom = 7)
googleeroad <- ggmap(googleroad)

northroad <- get_map(location = c(lon =36.12, lat = 32.26),
                     color = "bw", source = "google",maptype = "road", zoom = 9)
northeroad <- ggmap(northroad)

## Black & White cropped background map
stamen <- get_map(location = jordanbox,
                  crop = T, zoom = 8, color = "color", 
                  source = "stamen", maptype = "toner")
stamenet <- ggmap(stamen)


## Does not work
#osm <- get_openstreetmap(location = jordanbox,  source = "osm",  format = "png", filename = "ggmapTemp.png")
#osmback <- ggmap(osm)

#mapbox <- getmapbox_map(center = c(lng = 37.22, lat = 31.32),  mapbox = "unhcr.map-ohec27wu", zoom = 8, size = 640, filename = "map.png")
#mapboxback <- map_png(mapbox)

#leaflet basemap
install.packages("leaflet")
library(leaflet)

topoData <- readLines("json/us-10m.json") %>% paste(collapse = "\n")

m <- leaflet() 
m <- setView(m, lng = 36.5, lat = 31.5, zoom = 7) #extent for jordan
m <- addTiles(m)
m

###############################################################
### Simple Map Overlay in ggplot2
##############################################################
rm(map_obs)
map_obs <- googleeroad
map_obs <- map_obs + 
  geom_point(aes(x = long, y = lat), data=hve, color="#006ec7", # UN Blue...
             size=1, alpha = 1/10)+
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Home Visit") +
  scale_x_continuous(limits = c(34.8, 39.5)) +
  scale_y_continuous(limits = c(29.1, 33.5)) +
  theme_bw()
ggsave("out/map_obs.png", map_obs, width=8, height=6,units="in", dpi=300)
rm(map_obs)
