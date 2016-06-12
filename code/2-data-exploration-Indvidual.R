
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




#########################################################################

###

data.asylum.Case.size <- dcast((melt(data, id=c(22), measure=c(34))), coal1 ~ value )

data.asylum.Case.size2 <- melt(data, id=c(22), measure=c(34))

names <- levels(unique(as.factor(data.asylum.Case.size2$value)))

plist <- list()
plist[]

for (i in 1:length(names)) {
  d <- subset(data.asylum.Case.size2, value == names[i])
  data.asylum.Case.size2$coal1 <- factor(data.asylum.Case.size2$coal1, levels=d[order(data.asylum.Case.size2$value),]$coal1)
  
  p1 <- ggplot(data.asylum.Case.size2, aes(x = coal1, #y = value,
                                           fill = coal1, width=0.75)) + 
    labs(y = "Admin level 1 Asylum [ % ]", x = NULL, fill = NULL) +
    geom_bar(stat = "identity") +
    facet_wrap(~coal1) +
    scale_y_continuous(limits=c(0, 100)) +
    coord_flip() +
    guides(fill=FALSE) +
    theme_bw() + theme( strip.background  = element_blank(),
                        panel.grid.major = element_line(colour = "grey80"),
                        panel.border = element_blank(),
                        axis.ticks = element_line(size = 0),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.y = element_blank() ) +
    theme(legend.position="bottom") +
    scale_fill_brewer(palette="Set2")
  
  
  plist[[names[i]]] = p1
}   

do.call("grid.arrange", c(plist, ncol=4))
        