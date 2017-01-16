
source("code/0-packages.R")

## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 
#data <-progres.spneedevent
data <- read.csv("data/progrescase2.csv")


#####################################################################################
### Let's try to generate chart for all variables
#####################################################################################

#print(strtable(data, factor.values=as.integer), na.print='')

data.str <- strtable(data, factor.values=as.integer)

#################################################

## We will use barplot for select_one variable

## list of field who have multiple predefined values

#levels(as.factor(data.str$class))


## extracting unique choice questions
data.class2graph  <- data.str[(data.str$class=="Factor w/ 3 levels" | data.str$class=="Factor w/ 4 levels"| data.str$class=="Factor w/ 5 levels"| 
                                 data.str$class=="Factor w/ 6 levels"|  data.str$class=="Factor w/ 7 levels"|  data.str$class=="Factor w/ 8 levels"|
                                 data.str$class=="Factor w/ 9 levels"|  data.str$class=="Factor w/ 12 levels"|  data.str$class=="Factor w/ 13 levels"|
                                 data.str$class=="Factor w/ 14 levels"),]

class2graph <- data.class2graph$variable
data.single <- data[class2graph]

#levels(data.single$Family.Size)

data.single$total <- 1
#data.single$total[data.single$Family.Size=="1"] <- 1 
#data.single$total[data.single$Family.Size=="1 - 3"] <- 3 
#data.single$total[data.single$Family.Size=="4 - 6"] <- 5 
#data.single$total[data.single$Family.Size=="7 and above"] <- 7 

names(data.single)

## Now we can start plotting in a loop
# the key is at the line here
# p + aes_string(x = names(mydata)[i])
# Use aes_string instead of aes, so that when you look at summary(ggplot_obj), 
# the mapping for x-values that are changing will be the actual string and not a variable i.

p <- ggplot(data.frame(data.single), aes(y=data.single$total)) + ylab("# of Responses") + scale_y_continuous(labels=format_si())
for (i in 1:41 ) {
  rm(variablename)
  variablename <- names(data.single)[i]
  rm(plot)
  plot <- p + 
    aes_string(x = names(data.single)[i]) +
    # aes_string(x = reorder( names(data.single)[i], names(data.single)[i], function(x)-length(x)))  +
    xlab(colnames(data.single[i])) + 
    geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8")  +
    guides(fill=FALSE) + 
    # coord_flip()+
    xlab("") + 
    coord_flip() + 
    ggtitle(variablename)
  assign(paste("plot", variablename, sep=""), plot)
  ggsave(filename=paste("out/Individual/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}

rm(data.single)



##### Histogram of dependency 

hist.dependency.or <-ggplot(data=data, aes(data$dependency)) + 
  #geom_histogram(breaks=seq(0, 1, by =.2), col="white", aes(fill=..count..))+ 
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryOriginCategory, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Dependency Ratio (Youth + Old) - Origin")
ggsave("out/histdependencyorigin .png", hist.dependency.or , width=12, height=12,units="in", dpi=300)

hist.dependency.as <-ggplot(data=data, aes(data$dependency)) + 
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryAsylum, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Dependency Ratio (Youth + Old) - Asylum")
ggsave("out/histdependencyAsylum .png", hist.dependency.as , width=12, height=12,units="in", dpi=300)

# elederndependency

hist.elederndependency.or <-ggplot(data=data, aes(data$elederndependency)) +  
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryOriginCategory, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Eldern Dependency Ratio - Origin")
ggsave("out/histelederndependencyorigin .png", hist.elederndependency.or , width=12, height=12,units="in", dpi=300)

hist.elederndependency.as <-ggplot(data=data, aes(data$elederndependency)) + 
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryAsylum, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Eldern Dependency Ratio - Asylum")
ggsave("out/histelederndependencyAsylum .png", hist.elederndependency.as , width=12, height=12,units="in", dpi=300)

# youthdependency


hist.youthdependency.or <-ggplot(data=data, aes(data$youthdependency)) +  
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryOriginCategory, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Youth Dependency Ratio - Origin)")
ggsave("out/histyouthdependencyorigin .png", hist.youthdependency.or , width=12, height=12,units="in", dpi=300)

hist.youthdependency.as <-ggplot(data=data, aes(data$youthdependency)) + 
  geom_histogram(aes(y =..density..),
                 breaks=seq(0, 5, by = 2),
                 col="red",
                 fill="green",
                 alpha = .2) +
  geom_density(col=2) +
  scale_y_continuous(labels=format_si()) + 
  facet_wrap( ~ CountryAsylum, ncol=4) +
  xlab("Dependency") + 
  ggtitle("Youth Dependency Ratio - Asylum")
ggsave("out/histyouthdependencyAsylum .png", hist.youthdependency.as , width=12, height=12,units="in", dpi=300)

