## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")

data <- progres.case.sp.dep.rst

#####################################################################################
### Let's try to generate chart for all variables
#####################################################################################

#print(strtable(data, factor.values=as.integer), na.print='')

data.str <- strtable(data, factor.values=as.integer)

#################################################

## We will use barplot for select_one variable

## list of field who have multiple predefined values

levels(as.factor(data.str$class))


## extracting unique choice questions
data.class2graph  <- data.str[(data.str$class=="Factor w/ 3 levels" | data.str$class=="Factor w/ 4 levels"| data.str$class=="Factor w/ 5 levels"| 
                                 data.str$class=="Factor w/ 6 levels"|  data.str$class=="Factor w/ 7 levels"|  data.str$class=="Factor w/ 8 levels"|
                                 data.str$class=="Factor w/ 9 levels"|  data.str$class=="Factor w/ 12 levels"|  data.str$class=="Factor w/ 13 levels"|
                                 data.str$class=="Factor w/ 14 levels"),]

class2graph <- data.class2graph$variable
data.single <- data[class2graph]

levels(data.single$Family.Size)

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
  ggsave(filename=paste("out/plot_",i,variablename,".png",sep=""), plot=plot, width=8, height=6,units="in", dpi=300)
}



########################################################################################
## preparing a few additional graphs
########################################################################################


#################################################
#### Box plot to display arrival 
box.season <- ggplot(data[data$CountryOrigin == "SYR",], aes(x=season, y=Num_Inds, fill=season) ) + 
  geom_boxplot() 
ggsave("out/boxseason.png", box.season, width=8, height=6,units="in", dpi=300)

box.month <- ggplot(data[data$CountryOrigin == "SYR",], aes(x=Montharrival, y=Num_Inds, fill=Montharrival) ) + 
  geom_boxplot() 
ggsave("out/boxmonth.png", box.month, width=8, height=6,units="in", dpi=300)

box.year <-ggplot(data[data$YearArrival > 2011,], aes(x=YearArrival, y=Num_Inds, fill=YearArrival) ) + 
  geom_boxplot() 
ggsave("out/boxyear.png", box.year, width=8, height=6,units="in", dpi=300)






#################################################
#### Bar graph to show repartition for categories

bar.season <- ggplot(data=data[data$CountryOrigin == "SYR",], aes(x=season , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("Season of Arrival for Syrians") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("Arrival per season")
ggsave("out/barseason.png", bar.season, width=8, height=6,units="in", dpi=300)



#### Bar graph to show repartition for categories

data$occupationcat <- reorder(data$occupationcat, sum(data$Num_Inds))
data$occupationcat <- factor(data$occupationcat, levels = data$occupationcat[order(sum(data$Num_Inds)])

bar.occupationcat <- ggplot( #order_by(occupationcat, ~ Num_Inds, data, sum),
                             data=data[data$CountryOrigin == "SYR",], 
                            aes(x=occupationcat , y=Num_Inds #, fill=edu_highest_t
                                )) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=occupationcat), vjust=0) +
  facet_wrap( ~ edu_highest_t, ncol=3) +
  guides(fill=FALSE) + 
  xlab("occupation Category of principal applicant") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip() + 
  ggtitle("Number of Ind per occupation Category of principal applicant for Syrians")

ggsave("out/occupationcat.png", bar.occupationcat, width=12, height=12,units="in", dpi=300)


bar.edu_highest_t <- ggplot(data=data[data$CountryOrigin == "SYR",], 
  aes(x=edu_highest_t , y=Num_Inds, fill=occupationcat )) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  facet_wrap( ~ CountryOriginCategory, ncol=3) +
  guides(fill=FALSE) + 
  xlab("Education level of principal applicant") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip() + 
  ggtitle("Number of Ind per Education Level of principal applicant for Syrians")
ggsave("out/edu_highest_t.png", bar.edu_highest_t, width=8, height=6,units="in", dpi=300) 


bar.YearArrival <- ggplot(data=data[data$YearArrival > 2011,], aes(x=YearArrival , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  facet_wrap( ~ CountryAsylum, ncol=3) +
  xlab("year of Arrival") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("Arrival per Year - after 2011")
ggsave("out/YearArrival.png", bar.YearArrival, width=12, height=12,units="in", dpi=300)
