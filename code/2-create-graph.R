## Generation of a exploratory graphs

## Visual exploration of data will provide a quick overview 


#####################################################################################
### Let's try to generate chart for all variables
#####################################################################################

#print(strtable(data, factor.values=as.integer), na.print='')

data.str <- strtable(progres.case, factor.values=as.integer)

#################################################

## We will use barplot for select_one variable

## list of field who have multiple predefined values

levels(as.factor(data.str$class))


## extracting unique choice questions
data.class2graph  <- data.str[(data.str$class=="Factor w/ 3 levels" | data.str$class=="Factor w/ 4 levels"| data.str$class=="Factor w/ 5 levels"| 
                        data.str$class=="Factor w/ 6 levels"|  data.str$class=="Factor w/ 7 levels"),]

class2graph <- data.class2graph$variable
data.single <- progres.case[class2graph]

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




