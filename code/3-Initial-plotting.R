## preparing a few graphs

library(ggplot2)

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


## box plot to display 
ggplot(progres.case[progres.case$CountryOrigin == "SYR",], aes(x=season, y=Num_Inds, fill=season) ) + 
  geom_boxplot() 

ggplot(progres.case[progres.case$CountryOrigin == "SYR",], aes(x=Montharrival, y=Num_Inds, fill=Montharrival) ) + 
  geom_boxplot() 


#### Bar graph to show repartition for categories
bar.season <- ggplot(data=progres.case[progres.case$CountryOrigin == "SYR",], aes(x=season , y=Num_Inds)) + 
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

progres.case$occupationcat <- reorder(progres.case$occupationcat, sum(progres.case$Num_Inds))
#
progres.case$occupationcat <- factor(progres.case$occupationcat, levels = progres.case$occupationcat[order(sum(progres.case$Num_Inds)])

bar.occupationcat <- ggplot( #order_by(occupationcat, ~ Num_Inds, progres.case, sum),
                             data=progres.case[progres.case$CountryOrigin == "SYR",], 
                            aes(x=occupationcat , y=Num_Inds #, fill=edu_highest_t
                                )) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=occupationcat), vjust=0) +
  facet_wrap( ~ edu_highest_t, ncol=2) +
  guides(fill=FALSE) + 
  xlab("occupation Category of principal applicant") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip() + 
  ggtitle("Numnber of ind per occupation Category of principal applicant")
ggsave("out/occupationcat.png", bar.occupationcat, width=8, height=6,units="in", dpi=300)




bar.edu_highest_t <- ggplot(data=progres.case[progres.case$CountryOrigin == "SYR",], 
  aes(x=edu_highest_t , y=Num_Inds, fill=occupationcat )) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  #facet_wrap( ~ occupationcat, ncol=3) +
  guides(fill=FALSE) + 
  xlab("Education level of principal applicant") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip() + 
  ggtitle("Numnber of ind per Education Level of principal applicant")
ggsave("out/edu_highest_t.png", bar.edu_highest_t, width=8, height=6,units="in", dpi=300) 



ggplot(progres.case[progres.case$YearArrival > 2011,], aes(x=YearArrival, y=Num_Inds, fill=YearArrival) ) + 
  geom_boxplot() 

bar.YearArrival <- ggplot(data=progres.case[progres.case$YearArrival > 2011,], aes(x=YearArrival , y=Num_Inds)) + 
  geom_bar( stat="identity",fill="#2a87c8",colour="#2a87c8") +
  # geom_text(aes(label=variable), vjust=0) +
  guides(fill=FALSE) + 
  # coord_flip()+
  xlab("year of Arrival") + 
  ylab("# of Ind") +
  scale_y_continuous(labels=format_si())+
  coord_flip()+ 
  ggtitle("Arrival per Year - after 2011")
ggsave("out/YearArrival.png", bar.YearArrival, width=8, height=6,units="in", dpi=300)
