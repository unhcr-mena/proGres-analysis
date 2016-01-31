## preparing a few graphs



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
