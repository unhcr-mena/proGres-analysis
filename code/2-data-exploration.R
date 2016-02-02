
## Exploration of data will provide a quick overview 

#rm(progres.case.sp.dep.rst)
#progres.case.sp.dep.rst <- read.csv("data/progrescase2.csv")

data <- progres.case.sp.dep.rst

### Exploring first correlations

## looking at correlation between categorical variables

table(data$Family.unity, data$rst.Family.reunification)


chisq.test(data$function_surv, data$skillsurv_surv, correct=FALSE)
fisher.test(data$function_surv, data$skillsurv_surv)