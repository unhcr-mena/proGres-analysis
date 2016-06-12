####################################################################################################
### This scripts generate country report one by one based on the report template: 6-Report-Loop.Rmd
#####################################################################################################

## load all packages
source("code/0-packages.R")

library(knitr)
library(rmarkdown)

#### load restructured data
data1 <- read.csv("data/progrescase2.csv")
#data1 <- data

# levels(data$CountryAsylum)
#3ctrcode <- "JOR"
#ctrname <- "Jordan"

listasylum <- read.csv("data/listasylum.csv")

nindic <- nrow(listasylum)
for(i in 1:nindic)
  {
ctrcode  <- as.character(listasylum[ i , 1]) 
ctrname <- as.character(listasylum[ i , 2]) 

render("code/6-Report-Loop.Rmd", pdf_document())

#' ---
#' output:
#'   pdf_document:
#'     fig_caption: yes
#'     number_sections: yes
#'     toc: yes
#'     toc_depth: 3
#' ---

render("code/6-Report-Loop.Rmd", pdf_document(), output_options=self_contained)

#render("code/6-Report-Loop.Rmd", pdf_document(),
#                                output_options=list(pdf_document = 
#                                                      list(fig_caption= "yes",
#                                                           number_sections= "yes",
#                                                           toc= "yes",
#                                                           toc_depth= "3")))
## , output_options=self_contained
## , output_options=( fig_caption: yes, number_sections: yes, toc: yes, toc_depth: 3)
## fig_caption: yes, number_sections: yes, toc: yes, toc_depth: 3


file.rename("code/6-Report-Loop.pdf", paste0("out/report/",ctrname,"_Data_Mining_Report.pdf"))
}
