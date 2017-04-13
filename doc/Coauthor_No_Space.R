
setwd("~/Desktop/GR5243/Spr2017-proj4-team12/output")

temp = list.files(pattern="*.csv")

for (i in 1:length(temp)){
  assign(temp[i], read.csv(temp[i]))
  coauthor_no_space(temp[i])
}

coauthor_no_space <- function(data){

  new_coauthor <- gsub(" ", "", data$Coauthor)
  data$Coauthor <- gsub(pattern = "[[:punct:]]", " ", new_coauthor)
  
  return(data)
}



