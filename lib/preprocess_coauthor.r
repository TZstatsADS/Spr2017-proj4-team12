
preprocess_coauthor <- function(data){
  new_coauthor <- gsub(" ", "", data$Coauthor)
  data$Coauthor <- gsub(pattern = "[[:punct:]]", " ", new_coauthor)
  return(data)
}