feature_paper5_coauthor <- function(data){
  if (!require("text2vec")) install.packages("text2vec")
  library(text2vec)
  it_train <- itoken(data$Coauthor,
                     preprocessor = tolower,
                     tokenizer = word_tokenizer,
                     ids = data$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under"))
  #vocab
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  feature<-as.data.frame(as.matrix(dtm_train_tfidf))
  feature$label<-factor(data$AuthorID)
  return(feature)
}

# AKumar <- read.csv("AKumar.csv", as.is = T) #"as.is" is important here.
# AKumar.feature <- feature_paper5_coauthor(AKumar)

feature_paper5_Paper.Journal <- function(data){
  if (!require("text2vec")) install.packages("text2vec")
  library(text2vec)
  
  data1 <- rbind(as.matrix(data$Paper), as.matrix(data$Journal))
  data2 <- rbind(as.matrix(data$AuthorID), as.matrix(data$AuthorID))
  data3 <- rbind(as.matrix(data$PaperID), as.matrix(data$PaperID))
  data <- data.frame(Paper_Journal = as.character(data1), AuthorID = as.integer(data2),
             PaperID = as.integer(data3), stringsAsFactors = F)
  
  it_train <- itoken(data$Paper_Journal,
                     preprocessor = tolower,
                     tokenizer = word_tokenizer,
                     ids = data$PaperID,
                     # turn off progressbar because it won't look nice in rmd
                     progressbar = FALSE)
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under"))
  #vocab
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  feature<-as.data.frame(as.matrix(dtm_train_tfidf))
  feature$label<-factor(data$AuthorID)
  return(feature)
}

# AKumar.feature.new <- feature_paper5_Paper.Journal(AKumar)
