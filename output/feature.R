#input is dataframe
get_feature<-function(data, condition="combine"){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(text2vec, dplyr, qlcMatrix, kernlab, knitr)
  if (condition=="coauthor"){
    it_train <- itoken(data$Coauthor,
                       preprocessor = tolower,
                       tokenizer = word_tokenizer,
                       ids = data$PaperID,
                       # turn off progressbar because it won't look nice in rmd
                       progressbar = FALSE)
  }else if(condition=="paper"){
    it_train <- itoken(data$Paper,
                       preprocessor = tolower,
                       tokenizer = word_tokenizer,
                       ids = data$PaperID,
                       # turn off progressbar because it won't look nice in rmd
                       progressbar = FALSE)
  } else if(condition=="journal"){
    it_train <- itoken(data$Journal,
                       preprocessor = tolower,
                       tokenizer = word_tokenizer,
                       ids = data$PaperID,
                       # turn off progressbar because it won't look nice in rmd
                       progressbar = FALSE)
  } else{
    combine<-paste(data$Paper, data$Coauthor, data$Journal)
    it_train <- itoken(combine,
                       preprocessor = tolower,
                       tokenizer = word_tokenizer,
                       ids = data$PaperID,
                       # turn off progressbar because it won't look nice in rmd
                       progressbar = FALSE)
  }
  
  vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                     "at", "of", "above", "under"))
  #vocab
  vectorizer <- vocab_vectorizer(vocab)
  dtm_train <- create_dtm(it_train, vectorizer)
  tfidf <- TfIdf$new()
  dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
  docsdissim <- cosSparse(t(dtm_train_tfidf))
  y<-as.data.frame(as.matrix(docsdissim))
  y$label<-as.factor(data$AuthorID)
  return(y)
  #result_sclust <- specc(as.matrix(dtm_train_tfidf),
  # centers=length(unique(data$AuthorID)))
  #return(result_sclust)
  
}