#input is dataframe
get_feature_journal<-function(data){
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(text2vec, dplyr, qlcMatrix, kernlab, knitr)
  it_train <- itoken(data$Journal,
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
  docsdissim <- cosSparse(t(dtm_train_tfidf))
  y<-as.data.frame(as.matrix(docsdissim))
  y$label<-as.factor(data$AuthorID)
  return(y)
  #result_sclust <- specc(as.matrix(dtm_train_tfidf),
  # centers=length(unique(data$AuthorID)))
  #return(result_sclust)
}