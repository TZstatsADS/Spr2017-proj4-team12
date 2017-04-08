#input is dataframe
#output is dataframe
#install.packages("text2vec")
#library(text2vec)
get_feature_paper<-function(data){
  it_train <- itoken(data$Paper,
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
  feature<-as.data.frame(as.matrix(dtm_train_tfidf ))
  feature$label<-factor(data$AuthorID)
  return(feature )
  
}

get_feature_coauthor<-function(data){
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
  feature<-as.data.frame(as.matrix(dtm_train_tfidf ))
  feature$label<-factor(data$AuthorID)
  return(feature )
  
}

get_feature_journal<-function(data){
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
  feature<-as.data.frame(as.matrix(dtm_train_tfidf ))
  feature$label<-factor(data$AuthorID)
  return(feature )
  
}

get_feature_combo1<-function(data){
  it_train <- itoken(data$Combo1,
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
  feature<-as.data.frame(as.matrix(dtm_train_tfidf ))
  feature$label<-factor(data$AuthorID)
  return(feature )
  
}