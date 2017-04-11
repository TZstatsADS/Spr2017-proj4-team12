Sys.setlocale("LC_ALL", "C")

mergesomerows  <- function(cluster.id, mergevector, label_hat){
  paste(mergevector[cluster.id == label_hat], collapse = " ")
}


cluster_merge <- function(raw_data, label_hat){
  unique_label_hat <- unique(label_hat)
  df <- data.frame(Cluster = integer(), Coauthor = character(), Paper = character(), 
                   Journal = character(), stringsAsFactors = FALSE)
  
  Coauthor <- sapply(unique_label_hat, 
               mergesomerows, 
               mergevector = raw_data$Coauthor, label_hat = label_hat)
  
  Paper <- sapply(unique_label_hat, 
                     mergesomerows, 
                     mergevector = raw_data$Paper, label_hat = label_hat)
  
  Journal <- sapply(unique_label_hat, 
                     mergesomerows, 
                     mergevector = raw_data$Journal, label_hat = label_hat)
  merged_matrix <- cbind(Coauthor, Paper, Journal)
  return(list(Clustered = merged_matrix, matrix_label = unique_label_hat))
}

## data = cluster_merge(AKumar, rep(1:4,61))
## cluster_merge(CChen, rep(1:3,267))
cosine_similarity <- function(data){
  
  if (!require("text2vec")) install.packages("text2vec")
  if (!require("qlcMatrix")) install.packages("qlcMatrix")
  library(text2vec)
  library(qlcMatrix)
  
  feature <- list()
  for (i in 1:3){
    it_train <- itoken(data$Clustered[,i],
                       preprocessor = tolower,
                       tokenizer = word_tokenizer,
                       # ids = data$PaperID,
                       # turn off progressbar because it won't look nice in rmd
                       progressbar = FALSE)
    vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                       "at", "of", "above", "under"))
    #vocab
    vectorizer <- vocab_vectorizer(vocab)
    dtm_train <- create_dtm(it_train, vectorizer)
    tfidf <- TfIdf$new()
    dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
    ftfidf<-as.matrix(dtm_train_tfidf)
    feature[[i]] <- as.matrix(cosSparse(t(ftfidf)))
  }
  # feature$label<-factor(data$AuthorID)
  return(list(CLUSTER.ID = data$matrix_label, MATRIX = feature))
}

AKumar <- read.csv("AKumar.csv", as.is = T)
CChen <- read.csv("CChen.csv", as.is = T)
# # # "as.is" is important here.
test <- cosine_similarity(cluster_merge(CChen, rep(1:4,61)))





# feature_paper5_coauthor <- function(data){
#   if (!require("text2vec")) install.packages("text2vec")
#   library(text2vec)
#   it_train <- itoken(data$Coauthor,
#                      preprocessor = tolower,
#                      tokenizer = word_tokenizer,
#                      ids = data$PaperID,
#                      # turn off progressbar because it won't look nice in rmd
#                      progressbar = FALSE)
#   vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
#                                                      "at", "of", "above", "under"))
#   #vocab
#   vectorizer <- vocab_vectorizer(vocab)
#   dtm_train <- create_dtm(it_train, vectorizer)
#   tfidf <- TfIdf$new()
#   dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
#   feature<-as.data.frame(as.matrix(dtm_train_tfidf))
#   feature$label<-factor(data$AuthorID)
#   return(feature)
# }


# feature_paper5_Paper.Journal <- function(data){
#   if (!require("text2vec")) install.packages("text2vec")
#   library(text2vec)
#   
#   data1 <- rbind(as.matrix(data$Paper), as.matrix(data$Journal))
#   data2 <- rbind(as.matrix(data$AuthorID), as.matrix(data$AuthorID))
#   data3 <- rbind(as.matrix(data$PaperID), as.matrix(data$PaperID))
#   data <- data.frame(Paper_Journal = as.character(data1), AuthorID = as.integer(data2),
#              PaperID = as.integer(data3), stringsAsFactors = F)
#   
#   it_train <- itoken(data$Paper_Journal,
#                      preprocessor = tolower,
#                      tokenizer = word_tokenizer,
#                      ids = data$PaperID,
#                      # turn off progressbar because it won't look nice in rmd
#                      progressbar = FALSE)
#   vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
#                                                      "at", "of", "above", "under"))
#   #vocab
#   vectorizer <- vocab_vectorizer(vocab)
#   dtm_train <- create_dtm(it_train, vectorizer)
#   tfidf <- TfIdf$new()
#   dtm_train_tfidf <- fit_transform(dtm_train, tfidf)
#   feature<-as.data.frame(as.matrix(dtm_train_tfidf))
#   feature$label<-factor(data$AuthorID)
#   return(feature)
# }

# AKumar.feature.new <- feature_paper5_Paper.Journal(AKumar)



