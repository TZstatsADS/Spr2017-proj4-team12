
preprocess_coauthor <- function(data){
  new_coauthor <- gsub(" ", "", data$Coauthor)
  data$Coauthor <- gsub(pattern = "[[:punct:]]", " ", new_coauthor)
  return(data)
}

# Function that maps a text to vector of 50 dimensions, returns a matrix
text_vector_function <- function(text, word_vector){
  text_vec_matrix <- c()
  for (each in unique(text)){
    if (sum(rownames(word_vector)==tolower(each))!=0) {
      index <- which(rownames(word_vector)==tolower(each))
      text_vec_matrix <- rbind(text_vec_matrix,word_vector[index, ])
    }
  }
  return(text_vec_matrix)
}



text_matrix_function <- function(data){
  text_output <- list()
  raw_data <- data$Clustered
  
  for (i in 1:3){
    it_train <- itoken(raw_data[,i],
                       preprocessor = tolower,
                       tokenizer = space_tokenizer,
                       progressbar = FALSE)
    vocab <- create_vocabulary(it_train, stopwords = c("a", "an", "the", "in", "on",
                                                       "at", "of", "above", "under"))
    
    vectorizer <- vocab_vectorizer(vocab, 
                                   # don't vectorize input
                                   grow_dtm = FALSE, 
                                   # use window of 5 for context words
                                   skip_grams_window = 1L)
    
    tcm <- create_tcm(it_train, vectorizer)
    glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = vocab, x_max = 10)
    suppressMessages(fit(tcm, glove, n_iter = 20))
    word_vectors <- glove$get_word_vectors()
  
    text_matrix_min <- matrix(NA, nrow = nrow(raw_data), ncol = 50)
    text_matrix_max <- matrix(NA, nrow = nrow(raw_data), ncol = 50) 
    text_matrix_mean <- matrix(NA, nrow = nrow(raw_data), ncol = 50) 
    for (j in (1:nrow(raw_data))){
      text_vector <- unlist(strsplit(raw_data[,i][j], split = " ")) # row j, column i, a vector of strings
      text_vectorized <- text_vector_function(text = text_vector, word_vector = word_vectors)
      text_matrix_min[j, ] <- apply(text_vectorized, 2, min)
      text_matrix_max[j, ] <- apply(text_vectorized, 2, max)
      text_matrix_mean[j, ] <- apply(text_vectorized, 2, mean)
    }
    
    text_output[[i]] <- list(MIN=text_matrix_min, MAX=text_matrix_max, MEAN=text_matrix_mean)
  }
  # feature$label<-factor(data$AuthorID)
  return(list(CLUSTER.ID = data$matrix_label, TEXTMATRIX = text_output))
}
