
### Author: Kai Chen
### Time: April 8, 2017

source("../lib/feature_paper5.R")
AKumar <- read.csv("../output/AKumar.csv", as.is = T)
AKumar.feature <- feature_paper5_coauthor(AKumar)

# dim(AKumar.feature) 

find_max_in_the_matrix <- function(A, epi = 0.0001){
  dim_A <- dim(A)[1]
  A[A > 1-epi] = 0
  n <- which.max(A)[1]
  ncols <- ceiling(n/(dim(A)[1]))
  return(list(LOCATION = c(ncols, n - (ncols-1) * dim_A[1]), VALUE = max(A)))
}
#find_max_in_the_matrix(A), correct

# Compute Euclidean Distance, correct
compute_distance <- function(x,y){
  if (length(x) == length(y)){
    s <- (x-y)^2
    return(sum(s)^0.5)
  }
  else
    return(F)
}

# Compute Score for cluster
compute_cluster_score <- function(features, paras, which_label, label){
  
  ##
  X1 <- colMeans(matrix(features[which_label == label,], ncol = n_features))
  return(t(X1) %*% paras)
}

# Compute Score for all 
compute_all_score <- function(features, paras, label){
  ## X is subset of data, whose column is the values of features
  unique_label <- unique(label)
  results <- sapply(unique_label, compute_cluster_score, 
                    features = features, paras = paras, label = label)
  return(sum(results))
}

#compute_all_score(A, c(1,1), c(1,1))

### Whether Error Happens
no_error <- function(T_label, Test_label){
  # Return T if no error occurs
    number <- 1:length(T_label)
    unique_label <- unique(Test_label)
    n_label <- length(unique_label)
    for (i in unique_label){
      Test_cluster <- (Test_label == i)
      if (length(T_label[Test_cluster]) > 1 & var(T_label[Test_cluster]) != 0){
        return(F)
      }
    }  
  return(T)
}
# Feel free to test my code ~ 
# no_error(c(1,2,1,2,2),c(1,2,3,3,5))


### When we have the label, we need compute the features for each cluster
compute_cluster_feature <- function(features, label, interest_label){
  ## dui mou yi ge cluster qiu ping jun  
  n_features <- ncol(features)
  result_cluster <- colMeans(matrix(features[interest_label == label, ], ncol = n_features))
  return(result_cluster)
}

# merge, 3 matrices --> three features 
compute_3_feature <- function(MERGE, features3list){
  # find the correspoding entry in a certain position, which is the merge point MERGE. 
  sapply(features3list, function(matrix){matrix[MERGE[1], MERGE[2]]})
}

### Upate parameters 
update_para <- function(Recom_merge, Est_merge, paras, threebigmatrix){
  # Recom_merge, Est_merge are both in (i,j) form
  # feature3 is a list with 3 matrix
  paras_new <- paras + 
    compute_3_feature(Recom_merge, threebigmatrix) -
    compute_3_feature(Est_merge, threebigmatrix)
  paras_new <- paras_new / sum(paras_new)
  return(paras_new)
}

### Find an alternative one which is better
Give_you_better <- function(T_label, Test_label_previous){
  # We only need to give advice on which 2 clusters should be merged. 
  
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label){
    
    Test_cluster <- which(Test_label_previous == i) # which records are grouped by S
    T_cluster <- which(T_label == T_label[Test_cluster[1]])
    new_point_tf <- !(T_cluster %in% Test_cluster)
    if (sum(new_point_tf) > 0){
      merge <- c(i,(T_cluster[new_point_tf])[1])
      return(merge)
    }
  }  
  return(F)
}

Give_you_better2 <- function(T_label, Test_label_previous){
  # We only need to give advice on which 2 clusters should be merged. 
  
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label){
    
    Test_cluster <- which(Test_label_previous == i) # which records are grouped by S
    T_cluster <- which(T_label == T_label[Test_cluster[1]])
    new_point_tf <- !(T_cluster %in% Test_cluster)
    if (sum(new_point_tf) > 0){
      merge <- c(i,(T_cluster[new_point_tf])[1])
      return(merge)
    }
  }  
  return(F)
}
# Feel free to test my code ~ 
# Give_you_better(c(1,2,1,1,2),c(1,2,3,4,5))

change_label <- function(label,order){
  # Change label i to label j
  i <- order[1]
  j <- order[2]
  label[label == i] <- j
  return(label)
}



# Compute Each Step in Cluster
one_step_cluster <- function(pf5, paras, label){
  # paras should be a vector
  
 
  
  cluster_2id <- pf5[[1]] #cluster.id
  threebigmatrix <- pf5[[2]] # list of 3 Ms
  
  n_features <- length(paras)
  score_matrix <- 0
  for (sumi in 1:n_features){
    score_matrix <- score_matrix + paras[sumi] * threebigmatrix[[sumi]]
  }
  position <- find_max_in_the_matrix(score_matrix)$LOCATION # find our est_label of cluster
  recommended_cluster <- cluster_2id[position] 
  # a length-2 vector, indicating which 2 clusters should be merged.
  
  return(recommended_cluster)
  # label := current label, which can determine the dim of each feature
  # raw_data := record1, record2 ...
  # label = (1,2,1,2,2,3) 
  # features <- sapply(raw_data, chenyunfunction, merge = label) # we get three matrix
  # result_matrix <- 0
  # for (j in 1:length(paras)){
  #   result_matrix <- result_matrix + raw_data[[j]] * paras[j]
  # }
  # Choice <- find_max_in_the_matrix(result_matrix)$LOCATION
  # return(Choice)
  # unique_label <- unique(label)
  # a1 <- rep(unique_label, length(unique_label))
  # a2 <- rep(unique_label, each = length(unique_label))
  # delete <- (a1 >= a2)
  # a1 <- a1[!delete]
  # a2 <- a2[!delete]
  # order1 <- cbind(a1,a2)
  # matrix_labels <- apply(order1, 1, change_label, label = label)
  # # column: labels
  # scores <- apply(matrix_labels, 2, compute_all_score, features = features, paras = paras)
  # best_est_label <- change_label(label, order1[which.max(scores),])
#  return(best_est_label)
}



algorithm_paper_5 <- function(raw_data, True_labels, 
                              max.iter = 2000, epi = 0.0001, 
                              step.size = 0.1, show.history = F){
  
  # True_labels should be author.id
  # raw data := list of 3 matices
  n_obs <- nrow(raw_data)
  n_features <- ncol(raw_data) # xyz chinese: xuyao gai
  
  # Initial assignment
  paras <- rep(0, n_features)
  paras <- rbind(paras,rep(1/3, n_features))
  t <- 1
  
  # iteration
  while((t <= max.iter) & (compute_distance(paras[t+1,], paras[t,])> epi)){
    
    # initial assignment
    old_labels <- 1:n_obs
    for (i.ter in 1:n_obs){
      
      pf5 <- paper_feature5(raw_data, True_labels) # contributed by chenyun  
      ## list1 = cluster.id coresponding to each row of matrix, list2 = 3matrix
      
      # for each step, we merge only two clusters
      m_labels <- one_step_cluster(features, paras[t+1,], old_labels) #vector %in% R2
      
      new_labels <- change_label(label = True_labels, order = m_labels[1])
      
      if (!no_error(True_labels, new_labels)){ ## if error exists 
        
        # Find a better one
        better_labels <- Give_you_better2(True_labels, old_labels)
        
        # Update our paras 
        paras0 <- update_para(better_labels, m_labels, paras[t+1], pf5[[2]]) 
        paras <- rbind(paras, paras0)
        t <- t + 1
        break
      }
      # continue
      old_labels <- new_labels
    }
  }

  result <- list(best = paras[nrow(paras),], iter = t-1)
  return(result)
  
}

## let's test
A1 <- c(1,0,0,1,0,0,0)
A2 <- c(0,1,0,0,1,0,0)
A3 <- c(0,0,1,0,0,1,1)
True_labels <- c(1,2,3,1,2,3,3)
Feat <- cbind(A1,A2,A3)
algorithm_paper_5(Feat, True_labels, max.iter = 10000)

