
### Author: Kai Chen
### Time: April 8, 2017


# Compute Euclidean Distance
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
  ## X is subset of data, whose column is the values of features
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

# For all, We just average on each cluster's features
compute_all_feature <- function(features, label){
  unique_label <- unique(label)
  each_cluster <- sapply(unique_label, compute_cluster_feature, features = features, label = label)
  mean_cluster <- rowMeans(each_cluster)
  return(mean_cluster)
}

### Upate parameters 
update_para <- function(Recom_label, Est_label, paras, features){
  # Recom, Estimate are features of recommended partition and estimated model partition
  paras_new <- paras + 
    compute_all_feature(features, Recom_label) -
    compute_all_feature(features, Est_label)
  # Here I guess the difference of two features
  paras_new <- paras_new / sum(paras_new)
  return(paras_new)
}

### Find an alternative one which is better
Give_you_better <- function(T_label, Test_label_previous){
  # Please give me the labels [before] making an error
  # My strategy is return one that's at least correct till now
  
  number <- 1:length(T_label)
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label){
    Test_cluster <- which(Test_label_previous == i) # which records are grouped by S
    T_cluster <- which(T_label == T_label[Test_cluster[1]])
    new_point_tf <- !(T_cluster %in% Test_cluster)
    if (sum(new_point_tf) > 0){
      new_point <- T_cluster[new_point_tf][1]
      Test_label_previous[new_point] <- i
      return(Test_label_previous)
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
one_step_cluster <- function(features, paras, label){
  # We choose an cluster and change the label to the existing label(i.e. Merge)
  unique_label <- unique(label)
  a1 <- rep(unique_label, length(unique_label))
  a2 <- rep(unique_label, each = length(unique_label))
  delete <- (a1 >= a2)
  a1 <- a1[!delete]
  a2 <- a2[!delete]
  order1 <- cbind(a1,a2)
  matrix_labels <- apply(order1, 1, change_label, label = label)
  # column: labels
  scores <- apply(matrix_labels, 2, compute_all_score, features = features, paras = paras)
  best_est_label <- change_label(label, order1[which.max(scores),])
  return(best_est_label)
}



algorithm1 <- function(features, True_labels, max.iter = 2000, epi = 0.000001, 
                       step.size = 0.1, show.history = F){
  
  n_obs <- nrow(features)
  n_features <- ncol(features)
  
  # Initial assignment
  paras <- rep(0, n_features)
  paras <- rbind(paras,rep(1, n_features))
  
  t <- 1
  while((t <= max.iter) & (compute_distance(paras[t+1,], paras[t,])> epi)){
    old_labels <- 1:n_obs
    for (i.ter in 1:n_obs){
      new_labels <- one_step_cluster(features, paras[t+1,], old_labels)
      if (!no_error(True_labels, new_labels)){ ## if error exists 
        # Find a better one
        better_labels <- Give_you_better(True_labels, old_labels)
        # Update our paras 
        paras0 <- update_para(better_labels, new_labels, paras[t+1], features) 
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
algorithm1(Feat, True_labels, max.iter = 10000)
