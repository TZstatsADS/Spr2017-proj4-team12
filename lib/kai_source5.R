
### Author: Kai Chen
### Time: April 8, 2017
setwd("~/Desktop/5243 ADS/Spr2017-proj4-team12/doc")
source("../lib/feature_paper5.R")
################### Function part ############################




find_max_in_the_matrix <- function(A){
  dim_A <- dim(A)[1]
  for (i in 1:dim_A){
    A[i,i] <- 0
  }
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

compute_converge <- function(x){
  n <- nrow(x)
  if(n < 5)
    return(100)
  new_record <- colMeans(matrix(x[1:(n-1),],nrow = n-1))
  old_record <- colMeans(matrix(x[1:n,],,nrow = n))
  distance <- compute_distance(new_record,old_record)
  return(distance)
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
    m <- 1
    result <- sapply(unique_label, function(label){
      Test_cluster <- (Test_label == label)
      T_label_sel <- T_label[Test_cluster]
      sum(T_label_sel!=T_label_sel[1])
    })
    if(sum(result)==0)
      return(T)
    else
      return(F)
}

# Feel free to test my code ~ 
# no_error(c(1,2,1,2,2),c(1,2,3,4,5))


### When we have the label, we need compute the features for each cluster
compute_cluster_feature <- function(features, label, interest_label){
  ## dui mou yi ge cluster qiu ping jun  
  n_features <- ncol(features)
  result_cluster <- colMeans(matrix(features[interest_label == label, ], ncol = n_features))
  return(result_cluster)
}

# merge, 3 matrices --> three features 
compute_3_feature <- function(MERGE, threebigmatrix){
  # input your cluster
  sapply(threebigmatrix, function(matrix){matrix[MERGE[1], MERGE[2]]})
}

### Upate parameters 
update_para <- function(Recom_merge, Est_merge, paras1, threebigmatrix, stepsize){
  # Recom_merge, Est_merge are both in (i,j) form
  # feature3 is a list with 3 matrix
  paras_new <- paras1 + 
    stepsize * (compute_3_feature(Recom_merge, threebigmatrix) -
    compute_3_feature(Est_merge, threebigmatrix))
  paras_new <- paras_new / sum(paras_new)
  return(paras_new)
}


Give_you_better <- function(T_label, Test_label_previous){
  # We only need to give advice on which 2 clusters should be merged. 
  # ni zhao chu zhe ge hou yao qu kan, na ji kuai dui ying ke yi he qi lai 

  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label[sample(1:n_label)]){
    
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
# Give_you_better2(c(1,2,1,1,2),c(1,2,3,4,5))


Give_you_better2 <- function(T_label, Test_label_previous, bigmatrices_id){
  # We only need to give advice on which 2 clusters should be merged. 
  # ni zhao chu zhe ge hou yao qu kan, na ji kuai dui ying ke yi he qi lai 
  
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label[sample(1:n_label)]){
    
    Test_cluster <- which(Test_label_previous == i) # which records are grouped by S
    T_cluster <- which(T_label == T_label[Test_cluster[1]])
    new_point_tf <- !(T_cluster %in% Test_cluster)
    if (sum(new_point_tf) > 0){
      merge <- c(i,(T_cluster[new_point_tf])[1])
      
      # find the cluster merge belong to
      x1 <- which(bigmatrices_id == Test_label_previous[merge[1]])
      x2 <- which(bigmatrices_id == Test_label_previous[merge[2]])
      return(c(x1,x2)) # cluster
    }
  }  
  return(F)
}


Give_you_better3 <- function(True_labels, Test_label_previous, bigmatrices_id){
  # We only need to give advice on which 2 clusters should be merged. 
  # ni zhao chu zhe ge hou yao qu kan, na ji kuai dui ying ke yi he qi lai 
  
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label[sample(1:n_label)]){
    
    previous_same_id_i <- which(Test_label_previous == i) # which records are grouped by S
    same_in_true_labels <- which(True_labels == True_labels[previous_same_id_i[1]])
    new_point_tf <- (!(same_in_true_labels %in% previous_same_id_i))
    if (sum(new_point_tf) > 0){
      change_id <- same_in_true_labels[new_point_tf]
      merge_cluster <- Test_label_previous[change_id][1]
      ##(i, mer_cluster) 
      xi <- which(i == bigmatrices_id) ## position in the matrix
      yi <- which(merge_cluster == bigmatrices_id)
      return(c(xi, yi))
      # merge <- c(i,(T_cluster[new_point_tf])[1])
      # 
      # # find the cluster merge belong to
      # x1 <- which(bigmatrices_id == Test_label_previous[merge[1]])
      # x2 <- which(bigmatrices_id == Test_label_previous[merge[2]])
      # return(c(x1,x2)) # cluster
    }
  }  
  return(F)
}

# Give_you_better3(c(4,5,5,5,4,5),c(3,2,1,2,3,7),bigmatrices_id)

bigmatrices_id <- unique(c(3,2,1,2,3,7))

change_label <- function(label, CLUSTER.ID){
  # Change label i to label j
  i <- CLUSTER.ID[1]
  j <- CLUSTER.ID[2]
  label[label == i] <- j
  return(label)
}



# Compute Each Step in Cluster
one_step_cluster <- function(pf5, paras1, label){
  # paras1 should be a vector 
  ## wo men zhi shi fan hui le yi ge  zai ju zhen li mian de wei zhi !!!! qing zhuan huan!!!
  
  cluster_2id <- pf5[[1]] #cluster.id
  threebigmatrix <- pf5[[2]] # list of 3 Ms
  
  n_features <- length(paras1)
  score_matrix <- 0
  for (sumi in 1:n_features){
    score_matrix <- score_matrix + paras1[sumi] * threebigmatrix[[sumi]]
  }
  position <- find_max_in_the_matrix(score_matrix)$LOCATION # find our est_label of cluster
#  recommended_cluster <- cluster_2id[position] 
  # a length-2 vector, indicating which 2 clusters should be merged.
  # return the names of clusters
  return(position)
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

################### Function part ############################


algorithm_paper_5 <- function(raw_data, True_labels, 
                              max.iter = 2000, stepsize = 0.1,
                              epi = 0.03){
  
  # True_labels should be author.id
  # raw data := list of 3 matices
  n_obs <- nrow(raw_data)
  n_features <- ncol(raw_data) # xyz chinese: xuyao gai
  
  # Initial assignment
  paras <- rep(0, n_features)
  paras_t0 <- (paras)
  paras <- rbind(paras,rep(1/3, n_features))
  paras_t1 <- colMeans(paras)
  
  t <- 1
  
  # iteration
  while((t <= max.iter) & (compute_distance(paras_t0, paras_t1) > epi)){
    
    # initial assignment
    old_labels <- 1:n_obs
    
    for (i.ter in 1:n_obs){

      pf5 <- cosine_similarity(cluster_merge(raw_data, old_labels)) # contributed by chenyun  
      ## list1 = cluster.id coresponding to each row of matrix, list2 = 3matrix
      
      # for each step, we merge only two clusters
      m_position <- one_step_cluster(pf5, paras[t+1,], old_labels) #vector %in% R2
      m_labels <- pf5$CLUSTER.ID[m_position]
    #  sb<- rbind(sb, m_labels)
      new_labels <- change_label(label = old_labels, CLUSTER.ID = m_labels)
      
      cat('\ni.ter=', i.ter)
      cat('\n new_labels', new_labels)
      
      
      if (!no_error(True_labels, new_labels)){ ##  deny
        # Find a better [position] in the matrix
        better_labels <- Give_you_better3(True_labels, old_labels, pf5$CLUSTER.ID) # 19 10
        if (sum(better_labels)==0)
          return(paras[t+1,])
        # Update our paras 
        paras0 <- update_para(Recom_merge = better_labels, Est_merge = m_position, 
                              paras1 = paras[t+1,], threebigmatrix = pf5[[2]],
                              stepsize = stepsize) 
        paras_t0 <- colMeans(paras)
        paras <- rbind(paras, paras0)
        paras_t1 <- colMeans(paras)
        t <- t + 1
        cat("better: ", better_labels, ";ours: ",m_labels, "paras: ", paras[t+1,],"\n")
        break
      }
      old1 <- old_labels # backup
      # continue
      old_labels <- new_labels # accept
      
    }
  #  cat('\n paras:=',paras[t+1,])
  }

  result <- list(best = paras[nrow(paras),], iter = t-1)
  return(result)
}



## let's test


#AKumar <- read.csv("../output/AKumar.csv")
AKumar <- read.csv("../lib/AKumar_test.csv", as.is = T)

AKumar_raw <- data.frame(Coauthor = AKumar$Coauthor,Paper = AKumar$Paper, Journal = AKumar$Journal)
# colnames(AKumar_raw) <- c("Coauthor","Paper","Journal")
True_labels <- AKumar$AuthorID

raw_data <- AKumar_raw

setmember <- nrow(AKumar)

### training
trainingnumber <- ceiling(setmember * 0.8)
train.id <- sample(1:setmember, trainingnumber)
training_akumar <- raw_data[train.id,]
test_akumar <- raw_data[-train.id,]
dim(training_akumar)
dim(test_akumar)

ag5_akumar <- algorithm_paper_5(training_akumar, True_labels[train.id])

# sb <- NULL


# test_comemon_iamlazy(raw_data, paras, K)
test_comeon_iamlazy <- function(raw_data2, paras2, K){
  n_cluster <- nrow(raw_data2)
  labels <- 1:n_cluster
  while (length(unique(labels)) > K ){
    # iter is the number of cluster
    pf53 <- cosine_similarity(cluster_merge(raw_data2, labels))
    new_position <- one_step_cluster(pf53, paras2, labels)
    labels <- change_label(labels, pf53$CLUSTER.ID[new_position])
    cat("\n", labels)
  }
  return(labels)
}

test_true_label <- True_labels[-train.id]
test_our_label <- test_comeon_iamlazy(test_akumar, ag5_akumar$best, 10)


