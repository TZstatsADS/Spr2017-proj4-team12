
### Author: Kai Chen
### Time: April 8, 2017
# setwd("~/Desktop/5243 ADS/Spr2017-proj4-team12/doc")
source("../lib/feature_paper5.R")
source("../lib/text_vectorize.R")
source("../lib/evaluation_measures.R")

################### Function part ############################


one_step_5 <- function(file_names){
  
  SOMEONE <- read.csv(file_names, as.is = T)
  SOMEONE <- SOMEONE[ifelse(rowSums(SOMEONE == "") > 0, F, T), ]
  
  SOMEONE_raw <- data.frame(Coauthor = SOMEONE$Coauthor,Paper = SOMEONE$Paper, Journal = SOMEONE$Journal)
  # colnames(AKumar_raw) <- c("Coauthor","Paper","Journal")
  True_labels <- SOMEONE$AuthorID
  
  setmember <- nrow(SOMEONE)
  
  ### training
  trainingnumber <- ceiling(setmember * 0.5)
  train.id <- sample(1:setmember, trainingnumber)
  training_SOMEONE <- SOMEONE_raw[train.id,]
 
 # dim(training_SOMEONE)
#  dim(test_SOMEONE)
  
 # raw_data <- training_SOMEONE
  True_labels_train <- SOMEONE$AuthorID[train.id]
  
  ag5_SOMEONE <- algorithm_paper_5(raw_data = training_SOMEONE, True_labels = True_labels_train,
                                   stepsize = 0.1, epi = 0.03)
  
  # sb <- NULL
  
  
  test_SOMEONE <- SOMEONE_raw[-train.id,]
  test_true_label <- SOMEONE$AuthorID[-train.id]
  KK <- length(unique(test_true_label))
  
  test_our_label <- test_comeon_iamlazy(test_SOMEONE, ag5_SOMEONE$best[ag5_SOMEONE$iter,], KK)
  perform <- performance_statistics(matching_matrix(test_true_label, test_our_label))
  return(c(perform$precision, perform$recall, perform$f1, perform$accuracy))
}


old_become_new_pf5 <- function(old5, add5){
  if (sum(old5$CLUSTER.ID != add5$CLUSTER.ID) ==0){
    old5$MATRIX[[4]] <- -add5$DISTANCE[[1]]$MIN
    old5$MATRIX[[5]] <- -add5$DISTANCE[[1]]$MAX
    old5$MATRIX[[6]] <- -add5$DISTANCE[[1]]$MEAN
    old5$MATRIX[[7]] <- -add5$DISTANCE[[2]]$MIN
    old5$MATRIX[[8]] <- -add5$DISTANCE[[2]]$MAX
    old5$MATRIX[[9]] <- -add5$DISTANCE[[2]]$MEAN
    old5$MATRIX[[10]] <- -add5$DISTANCE[[3]]$MIN
    old5$MATRIX[[11]] <- -add5$DISTANCE[[3]]$MAX
    old5$MATRIX[[12]] <- -add5$DISTANCE[[3]]$MEAN
    return(old5)
  }
  else
    return(F)
}

find_max_in_the_matrix <- function(A){
  dim_A <- dim(A)[1]
  for (i in 1:dim_A){
    A[i,i] <- min(A)
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
  old_record <- colMeans(matrix(x[1:n,],nrow = n))
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
update_para <- function(Recom_merge, Est_merge, paras1, threebigmatrix, stepsize, scale1 = T){
  # Recom_merge, Est_merge are both in (i,j) form
  # feature3 is a list with 3 matrix
  paras_new <- paras1 + 
    stepsize * (compute_3_feature(Recom_merge, threebigmatrix) -
    compute_3_feature(Est_merge, threebigmatrix))
  if (scale1)
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
  
  # Labels  -- > Clusters should be merged 
  unique_label <- unique(Test_label_previous)
  n_label <- length(unique_label)
  for (i in unique_label[sample(1:n_label)]){
    
    previous_same_id_i <- which(Test_label_previous == i) 
      # the position in the previous indivi.id
    
    same_in_true_labels <- which(True_labels == True_labels[previous_same_id_i[1]]) 
      # select every position that is same with selected ones
    
    new_point_tf <- (!(same_in_true_labels %in% previous_same_id_i))
      # F: not in the previous labels 
    
    if (sum(new_point_tf) > 0){
      
      # start coloring 
      change_id <- same_in_true_labels[new_point_tf]
        # corresponding indices that could be colored in the previous
      merge_cluster <- Test_label_previous[change_id][1]
        # calculate the value in the old labels
      
      ##(i, mer_cluster) 
      xi <- which(i == bigmatrices_id) ## position in the matrix
      yi <- which(merge_cluster == bigmatrices_id)
      return(c(xi, yi))
    }
  }  
  return(F)
}

# Give_you_better3(c(4,5,5,5,4,5),c(3,2,1,2,3,7),bigmatrices_id)

bigmatrices_id <- unique(c(3,2,1,2,3,7))

change_label <- function(label, CLUSTER.ID){
  ## INPUT: cluster.id, not individual.id
  i <- CLUSTER.ID[1]
  j <- CLUSTER.ID[2]
  label[label == i] <- j
  return(label)
}



# Compute Each Step in Cluster
one_step_cluster <- function(pf5, paras1, label){
  # paras1 should be a vector 
  ## wo men zhi shi fan hui le yi ge  zai ju zhen li mian de wei zhi !!!! qing zhuan huan!!!
  ## pfnew5 is added features!!!
  
  cluster_2id <- pf5[[1]] #cluster.id
  bigmatrix <- pf5[[2]] # list of 12 Ms
  
  n_features <- length(paras1)
  score_matrix <- 0
  for (sumi in 1:n_features){
    score_matrix <- score_matrix + paras1[sumi] * bigmatrix[[sumi]]
  }
  position <- find_max_in_the_matrix(score_matrix)$LOCATION # find our est_label of cluster
  return(position)

}

################### Function part ############################


algorithm_paper_5 <- function(raw_data, True_labels, 
                              max.iter = 30, stepsize = 0.1,
                              epi = 0.03){
  
  # True_labels should be author.id
  # raw data := list of 3 matices
  n_obs <- nrow(raw_data)
  n_features <- 3 # This number is decided by our selection of features
  
  # Initial assignment
  paras <- rep(0, n_features)
  paras_t0 <- (paras)
  paras <- rbind(paras,rep(1/n_features, n_features))
  paras_t1 <- colMeans(paras)
  
  t <- 1  # iter control
  
  # iteration
  while((t <= max.iter) & (compute_distance(paras_t0, paras_t1) > epi)){
    
    # initial assignment
    old_labels <- 1:n_obs
    # length(unique(old_labels))
    
    for (i.ter in 1:n_obs){
      
      ## pf5 is the way we produced cosine features!!!!!!!!!!
      ## output: 
      ## list1 = cluster.id coresponding to each row of matrix, helping the position to merge.id 
      ## list2 = 3matrix
      
      merge_results1 <- cluster_merge(raw_data, old_labels)
      pf5 <- cosine_similarity(merge_results1) # contributed by chenyun  
      
      ## additional 9 features
     # raw_data_list <- list(Coauthor = raw_data$Coauthor, Paper = raw_data$Paper, Journal = raw_data$Journal)
   #   pfnew5_ <- text_feature(text_matrix_function(cluster_merge(raw_data_list, old_labels)))
   #    pfnew5 <- text_feature(text_matrix_function(merge_results1)) ## ju zhen yue xiao yue hao
  #     pf5 <- old_become_new_pf5(pf5, pfnew5)
      
      # for each step, we merge only two clusters
      m_position <- one_step_cluster(pf5, paras1 = paras[t+1,], old_labels) # est cluster position
      m_labels <- pf5$CLUSTER.ID[m_position]
      
    #  sb<- rbind(sb, m_labels)
      new_labels <- change_label(label = old_labels, CLUSTER.ID = m_labels)
      
      cat('\ni.ter=', i.ter)
#      cat('\n new_labels', new_labels)
      
      
      if (!no_error(True_labels, new_labels)){ ##  deny

        better_labels <- Give_you_better3(True_labels = True_labels,
                                          Test_label_previous =  old_labels, 
                                          bigmatrices_id = pf5$CLUSTER.ID)
        
        ## wan quan dui le 
        if (sum(better_labels)==0)
          return(paras[t+1,])
        
        # Update our paras 
        paras0 <- update_para(Recom_merge = better_labels, Est_merge = m_position, 
                              paras1 = paras[t+1,], threebigmatrix = pf5[[2]],
                              stepsize = stepsize) 
        paras_t0 <- colMeans(paras)
        paras <- rbind(paras, paras0)
        t <- t + 1
        paras_t1 <- colMeans(paras)
        cat('\n paras:=',paras[t+1,])
        
        #cat("better: ", better_labels, ";ours: ",m_labels, "paras: ", paras[t+1,],"\n")
        break
      }
      
      old1 <- old_labels # backup
      # continue
      old_labels <- new_labels # accept
      
    }
 #   cat('\n paras:=',paras[t+1,])
  }

  result <- list(best = paras, iter = t-1)
  return(result)
}


# test_comemon_iamlazy(raw_data, paras, K)
test_comeon_iamlazy <- function(raw_data2, paras2, K){
  n_cluster <- nrow(raw_data2)
  labels <- 1:n_cluster
  while (length(unique(labels)) > K ){
    # iter is the number of cluster
    
    merge_results1 <- cluster_merge(raw_data2, labels)
    pf53 <- cosine_similarity(merge_results1) # contributed by chenyun  
    
    ## additional 9 features
    # raw_data_list <- list(Coauthor = raw_data$Coauthor, Paper = raw_data$Paper, Journal = raw_data$Journal)
  #   pfnew5 <- text_feature(text_matrix_function(cluster_merge(raw_data2, labels)))
     
  #   pf53 <- old_become_new_pf5(pf53, pfnew5)
     
    new_position <- one_step_cluster(pf53, paras2, labels)
    labels <- change_label(labels, pf53$CLUSTER.ID[new_position])
 #   cat("\n", labels)
  }
  return(labels)
}

################## test ##########################

# library(parallel) 

# Filesname <- c("AGupta","AKumar","CChen","DJohnson","JLee","JMartin","JRobinson","JSmith","KTanaka","MBrown","MJones","MMiler","SLee","Ychen")
# files <- paste0("../output/Coauthor_No_Space/",Filesname,".csv")
# no_cores <- detectCores() - 1
# cl1 <- makeCluster(no_cores, type="FORK")
# performance <- parSapply(cl1, files[1:4], one_step_5)
# stopCluster(cl1)


Filesname <- c("AGupta","AKumar","CChen","DJohnson","JLee","JMartin","JRobinson","JSmith","KTanaka","MBrown","MJones","MMiler","SLee","Ychen")
files <- paste0("../output/Coauthor_No_Space/",Filesname,".csv")
time1 <- system.time(perform1 <- one_step_5(files[1]))
time2 <- system.time(perform2 <- one_step_5(files[2]))
time3 <- system.time(perform3 <- one_step_5(files[3]))
time4 <- system.time(perform4 <- one_step_5(files[4]))
time5 <- system.time(perform5 <- one_step_5(files[5]))
time6 <- system.time(perform6 <- one_step_5(files[6]))
time7 <- system.time(perform7 <- one_step_5(files[7]))
time8 <- system.time(perform8 <- one_step_5(files[8]))
time9 <- system.time(perform9 <- one_step_5(files[9]))
time10 <- system.time(perform10 <- one_step_5(files[10]))
time11 <- system.time(perform11 <- one_step_5(files[11]))
time12 <- system.time(perform12 <- one_step_5(files[12]))
time13 <- system.time(perform13 <- one_step_5(files[13]))
time14 <- system.time(perform14 <- one_step_5(files[14]))

