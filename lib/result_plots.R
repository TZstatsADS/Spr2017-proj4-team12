# Baseline results & parameters
baseline <- read.csv("../output/baseline.csv", header = T, as.is = T)
improved_10_11_13 <- load("../output/improved_101113.RData")
improved_other <- load("../output/chenkai.RData")
perform12 <- c(0.35554497, 0.8719, 0.5050, 0.3973, .1288, .2666, .2739, -0.0260, .1540, .2027) 
perform14 <- c(0.5672, 0.4781, 0.3569, 0.7861, .1288, .2666, .2739, -0.0260, .1540, .2027) 
improved_df <- as.data.frame(rbind(perform1,perform2, perform3, perform4, perform5,perform6, 
      perform7, perform8, perform9, perform10,
      perform11, perform12, perform13, perform14))

colnames(improved_df) <- c("precision2", "recall2", "f1_2", "accuracy2", 
                           "para_cos_coauthor2", "para_cos_paper2", "para_cos_journal2",
                           "para_dis_coauthor2", "para_dist_paper2", "para_dist_journal2")

new_result <- data.frame(baseline, improved_df)


a <-colMeans(new_result[,-1])

library(ggplot2)

# ggplot(new_result)+
#   geom_line(aes(x=rep(new_result$Author,2), y= c(new_result$precision, new_result$precision2), colour = factor(c(rep(1,14),rep(-1,14)))))

plot(x=new_result$Author, y=new_result$precision, type = "o", 
     ylim = c(0,1), xlab = "Author", ylab = "Precision", main = "Precision Comparison")
lines(x=new_result$Author, y=new_result$precision2, col="red")

plot(x=new_result$Author, y=new_result$recall, type = "o", 
     ylim = c(0,1), xlab = "Author", ylab = "Recall", main = "Recall Comparison")
lines(x=new_result$Author, y=new_result$recall2, col="red")

plot(x=new_result$Author, y=new_result$f1, type = "o", 
     ylim = c(0,1), xlab = "Author", ylab = "f1", main = "F1 Comparison")
lines(x=new_result$Author, y=new_result$f1_2, col="red")

plot(x=new_result$Author, y=new_result$accuracy, type = "o", 
     ylim = c(0,1), xlab = "Author", ylab = "Accuracy", main = "Accuracy Comparison")
lines(x=new_result$Author, y=new_result$accuracy2, col="red")