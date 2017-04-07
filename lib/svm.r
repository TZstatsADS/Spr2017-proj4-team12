#input is a dataframe

get_svm_result<-function(data){
library(e1071)
data<-read.csv("feature.csv")
data$label<-factor(data$label)
n<-ncol(data)
c<-0.2*n
sample<-sample(1:n,c)
test<-data[sample,]
train<-data[-sample,]
#Tune 
tuneResult <- tune(svm, label ~ .,  data = train,
                     ranges = list(gamma = seq(0,1,0.1), cost = 2^(2:9)))
g<-tuneResult$best.parameters["gamma"]
c<-tuneResult$best.parameters["cost"]
bestmodel <- tuneResult$best.model
svm.pre<-predict(bestmodel,test,decision.values = TRUE, probability = TRUE)
test.error<-sum(svm.pre==test$label)
#CV Errors
model<-svm(label~.,data=train,gamma=g,cost=c,cross=10)
Ave.acc<-mean(model$accuracies)
SD.acc<-sd(model$accuracies)

return(list(CVmean.accuracy=Ave.acc,CVsd.accuracy=SD.acc,TestError=test.error))

}