#input is a dataframe

get_svm_result<-function(data){
#library(e1071)
data$label<-factor(data$label)
n<-nrow(data)
c<-0.2*n
sample<-sample(1:n,c)
test<-data[sample,]
train<-data[-sample,]

tuneResult <- tune(svm, label ~ .,  data = train,
                     ranges = list(gamma = seq(0,1,0.1), cost = 2^(2:9)),tunecontrol = tune.control(cross=5))
g<-tuneResult$best.parameters$gamma
c<-tuneResult$best.parameters$cost
bestmodel <- tuneResult$best.model
svm.pre<-predict(bestmodel,test)
test.error<-sum(svm.pre!=test$label)/nrow(test)
#CV Errors
model<-svm(label~.,data=train,gamma=g,cost=c,cross=5)
Ave.acc<-mean(model$accuracies/100)
SD.acc<-sd(model$accuracies/100)

return(list(CVmean.accuracy=Ave.acc,CVsd.accuracy=SD.acc,TestError=test.error))

}