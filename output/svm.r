#input is a dataframe

get_svm_result<-function(data){
library(e1071)
train<-c()
test<-c()
author<-unique(data$label)
for (i in author){
  d<-data[data$label==i,]
  n<-nrow(d)
  c<-0.5*n
  sample<-sample(1:n,c)
  test<-rbind(test, d[sample,])
  train<-rbind(train, d[-sample,])
}
#Tune 
tuneResult <- tune(svm, label ~ .,  data = train,
                     ranges = list(gamma = 10^(-6:-1), cost = 10^(-1:1)))
bestmodel <- tuneResult$best.model
g<-bestmodel$gamma
c<-bestmodel$cost
#svm.pre<-predict(bestmodel,test)
model1<-svm(label~.,data=train,gamma=g,cost=c)
svm.pre<-predict(model1,test)
test.error<-sum(svm.pre!=test$label)/nrow(test)
Accuracy=1-test.error
#CV Errors
#model<-svm(label~.,data=train,gamma=g,cost=c, cross=5)
#Ave.acc<-model$tot.accuracy
#SD.acc<-sd(model$accuracies)

return(Accuracy)

}