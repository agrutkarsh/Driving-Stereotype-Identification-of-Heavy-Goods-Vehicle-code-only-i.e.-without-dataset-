svm_algo <- function(train,nc,out){
  
  library("e1071")
  library("mlbench")
  #data(Glass, package="mlbench")
  testset <- nc
  trainset <- train
  data<-cbind(train,as.factor(out))
  
  svm_tune <- tune(svm, train.x=trainset, train.y=as.factor(out), ranges=list(cost=10^(-1:2), gamma=c(.5,1,2)),cross = 10)
  
  print(svm_tune)
  
  svm_tune_model<- svm_tune$best.model
  
  svm.model <- svm(trainset,as.factor(out))
  #svm.pred <- predict(svm.model, testset)
  svm.pred <- predict(svm_tune_model, testset)
  return(svm.pred)
  
  #response<-rbind()
  #for (i in 1:nrow(out)){
  #  if (out[i]==1)
  #    response<-rbind(response,'a')
  #  if (out[i]==2)
  #    response<-rbind(response,'b')
  #  if (out[i]==3)
  #    response<-rbind(response,'c')
  #  if (out[i]==4)
  #    response<-rbind(response,'d')
  #  if (out[i]==5)
  #    response<-rbind(response,'e')
  #  if (out[i]==6)
  #    response<-rbind(response,'f')
  #  if (out[i]==7)
  #    response<-rbind(response,'g')
  #}
  #data1<-cbind(train,response)
  #svm.model <- svm(Type='C',out~.,data=data)
  #svm.model1 <- svm(response~.,data1)
  #svm.pred1 <- predict(svm.model1, testset)
  #svm.pred1
}