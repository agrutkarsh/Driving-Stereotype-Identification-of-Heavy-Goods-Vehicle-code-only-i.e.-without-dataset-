knn_auto1 <- function(train,nc,out){
  
  library("lazyeval")
  library("ggplot2")
  library("lattice")
  library("caret")
  library("ISLR")
  library("e1071")
  
  tdata <- data.frame(train,as.factor(out))
  ctrl <- trainControl(method="repeatedcv",repeats = 3)
  knnFit <- train(as.factor.out. ~ ., data = tdata, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 2)
  plot(knnFit)
  knnPredict <- predict(knnFit,newdata = nc )
  return(knnPredict)
}