knn_auto2 <- function(train,nc,out){
  
  library("class")
  
  k<-knn(train,nc,as.factor(out),k=7, prob = FALSE, use.all = TRUE)
  return(k)
}