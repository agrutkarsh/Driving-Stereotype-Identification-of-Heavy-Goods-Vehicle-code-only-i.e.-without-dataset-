setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/ml algos')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/ml algos')

#library("pracma")
library("cclust")
library("cluster")
library("clValid")
library("devtools")
library("nnls")
library("SuperLearner")
library("randomForest")

data<-read.csv("test_data_10_csv.csv", header = FALSE)
data1<-data[,1:10]
#data2<-data1/300
data2<-data

no_of_row<-nrow(data)
no_of_col<-ncol(data)
max_cls<-7

pos<-cbind()
nc<-rbind()
train<-rbind()
out<-rbind()
for (i in 1:no_of_row){
  if (data[i,11]=='NaN'){
    pos<-cbind(pos,i)
    n<-data2[i,1:10]
    nc<-rbind(nc,n)
  }
  else{
    n<-data2[i,1:10]
    train<-rbind(train,n)
    n<-data[i,11]
    out<-rbind(out,n)
  }
}
out1 <- array(0,dim=c(nrow(train),max_cls))
for (i in 1:nrow(train)){
  for (j in 1:max_cls){
    if (out[i]==j)
      out1[i,j]=1
  }
}

fit <- SuperLearner(Y = out,X = train, family = "binomial", SL.library = c("SL.knn", "SL.randomForest"), method = "method.NNLS")
