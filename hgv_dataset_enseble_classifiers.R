setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

#library("pracma")
library("cclust")
library("cluster")
library("clValid")

data<-read.csv("already_normalised_short_distance.csv", header = FALSE)
#data<-read.csv("already_normalised_medium_distance.csv", header = FALSE)
#data<-read.csv("already_normalised_long_distance.csv", header = FALSE)

data1<-data[,1:4]
#data2<-data1/300
data2<-data

no_of_row<-nrow(data)
no_of_col<-ncol(data)
max_cls<-3

pos<-cbind()
nc<-rbind()
train<-rbind()
out<-rbind()
for (i in 1:no_of_row){
  if (data[i,5]==0){
    pos<-cbind(pos,i)
    n<-data2[i,1:4]
    nc<-rbind(nc,n)
  }
  else{
    n<-data2[i,1:4]
    train<-rbind(train,n)
    n<-data[i,5]
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

#write.csv(nc,"already_normalised_long_distance_test_deep_file.csv")

### SVM ###

source('svm_algo.R')
svm.pred <- svm_algo(train,nc,out)

### Kernel SVM ###

#library(kernlab)
#svp <- ksvm(x=train,y=response,type="C-svc",kernel='stringdot',kpar = list(length = 4, lambda = 0.5),C=100,scaled=c(),cross=10)
#svp

### Nearest Neighbour ###

source('knn_auto1.R')
knnPredict1 <- knn_auto1(train,nc,out)

source('knn_auto2.R')
knnPredict2 <- knn_auto2(train,nc,out)

source('knn_algo.R')
knnPredict3 <- knn_algo(train,nc,out)

### Deep result ###
decisionPrreict<-read.csv("long_decision_tree.csv", header = TRUE)
deepPredict<-read.csv("long_deep_result.csv", header = TRUE)
ensemble_data1<-read.csv("long_ensemble_result_without_deep.csv", header = TRUE)
ensemble_data<-cbind(ensemble_data1,deepPredict,decisionPrreict)
### Majority Voting ###

#ensemble_data<-cbind(data.frame(svm.pred),data.frame(knnPredict1),data.frame(knnPredict2),knnPredict3)

#write.csv(ensemble_data,file="long_ensemble_result_without_deep.csv")

source('majority_voting.R')
ensemble_result <- majority_voting(ensemble_data)

count=0
for (i in 1:nrow(ensemble_result)){
  if (max(ensemble_result[i,])==8)
    count=count+1
}
#write.csv(ensemble_result,"already_normalised_long_distance_ensemble_file.csv")
### Decision Tree ###

#library("rpart")
#library("rpart.plot")
#data("iris")
#tree<-rpart(Species~.,data=iris,method="class")
#rpart.plot(tree)

### Multi Layer Perceptron (ANN) code (copie from Edinburgh dataset) ###

### Use MLP and deep learning from Matlab ###

#library("Rcpp")
#library("RSNNS")


#out2 <- decodeClassLabels(out)
#out3 <- t(out1)

#nn <- mlp(train, out1, size = c(20), maxit = 2000, initFunc = "Randomize_Weights", initFuncParams = c(-0.99, 0.99), learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2), inputsTest = nc)
#nn <- mlp(train, out1, size = c(20), maxit = 100,  initFunc = "Randomize_Weights", initFuncParams = c(-0.99, 0.99), learnFunc = "Std_Backpropagation", learnFuncParams = c(0.2))
#p<-predict(nn,nc)

#library("nnet")

#n<-nnet(train,out1,size=20, softmax = TRUE,rang=0.2,maxit=2000)
#p<-predict(n,nc)
#p

# trd<-t(train)
# nhl<-20
# nol<-4
# nil<-nrow(trd)
# ns<-ncol(trd)
# 
# set.seed(10000)
# 
# wih<-rbind()
# for (i in 1:nil){
#   temp <- -1+2*(runif(nhl))
#   wih <- rbind(wih,temp)
# }
# 
# who<-rbind()
# for (i in 1:nhl){
#   temp <- -1+2*(runif(nol))
#   who <- rbind(who,temp)
# }
# y<-out1
# alpha <- 0.2
# se <- 1
# epoc <- 0
# yhl<-matrix(0,nhl,1)
# yp<-matrix(0,ns,nol)
# error<-matrix(0,ns,nol)
# delo<-matrix(0,nol,1)
# delh<-matrix(0,nhl,1)
# 
# while (epoc<2000){
#   epoc<-epoc+1
#   for (i in 1:ns){
#     
#     for (j in 1:nhl){
#       temp1<-t(trd[,i])
#       temp2<-temp1*wih[,j]
#       temp<-sum(temp2)
#       yhl[j,1]=sigmoid(temp,a=1,b=0)
#     }
#     
#     for (j in 1:nol){
#       temp1<-t(yhl[,1])
#       temp2<-temp1*who[,j]
#       temp<-sum(temp2)
#       yp[i,j]<-sigmoid(temp,a=1,b=0)
#       error[i,j]<-y[i,j]-yp[i,j]
#       delo[j,1]<-yp[i,j]*(1-yp[i,j])*error[i,j]
#     }
#     
#     for (j in 1:nhl){
#       for (k in 1:nol){
#         who[j,k]<-who[j,k]+alpha*yhl[j,1]*delo[k,1]
#       }
#     }
#     
#     for (j in 1:nhl){
#       temp=0
#       for (k in 1:nol){
#         temp<-temp+delo[k,1]*who[j,k]
#       }
#       delh[j,1]<-yhl[j,1]*(1-yhl[j,1])*temp
#     }
#     
#     for (j in 1:nil){
#       for (k in nhl){
#         wih[j,k]<-wih[j,k]+alpha*trd[j,i]*delh[k,1]
#       }
#     }
#     
#     se<-sum((error^2))
#     
#   }
#   print(se)
#   print(epoc)
# }
# 
# output<-rbind()
# ynew<-matrix(0,nol,1)
# for (loop in 1:nrow(nc)){
#   x=nc[loop,]
#   
#   for (j in 1:nhl){
#     temp1<-t(x)
#     temp2<-temp1*wih[,j]
#     temp<-sum(temp2)
#     yhl[j,1]<-sigmoid(temp,a=1,b=0)
#   }
#   
#   for (j in 1:nol){
#     temp1<-t(yhl[,1])
#     temp2<-temp1*who[,j]
#     temp<-sum(temp2)
#     ynew[j,1]<-sigmoid(temp,a=1,b=0)
#   }
#   ypos=1
#   ymax=ynew[1,1]
#   for (j in 1:(nrow(ynew)-1)){
#     if (ymax<ynew[(j+1),1]){
#       ymax<-ynew[j+1]
#       ypos<-j+1
#     }
#   }
#   output<-rbind(output,ypos)
# }

#write.csv(output,file="ann_result_on_unnormalised_data2.csv")