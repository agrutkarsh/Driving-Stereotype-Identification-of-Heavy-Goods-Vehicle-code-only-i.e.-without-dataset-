setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/edinburgh series')
library("pracma", lib.loc="~/R/R-3.2.2/library")
library("cclust", lib.loc="~/R/R-3.2.2/library")
library("cluster", lib.loc="~/R/R-3.2.2/library")
library("clValid", lib.loc="~/R/R-3.2.2/library")

data<-read.csv("output_4cls_unnormalised_data.csv", header = FALSE)
data2<-data[,1:10]
#data2<-data1/300

no_of_row<-nrow(data)
no_of_col<-ncol(data)
max_cls<-4

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

testd<-t(nc)
number<-length(pos)
now<-nrow(train)
train1<-t(train)
#seeit<-matrix(0,number,now)
ab<-matrix(0,now,1)
output<-rbind()
result<-rbind()

for (i in 1:number){
  
  testd1<-testd[,i]
  for (j in 1:now){
    s<-0
    for (k in 1:ncol(train)){
      s<-s+((train1[k,j]-testd1[k])^2)
    }
    temp<-(s^0.5)
    ab[j,1]<-temp
    #seeit[i,j]<-temp
  }
  val<-min(ab)
  val_pos<-which.min(ab)
  output<-rbind(output,val_pos)
  result<-rbind(result,out[val_pos])
}
res<-rbind()
for (i in 1:nrow(nc)){
  if (result[i]==5)
    res<-rbind(res,nc[i,])
}
write.csv(result,file="nn_result_on_unnormalised_data.csv")
#res1<-res*300
#colors<-c(rep("lavender",10))
#boxplot(res1,las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"),col=colors)
#axis(side = 2, font = 2)
