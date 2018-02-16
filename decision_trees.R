#setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/ml algos')
setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/ml algos')

library("rpart")
library("rpart.plot")
data("iris")
data<-read.csv("already_normalised_short_distance.csv", header = FALSE)
data<-cbind(train,out)
tree<-rpart(out~.,data=data,method="class")
rpart.plot(tree)
pred<-predict(tree,nc,method="class")

res<-rbind()
for (i in 1:nrow(pred)){
  for (j in 1:ncol(pred)){
    if (pred[i,j]>.8){
      res<-rbind(res,j)
    }
  }
}

write.csv(res,"short_decision_tree.csv")
count=0
for (i in 1:nrow(pred)){
  if (res[i]==1)
    count=count+1
}