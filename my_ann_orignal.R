setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/edinburgh series')
library("pracma", lib.loc="~/R/R-3.2.2/library")
library("cclust", lib.loc="~/R/R-3.2.2/library")
library("cluster", lib.loc="~/R/R-3.2.2/library")
library("clValid", lib.loc="~/R/R-3.2.2/library")

data<-read.csv("output_4cls_unnormalised_data.csv", header = FALSE)
data2<-data[,1:10]
data2<-data2/300

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

trd<-t(train)
nhl<-20
nol<-4
nil<-nrow(trd)
ns<-ncol(trd)

wih<-rbind()
for (i in 1:nil){
  temp <- -1+2*(runif(nhl))
  wih <- rbind(wih,temp)
}

who<-rbind()
for (i in 1:nhl){
  temp <- -1+2*(runif(nol))
  who <- rbind(who,temp)
}
y<-out1
alpha <- 0.2
se <- 1
epoc <- 0
yhl<-matrix(0,nhl,1)
yp<-matrix(0,ns,nol)
error<-matrix(0,ns,nol)
delo<-matrix(0,nol,1)
delh<-matrix(0,nhl,1)

while (epoc<2000){
  epoc<-epoc+1
  for (i in 1:ns){
    
    for (j in 1:nhl){
      temp1<-t(trd[,i])
      temp2<-temp1*wih[,j]
      temp<-sum(temp2)
      yhl[j,1]=sigmoid(temp,a=1,b=0)
    }
    
    for (j in 1:nol){
      temp1<-t(yhl[,1])
      temp2<-temp1*who[,j]
      temp<-sum(temp2)
      yp[i,j]<-sigmoid(temp,a=1,b=0)
      error[i,j]<-y[i,j]-yp[i,j]
      delo[j,1]<-yp[i,j]*(1-yp[i,j])*error[i,j]
    }
    
    for (j in 1:nhl){
      for (k in 1:nol){
        who[j,k]<-who[j,k]+alpha*yhl[j,1]*delo[k,1]
      }
    }
    
    for (j in 1:nhl){
      temp=0
      for (k in 1:nol){
        temp<-temp+delo[k,1]*who[j,k]
      }
      delh[j,1]<-yhl[j,1]*(1-yhl[j,1])*temp
    }
    
    for (j in 1:nil){
      for (k in nhl){
        wih[j,k]<-wih[j,k]+alpha*trd[j,i]*delh[k,1]
      }
    }
    
    se<-sum((error^2))
    
  }
  print(se)
  print(epoc)
}

output<-rbind()
ynew<-matrix(0,nol,1)
for (loop in 1:nrow(nc)){
  x=nc[loop,]
  
  for (j in 1:nhl){
    temp1<-t(x)
    temp2<-temp1*wih[,j]
    temp<-sum(temp2)
    yhl[j,1]<-sigmoid(temp,a=1,b=0)
  }
  
  for (j in 1:nol){
    temp1<-t(yhl[,1])
    temp2<-temp1*who[,j]
    temp<-sum(temp2)
    ynew[j,1]<-sigmoid(temp,a=1,b=0)
  }
  ypos=1
  ymax=ynew[1,1]
  for (j in 1:(nrow(ynew)-1)){
    if (ymax<ynew[(j+1),1]){
      ymax<-ynew[j+1]
      ypos<-j+1
    }
  }
  output<-rbind(output,ypos)
}

write.csv(output,file="ann_result_on_unnormalised_data2.csv")
