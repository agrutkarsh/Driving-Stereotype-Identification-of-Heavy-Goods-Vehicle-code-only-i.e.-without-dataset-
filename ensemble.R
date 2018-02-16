#setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

#ensemble_data1<-read.csv("long_ensemble_result.csv", header = TRUE)
ensemble_data1<-read.csv("medium_ensemble_result.csv", header = TRUE)
#ensemble_data1<-read.csv("short_ensemble_result.csv", header = TRUE)
ensemble_data<-ensemble_data1[,c(1:7)]

source('majority_voting.R')
ensemble_result <- majority_voting(ensemble_data)
temp<-4
count=0
count2=matrix(0,temp,1)
for (i in 1:nrow(ensemble_result)){
  if (max(ensemble_result[i,])>=4){
    count=count+1
    count1=matrix(0,temp,1)
    for (j in 1:ncol(ensemble_data)){
      if (ensemble_data[i,j]==1)
        count1[1,1]=count1[1,1]+1
      if (ensemble_data[i,j]==2)
        count1[2,1]=count1[2,1]+1
      if (ensemble_data[i,j]==3)
        count1[3,1]=count1[3,1]+1
      if (ensemble_data[i,j]==4)
        count1[4,1]=count1[4,1]+1
    }
    for (j in 1:nrow(count1)){
      if (count1[j,1]>=4)
        count2[j,1]=count2[j,1]+1
    }
  }
}

