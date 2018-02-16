setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
#setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

#data<-read.csv("already_normalised_short_distance.csv", header = FALSE)
data<-read.csv("already_normalised_medium_distance.csv", header = FALSE)
#data<-read.csv("already_normalised_long_distance.csv", header = FALSE)

#ensemble_data1<-read.csv("short_ensemble_result.csv", header = TRUE)
ensemble_data1<-read.csv("medium_ensemble_result.csv", header = TRUE)
#ensemble_data1<-read.csv("long_ensemble_result.csv", header = TRUE)

res<-rbind()
for (i in 1:nrow(data)){
  if (data[i,5]==0)
    res<-rbind(res,data[i,c(1:4)])
}

ensemble_data<-ensemble_data1[,c(1:7)]
source('majority_voting.R')
ensemble_result <- majority_voting(ensemble_data)

n<-4
count2<-matrix(0,4,1)
temp<-rbind()
for (i in 1:nrow(ensemble_result)){
  if (max(ensemble_result[i,])>=n){
    count1<-matrix(0,4,1)
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
      if (count1[j,1]>=n)
        count2[j,1]=count2[j,1]+1
    }
    for (j in 1:nrow(count1)){
      if (count1[j,1]>=n)
        temp<-rbind(temp,cbind(res[i,],j))
    }
  }
}
# write.csv(temp,"medium_anova_test_data146_ge2n.csv")

res1<-rbind()
for (i in 1:nrow(temp)){
  if (temp[i,5]==1)
    res1<-rbind(res1,temp[i,c(1:4)])
}
temp1<-as.integer(rownames(res1))
data1<-data
k<-1
for (i in 1:nrow(data1)){
  if (k<=nrow(res1)){
    if (i==temp1[k]){
      data1[i,5]=5
      k<-k+1
    }
  }
}
#write.csv(data1,"short_distance_With_5groups.csv")
#write.csv(res1,"short_anova_test_data_potential_new group.csv")
#colors<-c("lavender")
#boxplot(res1,ylim=c(0,1),names=c("Harsh Braking","Overspeeding","Excessive Throttle","Over Revs"),medcol = "red",outcol="red",boxcol="blue")



