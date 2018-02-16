majority_voting <- function(ensemble_data){
  
  max_cls<-4

  temp <- array(0,dim=c(nrow(ensemble_data),max_cls))
  for (i in 1:ncol(ensemble_data)){
    for (j in 1:nrow(ensemble_data)){
      for (k in 1:max_cls){
        if (ensemble_data[j,i]==k)
          temp[j,k] <- temp[j,k] + 1
      }
    }
  }
  ensemble_result<-temp
  #res<-as.matrix(res)
  #count1<-0
  #for (i in 1:nrow(data)){
  #    if (data[i,1]==data[i,2])
  #      count1<-count1+1
  #  }
  #}
  #boxplot(res,ylim=c(0,1),las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"))
  #write.csv(data2,file="anova_test_file.csv")

  return(ensemble_result)
}

