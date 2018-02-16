knn_algo <- function(train,nc,out){
  
  testd<-t(nc)
  number<-nrow(nc)
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
  
  return(result)
  #res<-rbind()
  #for (i in 1:nrow(nc)){
  #  if (result[i]==5)
  #    res<-rbind(res,nc[i,])
  #}
  
  #write.csv(result,file="nn_result_on_unnormalised_data.csv")
  #res1<-res*300
  #colors<-c(rep("lavender",10))
  #boxplot(res1,las=2,names=c("ER","PgR","CK7/8","CK5/6","EGFR","HER2","HER3","HER4","p53","MUC1"),col=colors)
  #axis(side = 2, font = 2)
}
  
  