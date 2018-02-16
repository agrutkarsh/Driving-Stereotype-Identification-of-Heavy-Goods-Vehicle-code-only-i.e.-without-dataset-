setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

#library("pracma")
library("cclust")
library("cluster")
library("clValid")
library("devtools")

library("h2o")

out<-read.csv("already_normalised_medium_distance.csv", header = FALSE)
out<-cbind(train,out)

out11<-rbind()
for (i in 1:nrow(out)){
 if (out[i,5]==1)
   out11<-rbind(out11,'a')
 if (out[i,5]==2)
   out11<-rbind(out11,'b')
 if (out[i,5]==3)
   out11<-rbind(out11,'c')
 if (out[i,5]==4)
   out11<-rbind(out11,'d')
 if (out[i,5]==0)
   out11<-rbind(out11,'e')
}

data<-cbind(out[,1:4],out11)
write.csv(data,"already_normalised_long_distance_train_deep_file.csv")

#data<-read.csv("already_normalised_short_distance_deep_file.csv", header = TRUE)


localh2o <- h2o.init()

train <- h2o.importFile(path="already_normalised_long_distance_train_deep_file.csv", header = TRUE)

y<-"out"
x <- setdiff(names(train), y)
family <- "binomial"
learner <- c("h2o.glm.wrapper", "h2o.randomForest.wrapper")
metalearner <- "SL.glm"

fit <- h2o.deeplearning(x = x, y = y, training_frame = train, hidden = c(10,15), epochs = 500)

test_data<-h2o.importFile(path="already_normalised_long_distance_test_deep_file.csv", header = TRUE)
pred <- h2o.predict(object = fit, newdata = test_data)
output <- as.data.frame(pred)

h2o.shutdown()

out11<-rbind()
for (i in 1:nrow(pred)){
  if (output[i,1]=='a')
    out11<-rbind(out11,1)
  if (output[i,1]=='b')
    out11<-rbind(out11,2)
  if (output[i,1]=='c')
    out11<-rbind(out11,3)
  if (output[i,1]=='d')
    out11<-rbind(out11,4)
  # if (output[i,1]=='e')
  #   out11<-rbind(out11,5)
  # if (output[i,1]=='f')
  #   out11<-rbind(out11,6)
  # if (output[i,1]=='g')
  #   out11<-rbind(out11,7)

}

write.csv(out11,"long_deep_result_200.csv")
