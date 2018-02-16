#setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

library("stats")

#data<-read.csv("short_anova_test_data146_ge2n.csv", header = TRUE)
data<-read.csv("short_anova_test_data1234567_ge4n.csv", header = TRUE)
#data<-read.csv("medium_anova_test_data1234567_ge4n.csv", header = TRUE)
#data<-read.csv("medium_anova_test_data146_ge2n.csv", header = TRUE)
#data<-read.csv("long_anova_test_data1234567_ge4n.csv", header = TRUE)
#data<-read.csv("long_anova_test_data146_ge2n.csv", header = TRUE)
data<-as.matrix(data)
data1<-read.csv("already_normalised_short_distance.csv", header = FALSE)
#data1<-read.csv("already_normalised_medium_distance.csv", header = FALSE)
#data1<-read.csv("already_normalised_long_distance.csv", header = FALSE)
data1<-as.matrix(data1)

n<-3
c3<-rbind()
for (i in 1:nrow(data)){
  if (data[i,5]==n){#6
    c3<-rbind(c3,data[i,1:4])
  }
}
c4<-rbind()
for (i in 1:nrow(data1)){
  if (data1[i,5]==n){#6
    c4<-rbind(c4,data1[i,1:4])
  }
}
temp1<-cbind()
temp2<-cbind()

col1<-1
c1<-c3[,col1]
c2<-c4[,col1]
y1<-cbind(c1,1)
y2<-cbind(c2,2)
y<-rbind(y1,y2)
y<-data.frame(y)
boxplot(y$c1 ~ y[,2],ylim=c(0,1),names=c("new instance","old instance"))
fit<-aov(c1 ~ y[,2],data=y)
summary(fit)
t.test(c1, c2, alternative="two.sided", var.equal=FALSE)
wilcox.test(c1, c2,alternative = "two.sided",mu = 0, paired = FALSE, exact = NULL, correct = TRUE,conf.int = FALSE, conf.level = 0.95)
temp1<-cbind(temp1,c1)
temp2<-cbind(temp2,c2)

col1<-2
c1<-c3[,col1]
c2<-c4[,col1]
y1<-cbind(c1,1)
y2<-cbind(c2,2)
y<-rbind(y1,y2)
y<-data.frame(y)
boxplot(y$c1 ~ y[,2],ylim=c(0,1),names=c("new instance","old instance"))
fit<-aov(c1 ~ y[,2],data=y)
summary(fit)
t.test(c1, c2, alternative="two.sided", var.equal=FALSE)
wilcox.test(c1, c2,alternative = "two.sided",mu = 0, paired = FALSE, exact = NULL, correct = TRUE,conf.int = FALSE, conf.level = 0.95)
temp1<-cbind(temp1,c1)
temp2<-cbind(temp2,c2)

col1<-3
c1<-c3[,col1]
c2<-c4[,col1]
y1<-cbind(c1,1)
y2<-cbind(c2,2)
y<-rbind(y1,y2)
y<-data.frame(y)
boxplot(y$c1 ~ y[,2],ylim=c(0,1),names=c("new instance","old instance"))
fit<-aov(c1 ~ y[,2],data=y)
summary(fit)
t.test(c1, c2, alternative="two.sided", var.equal=FALSE)
wilcox.test(c1, c2,alternative = "two.sided",mu = 0, paired = FALSE, exact = NULL, correct = TRUE,conf.int = FALSE, conf.level = 0.95)
temp1<-cbind(temp1,c1)
temp2<-cbind(temp2,c2)

col1<-4
c1<-c3[,col1]
c2<-c4[,col1]
y1<-cbind(c1,1)
y2<-cbind(c2,2)
y<-rbind(y1,y2)
y<-data.frame(y)
boxplot(y$c1 ~ y[,2],ylim=c(0,1),names=c("new instance","old instance"))
fit<-aov(c1 ~ y[,2],data=y)
summary(fit)
t.test(c1, c2, alternative="two.sided", var.equal=FALSE)
wilcox.test(c1, c2,alternative = "two.sided",mu = 0, paired = FALSE, exact = NULL, correct = TRUE,conf.int = FALSE, conf.level = 0.95)
temp1<-cbind(temp1,c1)
temp2<-cbind(temp2,c2)

# yaxis<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
# boxplot(temp1,width=1,ylim=c(0,1),names=c("Harsh Braking","Overspeeding","Excessive Throttle","Over Revs"),medcol = "red",outcol="red",boxcol="blue")
# axis(side=2, at="0.1")
# boxplot(temp2,ylim=c(0,1),names=c("Harsh Braking","Overspeeding","Excessive Throttle","Over Revs"),medcol = "red",outcol="red",boxcol="blue")

write.csv(temp1,"short_new_cls3.csv")
write.csv(temp2,"short_old_cls3.csv")

