setwd('C:/Users/psxua/Dropbox/projects/phd/codes/r/hgv')
setwd('C:/Users/utkarsh/Dropbox/projects/phd/codes/r/hgv')

library('rpart')
library('mlbench')
library('lattice')
library('ggplot2')
library('caret')
library ('adabag')


train <- read.csv("nott_train_data1.csv", header = TRUE)
test <- read.csv("nott_test_data.csv", header = TRUE)

data(iris)
ada<-boosting(out~.,data=train, boos = TRUE, mfinal = 1, coeflearn = 'Breiman')
#use output as factor and it will work
#ada<-boosting(Species~.,data=iris, boos = TRUE, mfinal = 1, coeflearn = 'Breiman')
