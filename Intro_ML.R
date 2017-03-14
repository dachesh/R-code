###   First runs at ML in R   ###
require(stats)
require(ggplot2)
require(plyr)
require(ggvis)
require(class)
require(gmodels)

iris<-read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"),header=FALSE)
names(iris)<-c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

iris %>% ggvis(~Sepal.Length, ~Sepal.Width, fill=~Species) %>% layer_points()
iris %>% ggvis(~Petal.Length, ~Petal.Width, fill=~Species) %>% layer_points()

##  Training the data  ##
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

iris.train <- iris[ind==1, 1:4]
iris.test <- iris[ind==2, 1:4]

iris.trainLabel <- iris[ind==1, 5]
iris.testLable <- iris[ind==2, 5]

##  KNN (k nearest neighbors) Model  ##
iris_pred <- knn(train=iris.train, test=iris.test, cl=iris.trainLabel, k=3)  #k is typically an odd number to avoid ties

CrossTable(iris.testLable, iris_pred, prop.chisq = FALSE)
