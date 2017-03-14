require(stats)
require(ggplot2)
require(plyr)
require(ggvis)
require(class)
require(gmodels)

test.dat<-read.csv("/Users/eshelman/Documents/Adult_test.csv", sep=",", header = FALSE)
test.dat<-test.dat[-3]                                                                    # Dumps column 3 - meaningless data for this model
names(test.dat)<-c("1iage","workclass","education","education-num","marital-status","occupation","relationship","race",
                    "sex","capital-gain","capital-loss","hours-per-week","native-country","class")


levels(test.dat$class) = c('0','1')                                                       # Assigns <=50K to 0, >50K to 1 as factors


test.dat$workclass[grepl(" ?", test.dat$workclass, ignore.case=TRUE)] <- NA
test.dat$occupation[grepl(" ?", test.dat$occupation, ignore.case=TRUE)] <- NA
test.dat$`native-country`[grepl(" ?", test.dat$`native-country`, ignore.case=TRUE)] <- NA



##  Training the data  ##
ind <- sample(2, nrow(test.dat), replace=TRUE, prob=c(0.67, 0.33))

test.train <- test.dat[ind==1, 1:13]
test.test <- test.dat[ind==2, 1:13]

test.trainLabel <- test.dat[ind==1, 14]
test.testLable <- test.dat[ind==2, 14]

##  KNN (k nearest neighbors) Model  ##
test_pred <- knn(train=test.train, test=test.test, cl=test.trainLabel, k=2)  #k is typically an odd number to avoid ties

CrossTable(test.testLable, test_pred, prop.chisq = FALSE)
