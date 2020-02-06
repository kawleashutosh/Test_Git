#  problem statement :
#  Implement a KNN model to classify the animals in to categories. 
#  load the required libraries for the knn model:
library("class")
#load the data set 

getwd()
setwd("D:\\Data Science Excel R\\DataScience Assignments\\KNN")
ZooAnimals <- read.csv(file ="Zoo.csv") #DataSet -> Zoo.csv

#get Na value present in dataset
sum(is.na(ZooAnimals)) # 0 since no columns are having values as NA


# get column name and rownumber for the dataset provided..
ncol(ZooAnimals) #18 columns
nrow(ZooAnimals) #101 rows, observations 

#getcolmn Names
ZooAnimals[1,]

summary(ZooAnimals)
# animal.name feature contains animal name - remove
# output variablr to prdict is type
# scaling of data is not required a most of column values between o and 1, Except type
ZooAnimals <- ZooAnimals[,-1] # removing the animal name columns, as we don't need name of the animals.

#split the data in training and Test ration 75 and 31
n <- nrow(ZooAnimals)
n1 <- floor(n*0.8)
train <- sample(1:n,n1)
sort(train)
animalZoo_train <- ZooAnimals[train,]
animalZoo_test <-  ZooAnimals[-train,]

head(animalZoo_train) 
head(animalZoo_test)
sqrt(50.5)


# creating a knn model:

library(gmodels)

#creating best fit model , best fit model obtained at k=1
#getting test error
pedict_animalType_test <- knn(train =animalZoo_train, 
                          test = animalZoo_test,
                          cl = animalZoo_train$type,
                          k=1)

# creating confusion matrix for test accuracy.

confusionTest <- CrossTable( x=animalZoo_test$type,
                             y=pedict_animalType_test, prop.chisq = FALSE )
confusionTest2 <- table( x=animalZoo_test$type,y=pedict_animalType_test)                          


confusionTest

# Model accuracy : sum(diagonal)/total
accuracyTest <- 1 




# getting training Error
pedict_animalType_train <- knn( train = animalZoo_train, 
                                test =  animalZoo_train,
                                cl =  animalZoo_train$type,
                                k=1 )



# creating confusion matrix for train  accuracy.

 confusionTrain <-  CrossTable( x=animalZoo_train$type,
                                y=pedict_animalType_train,
                                prop.chisq = FALSE )

 accuarcy -1

 confusionTrain






# testtype <- as.factor(animalZoo_test$type)
# testtype

#below models can be ignored as their accuracy is low.

pedict_animalType5 <- knn(train =animalZoo_train, 
                         test = animalZoo_test,
                         cl = animalZoo_train$type,
                         k=5)


CrossTable(x=animalZoo_test$type,
           y=pedict_animalType5,
           prop.chisq = FALSE)




pedict_animalType4 <- knn(train =animalZoo_train, 
                         test = animalZoo_test,
                         cl = animalZoo_train$type,
                         k=4)



CrossTable(x=animalZoo_test$type,
           y=pedict_animalType4,
           prop.chisq = FALSE)



pedict_animalType3 <- knn(train =animalZoo_train, 
                          test = animalZoo_test,
                          cl = animalZoo_train$type,
                          k=3)


CrossTable(x=animalZoo_test$type,
           y=pedict_animalType3,
           prop.chisq = FALSE)




pedict_animalType2 <- knn(train =animalZoo_train, 
                          test = animalZoo_test,
                          cl = animalZoo_train$type,
                          k=2)


CrossTable(x=animalZoo_test$type,
           y=pedict_animalType2,
           prop.chisq = FALSE)


pedict_animalType1 <- knn(train =animalZoo_train, 
                          test = animalZoo_test,
                          cl = animalZoo_train$type,
                          k=1)


CrossTable(x=animalZoo_test$type,
           y=pedict_animalType1,
           prop.chisq = FALSE)


pedict_animalType3 <- knn(train =animalZoo_train, 
                          test = animalZoo_train,
                          cl = animalZoo_train$type,
                          k=1)


CrossTable(x=animalZoo_train$type,
           y=pedict_animalType3,
           prop.chisq = FALSE)




