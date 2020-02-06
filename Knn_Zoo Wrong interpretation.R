# Knn algorithm practice :
#Implement a KNN model to classify the animals in to categories. 
library('class')
#load the data set Z00 in r :
animals <- read.csv(file.choose())
#getting file headers.
 ncol(animals) #get features 
 n <- nrow(animals) # total rrows in the data 
# aminals_name <- (factor(animals[,1]))
#  table(levels(aminals_name))
summary(animals)

# only type column range is from 0 to 7 while all other columns
# are having range 0-1 hence scaling is not required

#divide tha data in train and test, Sampling the data randomly
n1 <- floor(nrow(animals)*(.7))
train<- sample(1:n,n1) #creates a sample of 75 randomly choosen values,
#sort(train)
#create training data for the model ...
animal_train <- animals[train,]
nrow(animal_train) # 70 row in training data 
#creating test data for model...............
animal_test <- animals[-train,]
nrow(animal_test) # 31 in test data 
animal_train$animal.name
sum(is.na(animals))

??knn

animals_predivt <- knn(train = animal_train[-1],test=animal_test[-1],cl = animal_train$animal.name,k=21)

animals_predivt
install.packages("gmodels")
library(gmodels)

animal_crsstbl <- CrossTable(x=animal_test$animal.name,
                              y=animals_predivt,
                              prop.chisq = FALSE)

testval <- data.frame(animals_predivt,animal_test$animal.name)
testval
