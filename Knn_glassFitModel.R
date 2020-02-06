#Prepare a model for glass classification using KNN
library("class")
library("gmodels")
?gmodels
getwd()
setw
glassdata <- read.csv(file.choose())
n <-nrow(glassdata) #214 
ncol(glassdata)#10columns
#verify wether na values are present in the dataset 
sum(is.na(glassdata)) # 0 na values
#pairs(glassdata)
summary(glassdata)
#without normalising the data as si is having range 69-75 as compared to others is high ..

#creating a dataset with train and Test .
n1 <- ceiling(0.8*n) #172
train <- sample(1:n,n1)
train_glassdata <- glassdata[train,]
test_glassdata <- glassdata[-train,]
levels(factor(train_glassdata$Type))# "1" "2" "3" "5" "6" "7"
levels(factor(test_glassdata$Type))# "1" "2" "3" "5" "6" "7"
levels(factor(glassdata$Type))# "1" "2" "3" "5" "6" "7"

#output to be classified is #type of cement
knn_classifier_Cement1 <- knn(train = train_glassdata,
                              test = test_glassdata,
                              cl = train_glassdata$Type,
                              k=1)

#length(knn_classifier_Cement1)
nrow(test_glassdata$Type)
table_test <-CrossTable( x = test_glassdata$Type,
            y = knn_classifier_Cement1,
            chisq = FALSE)

#finding test accuracy of the model :
(16+9+5+2+1+9)/42 #1 = 100%

#finding training accuracy 

knn_classifier_Cement_train <- knn(train = train_glassdata,
                              test = train_glassdata,
                              cl = train_glassdata$Type,
                              k=1)
table_train <-CrossTable( x = train_glassdata$Type,
                         y = knn_classifier_Cement_train,
                         chisq = FALSE)

train_accuray = 172/172 # 100%
?CrossTable

#find precison , recall ,sensitivity ,F1 score

#precison values :

Pc_1 <- 16/16+0 #1
PC_2 <- 9/(9+0) #1
Pc_3 <- 5/5+0 #1
PC_5 <-2/(2+0)#1
Pc_6 <-1/(1+0)#1
PC_7 <- 9/(9+0)#1

