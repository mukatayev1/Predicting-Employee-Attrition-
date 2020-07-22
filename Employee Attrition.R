#Importing essential libraries for the project
library(class)
library(gmodels)
library(scales)
#set the working directory for the project
setwd('/Users/AZM/Desktop/Ch7.KNN')
#Importing the dataset
data = read.csv('Employee-Attrition.csv')
#Getting familiar with the dataset
head(data)
str(data)
#after analysis, we see that there is too much columns. They can create too much noise.
#selecting only relevant columns
data1 <- data[,c(1,2,12,17,19,23,29,31)]
head(data1)
str(data1)
#For kNN we need to convert all values into numeric
#converting factor values into numeric
data1$Attrition = as.integer(data1$Attrition) #No = 1, Yes = 2
data1$Gender = as.integer(data1$Gender) # Female = 1, Male = 2
data1$OverTime = as.integer(data1$OverTime) # No = 1, Yes = 2
#checking the structure of dataset to make sure that all values are numeric
head(data1)
str(data1)

#Normalization for kNN
#using all columns except for target(Attrition)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
  }
data1.n <- as.data.frame(lapply(data1[,c(1,3:8)], normalize))
head(data1.n)

#Partitioning our dataset
set.seed(123)
tr.ind <- sample(1:nrow(data1.n), 
                 size = nrow(data1.n) * 0.7,
                 replace = FALSE)

train.data1 <- data1[tr.ind,]
test.data1 <- data1[-tr.ind,]

#Creating separate dataframe for "Attrition" that is our target
train.labels <- data1[tr.ind, 2]
test.labels <- data1[-tr.ind, 2]



# choosing k
#try k=5 first
knn.5 <- knn(train=train.data1,
            test=test.data1,
            cl=train.labels,
            k=5)
#confusion matrix
cm <- table(knn.5,test.labels)
cm

#Checking accuracy of the test for k = 5
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(cm)  

# optimal k selection
accuracy_k <- NULL

nnum<-nrow(train.data1)/2
nnum

for(kk in c(1:nnum))
{
  set.seed(1234)
  knn_k<-knn(train=train.data1,test=test.data1,cl=train.labels,k=kk)
  accuracy_k<-c(accuracy_k,sum(knn_k==test.labels)/length(test.labels))
}

# plot for k=(1 to n/2) and accuracy
test_k<-data.frame(k=c(1:nnum), accuracy=accuracy_k[c(1:nnum)])
#Here we see the first 20 rows with k accuracy. But it's challenging to say which one's the best 
head(test_k, 20)

# the lowest k with the highest accuracy
min(test_k[test_k$accuracy %in% max(accuracy_k),"k"])
#k=18 is 0.8321995 = 83.2% accuracy

#k=18 knn
knn.18 <- knn(train=train.data1,
             test=test.data1,
             cl=train.labels,
             k=18)
#confusion matrix
cm1 <- table(knn.18,test.labels)
cm1
#Checking accuracy of the test for k = 18
accuracy(cm1)

