#reading dataset
library(dplyr)
library(forcats)
library(tidyverse)

data <- read.csv("C:/Users/atl-vigneshn/Desktop/mcdaniel/ANA505/diabetic_data.csv")

summary(data) #missing values are denoted by "?"
str(data) #the data contains mostly integers and categorical variables

## Data Cleaning

#replacing all "?" with "NA"
data[data == "?"] <- NA
head(data)

# removing variables from the dataset
data$weight = NULL # mostly missing values
data$payer_code = NULL #type of payment
#data$medical_specialty = NULL #medical specialty of doctor

# removing variables which will not serve the analysis
data$encounter_id = NULL # id which will not have any effect o analysis
data$patient_nbr = NULL # id which will not have any effect o analysis
#data$diag_2 = NULL # will use diag_1 for analysis which is primary
#data$diag_3 = NULL # will use diag_1 for analysis which is primary
data$examide = NULL # have only one value "No"
data$citoglipton = NULL # have only one value "No"

# removing all other rows with missing values

# Automatically detect categorical variables
categorical_vars <- names(data[1:43])[sapply(data[1:43], is.character)]

# Perform label encoding
data_encoded <- data %>%
  mutate(across(all_of(categorical_vars), ~ as.integer(as.factor(.))))

library(caret)
library(lattice)

attach(data_encoded)
#Train/Test Split for SVM
inTrainingData <- createDataPartition(y= readmitted, p=0.70, list = FALSE)
trainData <- data_encoded[inTrainingData,]
testData <- data_encoded[-inTrainingData,]



#####knn
library(class)
acc_test<-data.frame(matrix(ncol=2,nrow=0))
colnames(acc_test)<-c("parameter","test accuracy")
###############scaling and substituting nan values with zero
train_scaled = scale(trainData[1:43])
test_scaled = scale(testData[1:43])
train_scaled[is.na(train_scaled)]<-0
test_scaled[is.na(test_scaled)]<-0

predict_opt<-knn(train=train_scaled,test=test_scaled,cl=trainData$readmitted,k=43,prob=T)

actual_value <- testData$readmitted
cm<-table(actual_value,predict_opt)
#cm_data<-data.frame(cm)

final_test_accuracy<- sum(diag(cm))/length(actual_value)


predict_opt_train<-knn(train=train_scaled,test=train_scaled,cl=trainData$readmitted,k=43,prob=T)

train_value<-trainData$readmitted
cm_train<-table(train_value,predict_opt_train)

final_train_accuracy<- sum(diag(cm_train))/length(train_value)


