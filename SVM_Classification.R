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


data$weight = NULL
data$payer_code = NULL
data$medical_specialty = NULL

# removing variables which will not serve the analysis
data$encounter_id = NULL # id which will not have any effect o analysis
data$patient_nbr = NULL # id which will not have any effect o analysis
data$diag_2 = NULL # will use diag_1 for analysis which is primary
data$diag_3 = NULL # will use diag_1 for analysis which is primary
data$examide = NULL # have only one value "No"
data$citoglipton = NULL # have only one value "No"

numerical_vars <- c("time_in_hospital", "num_lab_procedures", "num_procedures",
                    "num_medications", "number_outpatient", "number_emergency",
                    "number_inpatient", "number_diagnoses")


#Encoding
library(dplyr)
library(forcats)

# Assuming 'data' is your dataframe

# changing column type from numerical to categorical/nominal
#data$admission_type_id <- as.factor(data$admission_type_id)
#data$discharge_disposition_id <- as.factor(data$discharge_disposition_id)
#data$admission_source_id <- as.factor(data$admission_source_id)

# Automatically detect categorical variables
categorical_vars <- names(data)[sapply(data, is.character)]

# Perform label encoding
data_encoded <- data %>%
  mutate(across(all_of(categorical_vars), ~ as.integer(as.factor(.))))

# Check the encoded data
str(data_encoded)

# Libraries
library(e1071)


attach(data_encoded)
#Train/Test Split for SVM
inTrainingData <- createDataPartition(y= readmitted, p=0.70, list = FALSE)
trainData <- data_encoded[inTrainingData,]
testData <- data_encoded[-inTrainingData,]

trainData[1:43] <-scale(trainData[1:43])
testData[1:43] <-scale(testData[1:43])
trainData[is.na(trainData)]<-0
testData[is.na(testData)]<-0
trainData$readmitted = factor(trainData$readmitted)
svm_model <- svm(readmitted ~ ., data=trainData, type="C",
                 kernel="radial") #linear/polynomial/sigmoid

pred = predict(svm_model,testData)
tab = table(Predicted=pred, Actual = testData$readmitted)
1-sum(diag(tab)/sum(tab))
sum(diag(tab)/sum(tab))


