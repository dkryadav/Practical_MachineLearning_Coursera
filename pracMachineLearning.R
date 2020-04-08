#################
# Initial setup #
#################

# Load libraries
library(tidyverse)
library(caret)

# Download files
file1 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv" # Training data
file2 <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv" # Testing data

if (!file.exists("pml-training.csv")) {download.file(file1, destfile = "./pml-training.csv")}
if (!file.exists("pml-testing.csv")) {download.file(file2, destfile = "./pml-testing.csv")}

# Load files into environment
training <- data.frame(read.csv("pml-training.csv"))
testing <- data.frame(read.csv("pml-testing.csv"))

# Remove summary statistics, timestamps, and windows from data
omitvars <- grep("avg|max|min|total|var|stddev|skewness|kurtosis|amplitude|timestamp|window",
     names(training), value = TRUE)
training <- select(training, !omitvars & !X)
testing <- select(testing, !omitvars & !X)

# Convert relevant data to numeric class
training[,2:49] <- sapply(training[,2:49], as.numeric)
testing[,2:49] <- sapply(testing[,2:49], as.numeric)

########################
# Exploratory analysis #
########################

str(training)
table(training$user_name, training$classe)

################################
# Modeling using random forest #
################################

set.seed(1) # Set seed

# Configure resampling, 5-fold cross-validation
trControl <- trainControl(method = "cv", number = 5, allowParallel = TRUE)

# Run training model
rf <- train(classe ~ ., method="rf", data=training, trControl=trControl)

# Assess model
rf
rf$resample
confusionMatrix.train(rf) # 0.9942 accuracy

########################################
# Predict using model and testing data #
########################################

pred <- predict(rf, testing)
pred
