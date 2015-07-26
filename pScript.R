# Project Use HAR data at:
#		http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#The goal of your project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. You may use any of the other variables to predict with. You should create a report describing how you built your model, how you used cross validation, what you think the expected out of sample error is, and why you made the choices you did. You will also use your prediction model to predict 20 different test cases. 

#1. Your submission should consist of a link to a Github repo with your R markdown and compiled HTML file describing your analysis. Please constrain the text of the writeup to < 2000 words and the number of figures to be less than 5. It will make it easier for the graders if you submit a repo with a gh-pages branch so the HTML page can be viewed online (and you always want to make it easy on graders :-).
#2. You should also apply your machine learning algorithm to the 20 test cases available in the test data above. Please submit your predictions in appropriate format to the programming assignment for automated grading. See the programming assignment for additional details. 
#h the average of each variable for each activity and each subject.

# Load necessary libraries:
library(caret)
library(randomForest)
library(rpart) 		# For regression trees and partition
library(rpart.plot) 	# For plotting Decision Tree 
# setting the overall seed for reproduceability
set.seed(1234)
# Create project data directory, if none exists.
if(!file.exists("./pData")){dir.create("./pData")}
# set urls
dTrainUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
dTestUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# load files:
pmlTrainDs <- read.csv(url(dTrainUrl), na.strings=c("NA","#DIV/0!", ""))
pmlTestDs <- read.csv(url(dTestUrl))
 
# Check the structure of the data sets:
str(pmlTrainDs)
str(pmlTestDs)
# Clean out all NA variables
pmlTrainDs <- pmlTrainDs[,colSums(is.na(pmlTrainDs)) == 0]
pmlTestDs <- pmlTestDs[,colSums(is.na(pmlTestDs)) == 0]
# Clean out columns 1-7 as irrelevant to this analysis
pmlTrainDs <- pmlTrainDs[,-c(1:7)]
pmlTestDs <- pmlTestDs[,-c(1:7)]
str(pmlTrainDs)
str(pmlTestDs)
# Divide training data into a subTrain and SubTest random samples of 70:30 size ratios:
subtrainChoice <- createDataPartition(y=pmlTrainDs$classe, p=0.70, list=FALSE)
pmlSubtrainDs <- pmlTrainDs[subtrainChoice,]
pmlSubtestDs <- pmlTrainDs[-subtrainChoice,]
str(pmlTrainDs)
str(pmlTestDs)
# Model first based on Decision Tree Classification:
dcPmlModel <- rpart(classe ~ ., data=pmlSubtrainDs, method="class")
rpart.plot(dcPmlModel, main="PML Classification Tree", extra=107)	# Plot the tree
# Predict the subtest data set based on this decision tree:
dcPmlPrediction <- predict(dcPmlModel, pmlSubtestDs, type = "class")
# Get the confusion matrix for decision tree:
dcPmlCm <- confusionMatrix(dcPmlPrediction, pmlSubtestDs$classe)
# Now let's look at decision forests for more accuracy:
dfPmlModel <- randomForest(classe ~. , data=pmlSubtrainDs, method="class")
# Predict the subtest data set based on this decision forest:
dfPmlPredition <- predict(dfPmlModel, pmlSubtestDs, type = "class")
# Get the confusion matrix for decision tree:
confusionMatrix(dfPmlPredition, pmlSubtestDs$classe)
# Apply the better accuracy decision forest to the test data set:
# predict outcome levels on the original Testing data set using Random Forest algorithm
pmlPredict <- predict(dfPmlModel, pmlTestDs, type="class")

# Save results:
# Write files for submission
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pmlPredict)