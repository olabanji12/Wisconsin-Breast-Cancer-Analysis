library(caTools)
library(Amelia)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ISLR)
library(corrplot)
library(rpart)
library(rpart.plot)
# install.packages("caret")
library(caret)
library(class)

##Import the dataset
  #setwd(**Your directory here**)
##load the dataset into the 
ds <- read.csv('FNA_cancer.csv')

#reference of original data
data <- c(ds)
head(ds)

# data summary
summary(ds)
str(ds)
head(ds,20)

# check for missing data
missmap(ds, main = "Missing Map", col =c('yellow', 'black'))

# drop X column with missing data
ds$X <- NULL

# confirm no missing data
missmap(ds, main = "Missing Map", col =c('yellow', 'black'))

# removes the ID column; not necessary for analysis
ds <- select(ds, -id)

#factor the diagnosis column
ds$diagnosis <- factor(ds$diagnosis)

# create a correlation matrix
cor_matrix <- cor(ds[,-1])

# creates the correlation plot
corrplot(cor_matrix, method = "number", number.cex=0.5, tl.cex = 0.9, title = 'Correlation Plot', add = FALSE)

# correlation graphs
ggplot(ds, aes(x = diagnosis, y = radius_mean, fill = diagnosis)) +
  geom_boxplot() +
  labs(title = "Comparison of Mean Radius for Malignant and Benign Tumors",
       x = "Diagnosis",
       y = "Mean Radius") +
  theme_minimal()


#distribution of radium mean by diagnosis 

ggplot(ds, aes(x = radius_mean, fill = diagnosis)) +
  geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Radius Mean by Diagnosis",
       x = "Radius Mean",
       y = "Frequency") +
  facet_wrap(~diagnosis)

# Density plots for radius_mean by diagnosis

ggplot(ds, aes(x = radius_mean, fill = diagnosis)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density of Radius Mean by Diagnosis",
       x = "Radius Mean",
       y = "Density") +
  facet_wrap(~diagnosis)

# Scatter Plot of Concave Points Worst vs. Perimeter Worst
  # creates a liner model to get slope and coefficient
lm_model <- lm(concave.points_worst ~ perimeter_worst, data = ds)    
coeff<-coefficients(lm_model)           
intercept<-coeff[1] 
slope<- coeff[2] 

ggplot(data = ds, 
       aes(x = perimeter_worst, y = concave.points_worst, color = diagnosis)) +
  geom_point() +
  labs(
    x = "Perimeter Max",
    y = "Concave Points Worst",
    title = "Concave Points Worst vs. Perimeter Worst with Correlation 0.82"
  ) +
  geom_abline(intercept = intercept, slope = slope, size=1)

# Developing Models
  # split data into train and test for the Decision Tree model.
set.seed(101)
    #70% is train and 30% is test
split = sample.split(ds$diagnosis, SplitRatio = 0.70)
train = subset(ds, split == TRUE)
test = subset(ds, split == FALSE)
  
# Decision Trees
decision_tree_model <- rpart(diagnosis ~ ., data = train, method = "class")

  
  # Visualize the Decision Tree
plot.new()
rpart.plot(decision_tree_model, box.palette="RdBu", shadow.col="gray", nn=TRUE)


  # Make Prediction on Testing Set Using Decision Tree Model
predictions <- predict(decision_tree_model, test, type = "class")


  # Evaluate the Model
conf_matrix <- confusionMatrix(predictions, test$diagnosis)
conf_matrix
  # Calculate Accuracy
treeAccuracy <- conf_matrix$overall["Accuracy"]
  # Calculate Precision
treePrec <- conf_matrix$byClass["Pos Pred Value"]
  # Calculate Recall
treeRecall <- conf_matrix$byClass["Sensitivity"]
   # rounds Accuracy, Precision, and Recall values
model_accuracy <- paste("Accuracy: ", signif(treeAccuracy, 3) * 100)
model_precision <- paste("Precision: ", signif(treePrec, 3) * 100)
model_recall <- paste("Recall: ", signif(treeRecall, 3) * 100)
print(model_accuracy)
print(model_precision)
print(model_recall)


# KNN Model
  # Split and prepare data for KNN model
predicted.diagnosis <- select(ds, -diagnosis)
target.diagnosis <- ds$diagnosis

set.seed(123) #for reproducibility
trainIndex <- createDataPartition(target.diagnosis, p = 0.7, list = FALSE)
trainData <- predicted.diagnosis[trainIndex, ]
testData <- predicted.diagnosis[-trainIndex, ]
trainDiagnosis <- target.diagnosis[trainIndex]
testDiagnosis <- target.diagnosis[-trainIndex]

#Trying out different values of k in KNN
  #This is for k = 1
predicted_diagnosis_k1 <- knn(trainData, testData, trainDiagnosis, k = 1)
misclasserr_k1 <- mean(testDiagnosis != predicted_diagnosis_k1)
print(paste("Misclassification Error for k = 1:", misclasserr_k1))
conf_matrix_k1 <- table(Actual = testDiagnosis, Predicted = predicted_diagnosis_k1)


  # Display the confusion matrix for k = 1
print("Confusion Matrix for k = 1:")
print(conf_matrix_k1)

  #Calculate Accuracy, precision, and recall
accuracy_k1 <- sum(diag(conf_matrix_k1)) / sum(conf_matrix_k1)
precision_k1 <- conf_matrix_k1[2, 2] / sum(conf_matrix_k1[, 2])
recall_k1 <- conf_matrix_k1[2, 2] / sum(conf_matrix_k1[2, ])

cat("Accuracy for k = 1:", accuracy_k1, "\n")
cat("Precision for k = 1:", precision_k1, "\n")
cat("Recall for k = 1:", recall_k1, "\n")

  #Now, here is k=3
predicted_diagnosis_k3 <- knn(trainData, testData, trainDiagnosis, k = 3)
misclasserr_k3 <- mean(testDiagnosis != predicted_diagnosis_k3)
print(paste("Misclassification Error for k = 3:", misclasserr_k3))
conf_matrix_k3 <- table(Actual = testDiagnosis, Predicted = predicted_diagnosis_k3)

# Display the confusion matrix for k = 3
print("Confusion Matrix for k = 3:")
print(conf_matrix_k3)

  #Calculate Accuracy, precision, and recall
accuracy_k3 <- sum(diag(conf_matrix_k3)) / sum(conf_matrix_k3)
precision_k3 <- conf_matrix_k3[2, 2] / sum(conf_matrix_k3[, 2])
recall_k3 <- conf_matrix_k3[2, 2] / sum(conf_matrix_k3[2, ])

cat("Accuracy for k = 3:", accuracy_k3, "\n")
cat("Precision for k = 3:", precision_k3, "\n")
cat("Recall for k = 3:", recall_k3, "\n")

  #And here is k = 5
predicted_diagnosis_k5 <- knn(trainData, testData, trainDiagnosis, k = 5)
misclasserr_k5 <- mean(testDiagnosis != predicted_diagnosis_k5)
print(paste("Misclassification Error for k = 5:", misclasserr_k5))
conf_matrix_k5 <- table(Actual = testDiagnosis, Predicted = predicted_diagnosis_k5)

# Display the confusion matrix for k = 5
print("Confusion Matrix for k = 5:")
print(conf_matrix_k5)

  #Calculate Accuracy, precision, and recall
accuracy_k5 <- sum(diag(conf_matrix_k5)) / sum(conf_matrix_k5)
precision_k5 <- conf_matrix_k5[2, 2] / sum(conf_matrix_k5[, 2])
recall_k5 <- conf_matrix_k5[2, 2] / sum(conf_matrix_k5[2, ])

cat("Accuracy for k = 5:", accuracy_k5, "\n")
cat("Precision for k = 5:", precision_k5, "\n")
cat("Recall for k = 5:", recall_k5, "\n")




