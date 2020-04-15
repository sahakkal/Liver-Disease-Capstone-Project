# R Script for Liver Disease Prediction Project

# Introduction

# Methods

# Install packages and load libraries:
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(rminer)) install.packages("rminer", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library("readr") 
library("ggplot2")
library("dplyr")
library("caret")
library("tidyverse")
library("data.table")
library("gridExtra")
library("klaR")
library("rminer")
library("rpart")
library("randomForest")

# Load dataset:
indian_liver_patient <-
  read_csv("~/Downloads/indian_liver_patient.csv")
# or
dl <- tempfile()
download.file("https://www.kaggle.com/uciml/indian-liver-patient-records/data", dl)

# Examine the structure of the data: 
str(indian_liver_patient)
head(indian_liver_patient)
summary(indian_liver_patient)

# Remove NA entries from the dataset.
indian_liver_patient <- indian_liver_patient %>%
  filter(!is.na(Albumin_and_Globulin_Ratio))

# Change "Gender" and "Dataset" variables to factors. 
indian_liver_patient$Gender <- as.factor(indian_liver_patient$Gender) 
indian_liver_patient$Dataset <- as.factor(indian_liver_patient$Dataset)

# Procedure

# 1. Exploratory Data Analysis

# Count the number of males and females in the dataset.
sum(indian_liver_patient$Gender == "Male")
sum(indian_liver_patient$Gender == "Female")

# Plot the Gender Counts.
ggplot(indian_liver_patient, aes(Gender)) + 
  geom_bar(aes(fill = Gender)) + 
  ggtitle("Gender Counts") + 
  theme(legend.position="none")

# Plot the age distribution across genders. 
ggplot(indian_liver_patient, aes(x=Age, color=Gender)) + 
  geom_density(alpha=.5) +
  ggtitle("Age Distribution Across Genders") + 
  theme(legend.position="none")

# Count the number of positive and negative liver disease cases in the dataset.
sum(indian_liver_patient$Dataset == "1")
sum(indian_liver_patient$Dataset == "2")

# Plot the outcome distribution ("1" = liver disease or "2" = no liver disease).
ggplot(indian_liver_patient, aes(Dataset)) +
  geom_bar(stat = "count", aes(fill = Dataset)) + 
  ggtitle("Distribution of Outcomes") +
  theme(legend.position = "none")

# Plot the outcome distribution ("1" = liver disease or "2" = no liver disease) across age.
ggplot(indian_liver_patient, aes(x = Dataset, y = Age, group = Dataset)) +
  geom_boxplot(color = "blue", fill = "red") +
  ylab("Age") +
  ggtitle("Outcome Distribution Across Age") 

# Plot the outcome distribution ("1" = liver disease or "2" = no liver disease) across age and genders.
indian_liver_patient %>% group_by(Dataset, Gender) %>% 
  ggplot(aes(x = Dataset, y = Age)) +
  geom_boxplot(aes(color = Gender)) +
  ylab("Age") +
  ggtitle("Outcome Distribution Across Age and Genders") 

# Plot the frequency distributions (histograms) of the remaining predictors.
ph1 <- ggplot(indian_liver_patient, aes(x=Total_Bilirubin)) +
  geom_histogram(binwidth=4, colour="black", alpha=.5)

ph2 <- ggplot(indian_liver_patient, aes(x=Direct_Bilirubin)) +
  geom_histogram(binwidth=1, colour="black", alpha=.5)

ph3 <- ggplot(indian_liver_patient, aes(x=Alkaline_Phosphotase)) +
  geom_histogram(binwidth=100, colour="black", alpha=.5)

ph4 <- ggplot(indian_liver_patient, aes(x=Alamine_Aminotransferase)) +
  geom_histogram(binwidth=100, colour="black", alpha=.5)

ph5 <- ggplot(indian_liver_patient, aes(x=Aspartate_Aminotransferase)) +
  geom_histogram(binwidth=240, colour="black", alpha=.5)

ph6 <- ggplot(indian_liver_patient, aes(x=Total_Protiens)) +
  geom_histogram(binwidth=1/3, colour="black", alpha=.5)

ph7 <- ggplot(indian_liver_patient, aes(x=Albumin)) +
  geom_histogram(binwidth=1/4, colour="black", alpha=.5)

ph8 <- ggplot(indian_liver_patient, aes(x=Albumin_and_Globulin_Ratio)) +
  geom_histogram(binwidth=1/7, colour="black", alpha=.5)

grid.arrange(ph1, ph2, ph3, ph4, ph5, ph6, ph7, ph8, ncol=3)

# Plot the outcome distributions (boxplots) of the remaining predictors. 
pb1 <- ggplot(indian_liver_patient, aes(Dataset, Total_Bilirubin)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb2 <- ggplot(indian_liver_patient, aes(Dataset, Direct_Bilirubin)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb3 <- ggplot(indian_liver_patient, aes(Dataset, Alkaline_Phosphotase)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb4 <- ggplot(indian_liver_patient, aes(Dataset, Alamine_Aminotransferase)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb5 <- ggplot(indian_liver_patient, aes(Dataset, Aspartate_Aminotransferase)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb6 <- ggplot(indian_liver_patient, aes(Dataset, Total_Protiens)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb7 <- ggplot(indian_liver_patient, aes(Dataset, Albumin)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

pb8 <- ggplot(indian_liver_patient, aes(Dataset, Albumin_and_Globulin_Ratio)) + geom_boxplot(aes(fill = Dataset), alpha = 2/3) + stat_summary(fun=mean, geom="point", shape=3, size=4) + theme(legend.position = "none")

grid.arrange(pb1, pb2, pb3, pb4, pb5, pb6, pb7, pb8, ncol=3)

# 2. Model Development

# Split the dataset into "modeling" and "validation" subsets.
dat <- as.data.frame(indian_liver_patient)
dat <-  na.omit(dat)
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = dat$Dataset, times = 1, p = 0.1, list = FALSE)
modeling <- dat[-test_index,]
validation <- dat[test_index,]

# Divide the modeling data into train and test sets in order to explore different training models.
set.seed(1, sample.kind="Rounding")
test_index2 <- createDataPartition(y = modeling$Dataset, times = 1, p = 0.1, list = FALSE)
train_set <- modeling[-test_index2,]
test_set <- modeling[test_index2,]

# Logistic Regression Model
modelFit_LR = train(Dataset ~ ., data = train_set, method = "glm", family = "binomial", na.action=na.omit)
modelFit_LR

# View the most important predictors.
LR_imp = varImp(modelFit_LR, scale = FALSE)
LR_imp

# Plot the most important predictors.
plot(LR_imp)

# Use the LR model to make predictions on the test_set.
pred_LR = predict(modelFit_LR, test_set)
confusionMatrix(pred_LR, test_set$Dataset)

# Random Forest Model
modelFit_RF = train(Dataset ~ ., method = "rf", data = train_set, prox = TRUE, na.action=na.omit)
modelFit_RF

# Plot the model to determine the optimal mtry.
plot(modelFit_RF)

# View the most important predictors.
RF_imp = varImp(modelFit_RF, scale = FALSE)
RF_imp

# Plot the most important predictors.
plot(RF_imp)

# Use the RF model to make predictions on the test_set.
pred_RF = predict(modelFit_RF, test_set)
confusionMatrix(pred_RF, test_set$Dataset)

# Naive Bayes Model
modelFit_nb = train(Dataset ~., "nb", data = train_set, trControl = trainControl(method ="cv", number = 10))
modelFit_nb

# Use the NB model to make predictions on the test_set.
pred_nb <- predict(modelFit_nb, test_set)
confusionMatrix(pred_nb, test_set$Dataset)

# Linear Discriminant Analysis Model
modelFit_lda <- train(Dataset ~ ., data = train_set, method = "lda", na.action=na.omit)
modelFit_lda

# Use the LDA model to make predictions on the test_set.
pred_lda <- predict(modelFit_lda, test_set)
confusionMatrix(pred_lda, test_set$Dataset)

# K-Nearest Neighbors Model
modelFit_KNN <- train(
  Dataset ~ ., data = train_set, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"), 
  tuneLength = 20
)
modelFit_KNN

# Plot the model to determine the optimal k.
plot(modelFit_KNN)

# Use the KNN model to make predictions on the test_set.
pred_KNN <- predict(modelFit_KNN, test_set)
confusionMatrix(pred_KNN, test_set$Dataset)

# Results

# Summary Table of the accuracies of the 5 models used for training.
summaryTable <- data.frame("Model" = c("Naive Bayes", "Linear Discriminant Analysis", "K-Nearest Neighbors", "Logistic Regression", "Random Forest"),
                           "Accuracy" = c(0.6981, 0.7358, 0.7547, 0.7736, 0.7925))
summaryTable

# Now train the best-performing, Final Model (RF) on the modeling set.
FinalmodelFit_RF = train(Dataset ~ ., method = "rf", data = modeling, prox = TRUE, na.action=na.omit)
FinalmodelFit_RF

# Plot the model to determine the optimal mtry.
plot(FinalmodelFit_RF)

# View the most important predictors.
FinalmodelRF_imp = varImp(FinalmodelFit_RF, scale = FALSE)
FinalmodelRF_imp

# Plot the most important predictors.
plot(FinalmodelRF_imp)

# Finally, use the Final Model to make predictions on the validation set.
Finalmodelpred_RF = predict(FinalmodelFit_RF, validation)
confusionMatrix(Finalmodelpred_RF, validation$Dataset)

# Conclusion

# References
