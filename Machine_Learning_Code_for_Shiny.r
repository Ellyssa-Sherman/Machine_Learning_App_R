# To add caret package with needed dependencies. NOTE: This takes forever and I do not suggest running the code this way.
install.packages("caret", dependencies=c("Depends", "Suggests"))
library(caret)

# library(caret)
# install.packages('ellipse')
# install.packages('e1071', dependencies=TRUE)
# library(ellipse)
# library(e1071)
# install.packages("kernlab")
# library(kernlab)

# # Attach the iris dataset to the environment
# data(iris)
# # Rename the dataset
# dataset <- iris

# Loading the data set from a .csv file
# Define the filename
filename <- "iris.csv"
# load the CSV file from the local directory
dataset <- read.csv(filename, header=FALSE)
# Set the column names in the dataset
colnames(dataset) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

# Create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
# Select 20% of the data for validation
validation <- dataset[-validation_index,]
# Use the remaining 80% of data to training and testing the models
dataset <- dataset[validation_index,]

# Dimensions of dataset
dim(dataset)

# Convert character column to factor column
    # dataset$Species=as.factor (dataset$Species)

# List types for each attribute
sapply(dataset, class)
# Take a peek at the first 5 rows of the data
head(dataset)
# List the levels for the class
levels(dataset$Species)

# Summarize the class distribution
percentage <- prop.table(table(dataset$Species)) * 100
cbind(freq=table(dataset$Species), percentage=percentage)

# Summarize attribute distributions
summary(dataset)

# Split input (Sepal and Petal Dimensions) and output (Species)
x <- dataset[,1:4]
y <- dataset[,5]

# Boxplot for each attribute on one image
par(mfrow=c(1,4))
  for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}

# Barplot for class breakdown
plot(y)

# Scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# Box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# Density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=dataset, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=dataset, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=dataset, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=dataset, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)

# Summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# Compare accuracy of models
dotplot(results)

# Summarize Best Model
print(fit.lda)

# Estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, validation)
confusionMatrix(predictions, validation$Species)
