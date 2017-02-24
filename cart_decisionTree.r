
install.packages("caret")
install.packages("rpart")
install.packages("randomForest")
install.packages("gbm")
install.packages("rattle")
install.packages("pROC")

library(caret)
library(rpart)
library(randomForest)
library(gbm)
library(rattle)
library(pROC)
library(plyr)

cancer_data <- read.csv("c:/Users/Bhavana/Desktop/R/wdbc_data.csv", header=FALSE)
View(cancer_data)
#Removing first column
cancer_data <- cancer_data[,2:32]
# name the columns of data set
names(cancer_data) <-  c("diagnosis", "radius", "texture", "perimeter", "area",
                         "smoothness", "compactness", "concavity", "concave_points",
                         "symmetry", "fractal_dimension", "radius_SE", "texture_SE",
                         "perimeter_SE", "area_SE", "smoothness_SE", "compactness_SE",
                         "concavity_SE", "concave_points_SE", "symmetry_SE",
                         "fractal_dimension_SE", "radius_Worst", "texture_Worst",
                         "perimeter_Worst", "area_Worst", "smoothness_Worst",
                         "compactness_Worst","concavity_Worst","concave_points_Worst",
                         "symmetry_Worst", "fractal_dimension_Worst")
View(cancer_data)
#looking at the statistics of data
table(cancer_data$diagnosis)
summary(cancer_data)
str(cancer_data)
head(cancer_data,5)
nrow(cancer_data)
#cheking for zero's in data
malign_data <- subset(cancer_data, cancer_data$diagnosis=="M")
# use apply function to check for zero's
apply(malign_data, 2, min)
# check for near zero variave
nearZeroVar(cancer_data)

#Build Learning models by dividing data into training and test set
set.seed(1)
divide_data<- createDataPartition(cancer_data$diagnosis, p=0.7, list=FALSE)
training_data <- cancer_data[divide_data,]
test_data <- cancer_data[-divide_data,]
dim(training_data); dim(test_data)
table(training_data$diagnosis)

# CART MODEL
install.packages("e1071")
library(e1071)
set.seed(1)
cart_model <- train(diagnosis ~ ., data=training_data, method="rpart")

cart_model
cart_model$finalModel
install.packages("rpart.plot")
library(rpart.plot)
fancyRpartPlot(cart_model$finalModel, sub="")

### Testing data
predicting <- predict(cart_model, test_data)
confusionMatrix(predicting, test_data$diagnosis, positive="M")
x11()
fancyRpartPlot(cart_model$finalModel, sub="")

#Building CART model with fine tuning of cp parameter
tuning <- trainControl(method = "repeatedcv", repeats = 3)

grid <- expand.grid(cp = seq(0, 1, by = 0.01))

set.seed(1)
cart_tune_model <- train(diagnosis ~ ., data=training_data, method="rpart",
                         tuneGrid = grid, trControl = tuning)

cart_tune_model$finalModel

## cp parameter tuning 
trellis.par.set(caretTheme())
plot(cart_tune_model)

### testing the data again
predicting2 <- predict(cart_tune_model, test_data)
confusionMatrix(predicting2, test_data$diagnosis, positive="M")
###No improvement in accuracy on testing set.
### the number of observations in this dataset is rather small and differences in accuracy can simply reflect this particular split into training and testing data set.
fancyRpartPlot(cart_tune_model$finalModel, sub="")
