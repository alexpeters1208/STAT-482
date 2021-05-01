############### Loading in Libraries and Data ######################

#loading in libraries
library(ggplot2)
library(MASS)
library(corrplot)
library(arm)
library(xgboost)

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)

###################### xgboost Model Setup #########################

train.index <- which(data$Year != 2020) #all but 2020

# Separating Response and Excluding Team and Year
label <- data$Selected #response variable
team = data$Team
removed = c("Team", "Year", "Games", "Selected")
newdata = data[,! names(data) %in% removed]


#converting conference to numbers for xgboost
newdata$Conference <- as.numeric(factor(newdata$Conference)) 

#splitting data into train and test and indicating response for evaluation
train.data <- as.matrix(newdata[train.index,])
train.label <- label[train.index]
test.data <- as.matrix(newdata[-train.index,])
test.label <- label[-train.index]

### xgb.DMatrix
#converting into xgb matrix for training
xgb.train <- xgb.DMatrix(data = train.data, label = train.label)
xgb.test <- xgb.DMatrix(data = test.data, label = test.label)

####################### xgboost Model ################################

# Training the model
### Parameters
#5, .05, 0, 1; .9956913
xgb_params <- list(
  booster = "gbtree", 
  objective = "binary:logistic",
  max_depth = 5,
  eta = 0.01,
  gamma = 0,
  min_child_weight = 1
)

### Fitting the Model
#fitting model
xgb.fit <- xgb.train(
  params = xgb_params,
  data = xgb.train,
  nrounds = 1500)

#################### xgboost Prediction and Analysis ###################
xgb.pred <- predict(xgb.fit, test.data, reshape = T) #predicting on set aside test data
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(label)
head(xgb.pred) #model predictions on set aside test data
pred_real <- cbind(team[1:65],xgb.pred, test.label) #comparing prediction to test label
colnames(pred_real) = c("Team", "Prediction", "Selected")

# Model Accuracy
sum(abs(test.label - xgb.pred[,]))/length(test.label)
plot(xgb.pred[,],test.label, xlab = "Model Prediction", ylab = "Selected")   

importance = xgb.importance(model=xgb.fit) #Most important features
importance

head(pred_real[order(pred_real$Prediction, decreasing = TRUE),], 9)
