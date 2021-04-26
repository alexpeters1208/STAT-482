############### Loading in Libraries and Data ######################

#loading in libraries
library(MASS)
library(arm)
library(caret)
library(xgboost)

#loading in data
data = read.csv("football.csv")
# remove 2020
data = data[data$Year != 2020,]
# doing variable type conversions
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)
data$Conference = as.numeric(factor(data$Conference))

####################### creating xgboost Model ################################

# Training the model
### Parameters
#5, .05, 0, 1; .9956913
xgb_params <- list(
  booster = "gbtree", 
  objective = "binary:logistic",
  max_depth = 5,
  eta = 0.05,
  gamma = 0,
  min_child_weight = 1
)

# create xgboost function
exec_xgb <- function(n) {
  repeat {
    train.index = as.numeric(unlist(createDataPartition(data$Selected, p=.75))) #75/25 dataset split
    train.data = as.matrix(newdata[train.index,])
    train.label = label[train.index]
    test.data = as.matrix(newdata[-train.index,])
    test.label = label[-train.index]
    
    ### xgb.DMatrix
    #converting into xgb matrix for training
    xgb.train = xgb.DMatrix(data = train.data, label = train.label)
    xgb.test = xgb.DMatrix(data = test.data, label = test.label)
    
    ### Fitting the Model
    #fitting model
    xgb.fit <- xgb.train(
      params = xgb_params,
      data = xgb.train,
      nrounds = 1500)
    
    xgb.pred <- predict(xgb.fit, test.data, reshape = T) #predicting on set aside train data
    xgb.pred <- as.data.frame(xgb.pred)
    pred_real <- cbind(xgb.pred, test.label)
    
    pred_real$xgb.pred = as.numeric(pred_real$xgb.pred > .5)
    confusion = table(pred_real)
    
    if(! is.na(confusion[3] + confusion[4])) {
      break
    }
  }
  # return confusion matrix in form TN, FP, FN, TP
  return(as.vector(confusion))
}

################## executing xgboost #####################

# creating label for xgboost
label <- data$Selected
newdata = data[!(names(data) %in% c("Team", "Year", "Games", "Selected"))] #removing columns

# build n xgboost models and compute their TPR reps times
n=100
reps=2
# create vector for overall true positive rates
TPR <- c()
# create vector for overall true negative rates
TNR <- c()

# train reps*n xgboost models
for (j in 1:reps) {
  # execute n xgboost models
  test = sapply(1:n, exec_xgb)
  # add up true positives and false negatives from each of n models
  results = apply(test, 1, sum)
  # compute overall true positive and append to TPR
  TPR = cbind(TPR, results[4] / (results[3] + results[4]))
  TNR = cbind(TNR, results[1] / (results[1] + results[2]))
}

# display true positive rates
TPR
# display true negative rates
TNR

# create histogram of true postive rates
#hist(TPR,
#     main = "1. Sampling Distribution of TPR (n=100)",
#     xlab = "True Positive Rate")
#hist(TNR,
#     main = "1. Sampling Distribution of TNR (n=50)",
#     xlab = "True Negative Rate")

write.csv(TPR, "xgb_tpr.csv")
write.csv(TNR, "xgb_tnr.csv")
