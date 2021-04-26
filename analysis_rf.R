############### Loading in Libraries and Data ######################

#loading in libraries
library(MASS)
library(arm)
library(caret)
library(randomForest)

#loading in data
data = read.csv("football.csv")
# remove 2020
data = data[data$Year != 2020,]
# doing variable type conversions
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)
data$Conference = as.numeric(factor(data$Conference))

####################### creating random forest Model ################################

# create rf function
exec_rf <- function(n) {
  repeat {
    train.index = as.numeric(unlist(createDataPartition(data$Selected, p=.75))) #75/25 dataset split
    train.data = as.matrix(newdata[train.index,])
    train.label = label[train.index]
    test.data = as.matrix(newdata[-train.index,])
    test.label = label[-train.index]
    
    ### Fitting the Model
    #fitting model
    rf.fit <- randomForest(
      x=train.data,
      y=as.factor(train.label),
      mtry=15,
      ntree=1500
    )
    
    rf.pred <- predict(rf.fit, test.data, reshape = T) #predicting on set aside train data
    rf.pred <- as.data.frame(rf.pred)
    pred_real <- cbind(rf.pred, test.label)
    
    confusion = table(pred_real)

    if(! is.na(confusion[3] + confusion[4])) {
      break
    }
  }
  # return confusion matrix in form TN, FP, FN, TP
  return(as.vector(confusion))
}

################## executing random forest #####################

# creating label for rf
label <- data$Selected
newdata = data[!(names(data) %in% c("Team", "Year", "Games", "Selected"))] #removing columns

# build n rf models and compute their TPR reps times
n=100
reps=2
# create vector for overall true positive rates
TPR <- c()
# create vector for overall true negative rates
TNR <- c()

# train reps*n rf models
for (j in 1:reps) {
  # execute n rf models
  test = sapply(1:n, exec_rf)
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

write.csv(TPR, "rf_tpr.csv")
write.csv(TNR, "rf_tnr.csv")
