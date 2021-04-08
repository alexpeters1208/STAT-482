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

# ################### Explorartory Histograms ##########################
# ggplot(data, aes(x = Games)) + geom_histogram(binwidth = 1, fill = 4) + 
#   ggtitle("Histogram of Games Played") + facet_grid(Year ~ .)
# ggplot(data, aes(x = Avg.Point.Differential, fill = Conference)) + geom_histogram(binwidth = 5) +
#   ggtitle("Histogram of Average Point Differential")
# 
# ggplot(data, aes(x = Avg.Point.Differential)) + 
#   geom_histogram(binwidth = 5, fill = "red", color = "white") +
#   ggtitle("Histogram of Average Point Differential") + 
#   facet_grid(Conference ~ ., scales = "free")

data = data[data$Year != 2020,] #removing 2020 data

############ Correlation Plots #############

# # fbs independent
# corrplot(cor(data[data$Conference=="FBS Independent",! names(data) %in% c("Team","Conference","Year")],
#              method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
# title(main="Correlation for Notre Dame", adj=1)

# # all but fbs independent
#corrplot(cor(transform(data, Conference = as.numeric(Conference))[data$Conference!="FBS Independent", ! names(data) %in% c("Team","Year")],
#              method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
# title(main="Correlation without Notre Dame", adj=1)

# # include fbs independent
# corrplot(cor(transform(data,Conference = as.numeric(Conference))[,! names(data) %in% c("Team","Year")],
#              method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
# title(main="Overall Correlation", adj=1)


# removed = c("Team", "Year", "X3rdDownConvPct", "X3rdDownConvPctDef", "OppFDPerGame",
#            "RushYdsPerRushDef", "RushYdsPerGameDef", "PPGDef", "PPG", "YdsPerGame",
#            "YdsPerGameDef", "PassYdsPerAtt", "PassYdsPerAttDef", "RushYdsPerRush",
#            "Avg.Point.Differential", "PenPerGame", "OppRZScorePct", "RZScorePct",
#            "SacksAllowed", "Sacks", "WinPct")

###################### xgboost Model Setup #########################

n <- nrow(data) #number of rows of train
train.index <- sample(n, floor(0.75 * n)) #75/25 dataset split

# Separating Response and Excluding Team and Year
label <- data$Selected #response variable
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

###################### xgboost Cross Validation ##########################

### Running Cross Validation to Find Best Set of Parameters
#running a cross validation to determine best set of parameters, note that I have run this
#multiple times with many more potential parameter combinations and 
#only selected ideal #parameters for time reasons during this run
# potential_param <- data.frame(
#   depth = numeric(),
#   eta = numeric(),
#   gamma = numeric(),
#   weight = numeric(),
#   mlogloss = numeric())
# 
# for (depth in c(3, 4, 5)) {
#   for (eta in c(0.05, 0.25)) {
#     for (gamma in c(0, 0.5, 1)) {
#       for (weight in c(0)) {
#         print(c(depth, eta, gamma, weight))
#         cv = xgb.cv(
#           data = xgb.train ,
#           params = list(objective = "binary:logistic",
#                         eval_metric = "auc",
#                         max_depth = depth,
#                         eta = eta,
#                         gamma = gamma,
#                         min_child_weight = weight
#           ),
#           nrounds = 100,
#           nfold = 3)
#         logloss = min(cv$evaluation_log[,"test_auc_mean"])
#         potential_param = rbind(potential_param, c(depth, eta, gamma, weight, logloss))
#       }
#     }
#   }
# }

# Picking best nrounds
# xgbcv <- xgb.cv(params = xgb_params,
#                 data = xgb.train,
#                 nrounds = 10000,
#                 nfold = 3,
#                 showsd = T,
#                 stratified = T,
#                 print.every.n = 1000,
#                 early.stop.round = 1000,
#                 maximize = F)

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
xgb.pred <- predict(xgb.fit, test.data, reshape = T) #predicting on set aside train data
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(label)
head(xgb.pred) #model predictions on set aside test data
pred_real <- cbind(xgb.pred, test.label) #comparing prediction to test label
head(pred_real)

# Model Accuracy
sum(abs(test.label - xgb.pred[,]))/length(test.label)
plot(xgb.pred[,],test.label, xlab = "Model Prediction", ylab = "Selected")   

importance = xgb.importance(model=xgb.fit) #Most important features
importance

################ Accuracy of Predictions > .5 ########################
err_vec = c()
good = 0
bad = 0

for (i in 1:100) {
  train.index <- sample(n, floor(0.75 * n)) #75/25 dataset split
  train.data <- as.matrix(newdata[train.index,])
  train.label <- label[train.index]
  test.data <- as.matrix(newdata[-train.index,])
  test.label <- label[-train.index]
  
  ### xgb.DMatrix
  #converting into xgb matrix for training
  xgb.train <- xgb.DMatrix(data = train.data, label = train.label)
  xgb.test <- xgb.DMatrix(data = test.data, label = test.label)
  
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
  confusion <- table(pred_real)
  error = confusion[4]/(confusion[3] + confusion[4])
  err_vec = c(err_vec, error)
  
  good = good + confusion[4]
  bad = bad + confusion[3]
}

good/(good + bad) #TPR after 100 simulations

hist(err_vec, 
     main = "True Positive Rates in xgboost Model (n=100)", 
     xlab = "True Positive Rate",
     breaks = 4)
