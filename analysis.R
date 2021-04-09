############### Loading in Libraries and Data ######################

#loading in libraries
library(ggplot2)
<<<<<<< HEAD
library(MASS)
library(corrplot)
library(arm)
library(xgboost)
=======
library(corrplot)
library(car)
library(MASS)
library(arm)
>>>>>>> d3115a2761dba4762ef958e54798a7a85d45c7ad

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)

<<<<<<< HEAD
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

=======
# More histograms
# Explorartory Histograms
ggplot(data, aes(x = Games, fill = Year)) + geom_histogram(binwidth = 1) + 
  ggtitle("Histogram of Games Played")
ggplot(data, aes(x = X3rdDownConvPct, fill = Conference)) + geom_histogram(binwidth = .02) +
  ggtitle("Histogram of 3rd Down Conversion %") 
ggplot(data, aes(x = X3rdDownConvPctDef, fill = Conference)) + geom_histogram(binwidth = .02) +
  ggtitle("Histogram of Defensive 3rd Down Conversion %") 
ggplot(data, aes(x = X4thDownConvPct, fill = Conference)) + geom_histogram(binwidth = .05) +
  ggtitle("Histogram of 4th Down Conversion %") 
ggplot(data, aes(x = X4thDownConvPct, fill = Conference)) + geom_histogram(binwidth = .05) +
  ggtitle("Histogram of Defensive 4th Down Conversion %") 
ggplot(data, aes(x = CompPct, fill = Conference)) + geom_histogram(binwidth = .02) +
  ggtitle("Histogram of Completion %") 
ggplot(data, aes(x = PenPerGame, fill = Conference)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Penalties per Game") 
ggplot(data, aes(x = PenYardsPerGame, fill = Conference)) + geom_histogram(binwidth = 5) +
  ggtitle("Histogram of Penalty Yards per Game") 
ggplot(data, aes(x = OppFDPerGame, fill = Conference)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Opponents First Downs per Game") 
ggplot(data, aes(x = FDPerGame, fill = Conference)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram of First Downs per Game") 
ggplot(data, aes(x = NetYdsPunt, fill = Conference)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Net Yards per Punt") 
ggplot(data, aes(x = PassYdsPerAtt, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Pass Yards per Attempt") 
ggplot(data, aes(x = PassYdsPerComp, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Pass Yards per Completion") 
ggplot(data, aes(x = PassYdsPerGame, fill = Conference)) + geom_histogram(binwidth = 30) +
  ggtitle("Histogram of Pass Yards per Game") 
ggplot(data, aes(x = PassYdsPerAtt, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Defensive Pass Yards Allowed per Attempt") 
ggplot(data, aes(x = PassYdsPerCompDef, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Defensive Pass Yards Allowed per Completion") 
ggplot(data, aes(x = PassYdsPerGameDef, fill = Conference)) + geom_histogram(binwidth = 25) +
  ggtitle("Histogram of Defensive Pass Yards Allowed per Game") 
ggplot(data, aes(x = AvgPuntReturnYds, fill = Conference)) + geom_histogram(binwidth = 2) +
  ggtitle("Histogram of Average Punt Return Yards") 
ggplot(data, aes(x = OppRZScorePct, fill = Conference)) + geom_histogram(binwidth = .02) +
  ggtitle("Histogram of Oppenent Red Zone Score %")
ggplot(data, aes(x = OppPointsPerRZAtt, fill = Conference)) + geom_histogram(binwidth = .2) +
  ggtitle("Histogram of Oppenent Red Zone Points per Attempt")
ggplot(data, aes(x = RZScorePct, fill = Conference)) + geom_histogram(binwidth = .02) +
  ggtitle("Histogram of Red Zone Score %")
ggplot(data, aes(x = PointsPerRZAtt, fill = Conference)) + geom_histogram(binwidth = .2) +
  ggtitle("Histogram of Red Zone Points per Attempt")
ggplot(data, aes(x = RushYdsPerRushDef, fill = Conference)) + geom_histogram(binwidth = .3) +
  ggtitle("Histogram of Defensive Rushing Yards per Rush")
ggplot(data, aes(x = RushYdsPerRush, fill = Conference)) + geom_histogram(binwidth = .3) +
  ggtitle("Histogram of Rushing Yards per Rush")
ggplot(data, aes(x = RushYdsPerGame, fill = Conference)) + geom_histogram(binwidth = 20) +
  ggtitle("Histogram of Defensive Rushing Yards per Game")
ggplot(data, aes(x = SacksAllowed, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Sacks Allowed per Game")
ggplot(data, aes(x = PPGDef, fill = Conference)) + geom_histogram(binwidth = 2.5) +
  ggtitle("Histogram of Defensive Points Allowed per Game")
ggplot(data, aes(x = PPG, fill = Conference)) + geom_histogram(binwidth = 2.5) +
  ggtitle("Histogram of Points per Game")
ggplot(data, aes(x = TFLAllowed, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Tackles for Loss Allowed per Game")
ggplot(data, aes(x = Sacks, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Sacks per Game")
ggplot(data, aes(x = TFLPG, fill = Conference)) + geom_histogram(binwidth = .5) +
  ggtitle("Histogram of Tackles for Loss per Game")
ggplot(data, aes(x = AvgTOP, fill = Conference)) + geom_histogram(binwidth = 1) +
  ggtitle("Histogram of Average Time of Possesion")
ggplot(data, aes(x = YdsPerPlayDef, fill = Conference)) + geom_histogram(binwidth = .25) +
  ggtitle("Histogram of Average Defensive Yards Allowed per Play")
ggplot(data, aes(x = YdsPerGameDef, fill = Conference)) + geom_histogram(binwidth = 20) +
  ggtitle("Histogram of Average Defensive Yards Allowed per Game")
ggplot(data, aes(x = YdsPerPlay, fill = Conference)) + geom_histogram(binwidth = .25) +
  ggtitle("Histogram of Average Yards per Play")
ggplot(data, aes(x = YdsPerGame, fill = Conference)) + geom_histogram(binwidth = 20) +
  ggtitle("Histogram of Average Yards per Game")
ggplot(data, aes(x = AvgTurnoverMargin, fill = Conference)) + geom_histogram(binwidth = .2) +
  ggtitle("Histogram of Average Turnover Margin")
ggplot(data, aes(x = WinPct, fill = Conference)) + geom_histogram(binwidth = .1) +
  ggtitle("Histogram of Win %")
ggplot(data, aes(x = Win...vs.Rank, fill = Conference)) + geom_histogram(binwidth = .1) +
  ggtitle("Histogram of Win % vs. Ranked Opponents")
ggplot(data, aes(x = Avg.Point.Differential, fill = Conference)) + geom_histogram(binwidth = 5) +
  ggtitle("Histogram of Average Point Differential")

data = data[data$Year != 2020,]

############ Correlation plots #############

# fbs independent
corrplot(cor(data[data$Conference=="FBS Independent",! names(data) %in% c("Team","Conference","Year")],
             method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
title(main="Correlation for Notre Dame", adj=1)

# all but fbs independent
corrplot(cor(transform(data,Conference = as.numeric(Conference))[data$Conference!="FBS Independent",! names(data) %in% c("Team","Year")],
             method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
title(main="Correlation without Notre Dame", adj=1)

# include fbs independent
corrplot(cor(transform(data,Conference = as.numeric(Conference))[,! names(data) %in% c("Team","Year")],
             method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
title(main="Overall Correlation", adj=1)

################## removing multicollinearity ##################

red.data = data[, ! names(data) %in% c("Team","Games","Year","Avg.Point.Differential")]
red.data$Conf.Champ = abs(red.data$Conf.Champ)

model = glm(Selected ~., data=red.data, family="binomial")
ld.vars = attributes(alias(model)$Complete)$dimnames[[1]]
vifs = vif(model)

test = vifs[,3]
sort(test, decreasing=TRUE)[1]

curr_vif = as.numeric(sort(test, decreasing=TRUE)[1])
curr_name = names(sort(test, decreasing=TRUE)[1])
while (curr_vif > 5) {
  red.data = red.data[, ! names(red.data) %in% curr_name]
  newmodel = glm(Selected ~., data=red.data, family="binomial")
  newvif = vif(newmodel)
  curr_vif = as.numeric(sort(newvif[,3], decreasing=TRUE)[1])
  curr_name = names(sort(newvif[,3], decreasing=TRUE)[1])
}

red.model = glm(Selected ~., data=red.data, family="binomial")
summary(red.model)

########## with bayes glm ############

model2 = bayesglm(Selected ~., data=red.data, family="binomial")
summary(red.model)

red.model2 = stepAIC(model2, trace=FALSE, direction="backward")
summary(red.model2)

red.data2 = red.data[,names(red.data) %in% names(red.model2$coefficients)[-1]]
>>>>>>> d3115a2761dba4762ef958e54798a7a85d45c7ad

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
