#loading in libraries
library(ggplot2)
library(MASS)
library(corrplot)
library(arm)
library(xgboost)
library(Matrix)

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)

# Explorartory Histograms
ggplot(data, aes(x = Games)) + geom_histogram(binwidth = 1, fill = 4) + 
  ggtitle("Histogram of Games Played") + facet_grid(Year ~ .)
ggplot(data, aes(x = Avg.Point.Differential, fill = Conference)) + geom_histogram(binwidth = 5) +
  ggtitle("Histogram of Average Point Differential")

ggplot(data, aes(x = Avg.Point.Differential)) + 
  geom_histogram(binwidth = 5, fill = "red", color = "white") +
  ggtitle("Histogram of Average Point Differential") + 
  facet_grid(Conference ~ ., scales = "free")

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


# removed = c("Team", "Year", "X3rdDownConvPct", "X3rdDownConvPctDef", "OppFDPerGame",
#            "RushYdsPerRushDef", "RushYdsPerGameDef", "PPGDef", "PPG", "YdsPerGame",
#            "YdsPerGameDef", "PassYdsPerAtt", "PassYdsPerAttDef", "RushYdsPerRush",
#            "Avg.Point.Differential", "PenPerGame", "OppRZScorePct", "RZScorePct",
#            "SacksAllowed", "Sacks", "WinPct")

# Separating Response and Excluding Team and Year
label <- data$Selected #response variable
removed = c("Team", "Year", "Games", "Selected")
newdata = data[,! names(data) %in% removed]


#xgboost
n <- nrow(data) #number of rows of train
train.index <- sample(n, floor(0.8 * n)) #80/20 dataset spli

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

# Training the model
xgb_params <- list(
  booster = "gbtree", 
  objective = "binary:logistic"
  # max_depth = 5,
  # eta = 0.3,
  # gamma = 3,
  # min_child_weight = 20
)

### Fitting the Model
#fitting model
xgb.fit <- xgb.train(
  params = xgb_params,
  data = xgb.train,
  nrounds = 100)

# Prediction
xgb.pred <- predict(xgb.fit, test.data, reshape = T) #predicting on set aside train data
xgb.pred <- as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(label)
head(xgb.pred) #model predictions on set aside test data

#Analyzing the Model
sum(abs(test.label - xgb.pred[,]))/length(test.label)
plot(xgb.pred[,],test.label, xlab = "Model Prediction", ylab = "Selected")   

xgb.importance(model=xgb.fit) #Most important features