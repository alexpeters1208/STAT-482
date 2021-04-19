#loading in libraries
library(ggplot2)
library(randomForest)
library(caret)

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)
data$Selected = as.factor(data$Selected)
##data$Yrs.Since.Last.Selected = as.factor(data$Yrs.Since.Last.Selected

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


removed = c("Team", "Year", "X3rdDownConvPct", "X3rdDownConvPctDef", "OppFDPerGame",
            "RushYdsPerRushDef", "RushYdsPerGameDef", "PPGDef", "PPG", "YdsPerGame",
            "YdsPerGameDef", "PassYdsPerAtt", "PassYdsPerAttDef", "RushYdsPerRush",
            "Avg.Point.Differential", "PenPerGame", "OppRZScorePct", "RZScorePct",
            "SacksAllowed", "Sacks")
cameron_removed = c("Team","Year","Games")

newdata = data[,! names(data) %in% cameron_removed]

corrplot(cor(transform(newdata,Conference = as.numeric(Conference)),
             method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
title(main="Overall Correlation", adj=1)

successful_years<-0
all_but_four<-0
r<-100
for (k in 1:r) {
  new_vec<-matrix(ncol=3)
  #set.seed(k^3)
  trainIndex <- createDataPartition(newdata$Selected, p = 5/6,
                                    list = FALSE,
                                    times = 1)
  train_data <- newdata[ trainIndex,]
  test_data <- newdata[-trainIndex,]
  #train_data<-newdata[c(1:260,326:390),]
  #test_data<-newdata[261:325,]
  
  control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
  mtry <- sqrt(ncol(train_data))
  rf_random <- train(Selected~., data=train_data, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
  #print(rf_random)
  #plot(rf_random)
  
  rf_probs<-predict(rf_random, newdata = test_data, type = "prob")
  for (i in 1:65) {
    if (rf_probs[i,2] >= 0.1) {
      new_vec <- rbind(new_vec,c(as.numeric(row.names(rf_probs[i,])),rf_probs[i,]))
    }
  }
  new_vec<-new_vec[-1,]
  new_vec1<-unlist(new_vec[,1])
  new_vec2<-unlist(new_vec[,2])
  new_vec3<-unlist(new_vec[,3])
  new_vec <- matrix(c(new_vec1,new_vec2,new_vec3),ncol=3,byrow=FALSE)
  new_vec<-new_vec[order(new_vec[,3],decreasing=TRUE),]
  print(new_vec)
  for (j in 1:nrow(new_vec)) {
    row_num<-as.numeric(unlist(new_vec[j,1])) - 65
    if (data[row_num,2] == 1) {
      cat(as.character(data[row_num,1]),"was selected in",as.character(data[row_num,"Year"]),"\n")
    } else {
      cat(as.character(data[row_num,1]),"was not selected in",as.character(data[row_num,"Year"]),"\n")
    }
  }
  if (data[new_vec[1,1]-65,2] == 1 && data[new_vec[2,1]-65,2] == 1 && data[new_vec[3,1]-65,2] == 1 && data[new_vec[4,1]-65,2] == 1) {
    successful_years <- successful_years + 1
  }
  else {
    if (data[new_vec[1,1]-65,2] == 1 && data[new_vec[2,1]-65,2] == 1 && data[new_vec[3,1]-65,2] == 1 && data[new_vec[4,1]-65,2] == 0) {
      all_but_four<-all_but_four+1
    }
  }
  paste("\n")
  paste("\n")
}
correct_pct<-(successful_years+all_but_four)/r
correct_pct
