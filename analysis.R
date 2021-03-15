#loading in libraries
library(ggplot2)
library(MASS)
library(corrplot)
library(arm)

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


removed = c("Team", "Year", "X3rdDownConvPct", "X3rdDownConvPctDef", "OppFDPerGame",
            "RushYdsPerRushDef", "RushYdsPerGameDef", "PPGDef", "PPG", "YdsPerGame",
            "YdsPerGameDef", "PassYdsPerAtt", "PassYdsPerAttDef", "RushYdsPerRush",
            "Avg.Point.Differential", "PenPerGame", "OppRZScorePct", "RZScorePct",
            "SacksAllowed", "Sacks", "WinPct")

removed = c("Team", "Year", "Avg.Point.Differential", "Games")

newdata = data[,! names(data) %in% removed]

corrplot(cor(transform(newdata,Conference = as.numeric(Conference)),
             method="spearman"), type='lower', tl.cex=.5, tl.srt=45, tl.col="black")
title(main="Overall Correlation", adj=1)

newmodel = bayesglm(Selected ~ ., data=newdata, family=binomial)
newfit = stepAIC(newmodel, trace=FALSE, direction="backward")
summary(newfit)