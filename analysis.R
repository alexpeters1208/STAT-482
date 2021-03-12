#loading in libraries
library(ggplot2)
library(car)

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)

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
ggplot(data, aes(x = Win...vs.Rank, fill = Conference)) + geom_histogram(binwidth = .15) +
  ggtitle("Histogram of Win % vs. Ranked Opponents")
ggplot(data, aes(x = AvgPointDifferential, fill = Conference)) + geom_histogram(binwidth = 5) +
  ggtitle("Histogram of Average Point Differential")

### Checking logit linear relationship - data issue?
test_data<-data[,-c(1,3,20,40,42,44,45,46,47)]
boxTidwell(Selected~.,data=test_data)

glm.fit<-glm(Selected~.-Year-Team-Games,data=data,family='binomial')
summary(glm.fit)

##Checking for outliers
cooks<-cooks.distance(glm.fit)
any(cooks>1)
##No Cook's D values > 1, no outliers assumption satisfied
