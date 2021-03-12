#loading in libraries
library(ggplot2)

#loading in data
data = read.csv("football.csv")
data$Conference = as.factor(data$Conference)
data$Year = as.factor(data$Year)

print("This should be in Alex's branch")
print("Cameron was present")
print("Rich's branch")

# Explorartory Histograms
ggplot(data, aes(x = Games)) + geom_histogram(binwidth = 1, fill = 4) + 
  ggtitle("Histogram of Games Played") + facet_grid(Year ~ .)
ggplot(data, aes(x = Avg.Point.Differential, fill = Conference)) + geom_histogram(binwidth = 5) +
  ggtitle("Histogram of Average Point Differential")
ggplot(data, aes(x = Avg.Point.Differential)) + 
  geom_histogram(binwidth = 5, fill = "red", color = "white") +
  ggtitle("Histogram of Average Point Differential") + 
  facet_grid(Conference ~ ., scales = "free")
