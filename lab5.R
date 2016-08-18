# Reset R session
rm(list=ls())
cat("\014")

library(ggplot2)
qplot(as.factor(mtcars$cyl))

myPlot <- ggplot(diamonds, aes(carat, price, colour = cut))
(myPlot <- myPlot + geom_point())
summary(myPlot)

myBestFitLine <- geom_smooth(method = "lm", se = T,
                             colour = alpha("steelblue", 0.5), size = 2)
qplot(sleep_rem, sleep_total, data = msleep) + myBestFitLine
qplot(awake, brainwt, data = msleep) + myBestFitLine
qplot(bodywt, brainwt, data = msleep, log= "xy") + myBestFitLine

myPlot <- ggplot(mtcars, aes(x = mpg, y = wt))
myPlot + geom_point()
myPlot + geom_point(aes(colour = factor(cyl)))
myPlot + geom_point(aes(y = disp)) #the y-axis label of the plot does not change! Possible mistakes

my_ggplotObj_01 <- ggplot(diamonds, aes(carat))
summary(my_ggplotObj_01)
my_ggplotObj_01 + geom_histogram()
my_ggplotObj_01 + geom_histogram(aes(y = ..density..))


airquality$MonthName <- factor(month.name[airquality$Month], levels = unique(month.name[airquality$Month]))
myPlot <- ggplot(airquality, aes(Day, Temp, group = Month, colour = MonthName))
myPlot + geom_point() + geom_line()

windMeans <- aggregate(Wind ~ Month, airquality, mean)[,2]
myPlot2 <- ggplot(airquality, aes(MonthName, Wind))
myPlot2 + geom_bar(stat = "summary", fun.y ="mean")

myPlot2 + geom_bar(windMeans, stat = "identity")


library(tidyr)
library(dplyr)
library(scales)
myDataFrame <- data.frame(
  City = c("Austin", "Georgia", "Vancouver"),
  Fancy = c(35000, 43000, 106000),
  Normal = c(30000, 44000, 77000)
)

myDataFrame <- gather(myDataFrame, lightState, Sales, Fancy, Normal)
myPlot <- ggplot(myDataFrame, aes(City, Sales, group = lightState, colour = lightState))
myPlot + 
  geom_line() + 
  geom_point(size = 3) + 
  scale_y_continuous(labels = comma) +
  xlab("\n City") + 
  ylab("Sales\n") + 
  theme(legend.title=element_blank())



set.seed(1979)
(myDataFrame <- data.frame(
  uniqueID = 1:4,
  treatment = sample(rep(c("iOS", "Android"), each = 2)),
  work_am = runif(4, 0, 1),
  home_am = runif(4, 0, 1),
  work_pm = runif(4, 1 ,2),
  home_pm = runif(4, 1, 2)
))

(myTidyDataFrame <- gather(myDataFrame, sample, time, -uniqueID, -treatment))
(myTidierDataFrame <- separate(myTidyDataFrame, sample, into = c("location", "timeOfDay"), sep = "\\_"))
myTidierDataFrame$timeOfDay <- toupper(myTidierDataFrame$timeOfDay)

ggplot(myTidierDataFrame, aes(timeOfDay, time)) +
  aes(group = interaction(location, treatment),
      colour = interaction(location, treatment)) +
  stat_summary(fun.y = mean, geom = "line", size = 1) +
  stat_summary(fun.y = mean, geom = "point", size = 3) +
  labs(x = "\nTime of Day", y = "Time on Mobile Device \n") +
  theme(legend.title = element_blank())














