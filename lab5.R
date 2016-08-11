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

