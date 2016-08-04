firstNames <- c("George", "Tony", "Gino", "Peter", "Claire")
lastNames <- c("Fuentes", "Barr", "Smith", "Mathis", "Judd")
monthsWork <- c(2, 9, 16, 130, 24)
married <- c(T, F, T, F, F)
areaCode <- c(478, 831, 425, 919, 510)
areaCode <- factor(areaCode)

mean(monthsWork)
median(monthsWork)
sd(monthsWork)
var(monthsWork)
max(monthsWork)
min(monthsWork)
quantile(monthsWork) #quantile(monthsWork, .75)
summary(monthsWork)

sum(married)
mean(married)

(myExperimentalData <- data.frame(lastNames,
                                 firstNames,
                                 married,
                                 areaCode,
                                 monthsWork,
                                 stringsAsFactors = FALSE))
str(myExperimentalData)
mean(myExperimentalData$monthsWork[myExperimentalData$married])
#mean(myExperimentalData[myExperimentalData$married, "monthsWork"]) #also works


titanic <- read.csv("titanic.csv", stringsAsFactors = F)
str(titanic$Survived)

titanic$Survived <- ifelse(titanic$Survived == 0, "Perished", "Survived")
str(titanic$Survived)


#for(i in 1:length(titanic$Survived)){
#  if(titanic$Survived[i] == 0){
#    titanic$Survived[i] = "Perished"
#  } else{
#    titanic$Survived[i] = "Survived"
#  }
#}

#titanic$Survived[titanic$Survived == 1] <- "Survived"
#titanic$Survived[titanic$Survived == 0] <- "Perished"

titanic$ageClass <- factor(
  ifelse(titanic$Age < 18, "Minor", ifelse(titanic$Age < 65, "Adult", "Senior")),
  levels = c("Minor", "Adult", "Senior"),
  ordered = T)
str(titanic)

i <- 1
while(i != 100){
  name <- titanic$Name[order(titanic$Name[titanic$Survived == "Survived"], decreasing = T)][i]
  i <- i+1
}
print(titanic$Name[order(titanic$Name[titanic$Survived == "Survived"], decreasing = T)][i])
print(titanic$Name[order(titanic$Name[titanic$Survived == "Survived"], decreasing = T)][100])

for(class in titanic$Pclass){
  print(typeof(i))
  print(
    switch(class,
           "1" = "First Class",
           "2" = "Business Class",
           "3" = "Economy"
           )
  )
}

titanic$Pclass <- factor(titanic$Pclass)
titanic$Survived <- factor(titanic$Survived)
titanic$SibSp <- factor(titanic$SibSp)
titanic$Parch <- factor(titanic$Parch)
for(c in 1:ncol(titanic)){
  if(c == 1) next
  if(is.numeric(titanic[[c]])){
    hist(titanic[[c]], main=names(titanic)[c])
    cat ("Press [enter] to continue")
    line <- readline()
  } else if(is.factor(titanic[[c]])){
    plot(titanic[[c]], main=names(titanic)[c])
    cat ("Press [enter] to continue")
    line <- readline()
  }
}

