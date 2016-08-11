rm(list=ls())
cat("\014")

age <- c(23, 35, 31, 49, 77, 42, 50, 30, 12)
salaryInThousands <- c(33, 105, 153, 0, 74, 99, 32, 188, 12)

mean(age)
sd(age)
var(salaryInThousands)
cor(age,salaryInThousands)
hist(age)
hist(salaryInThousands)
plot(age,salaryInThousands)

# detach("package:PKGNAME", unload=TRUE)
# use library(PKGNAME) instead of require(PKGNAME)


doubleAtomicVector <- c(1, 3.14, 99.999)
integerAtomicVector <- c(1L, 3L, 19L)
logicalAtomicVector <- c(TRUE, FALSE, T, F)
characterAtomicVector <- c("this", "is a", "string")


myFavNum <- 3/10 # double
myNums <- c(1, 2, 3, 5, 8, 13, 21) # double
firstNames <- c("Dereck", "Nick") # character
myVec <- c("Perez", 25) # character

(myAtomicVector <- c(1, 2, 3, 4, -99, 5, NA, 4, 22.223))
myAtomicVector[5]
myAtomicVector[c(1,2,5,9)]
myAtomicVector[10]
myAtomicVector[3:8]

myFirstAddition <- myNums[7] + myFavNum
mySecondAddition <- myNums + myFavNum
myFirstSum <- sum(c(myNums, myFavNum))
thisIsGettingMoreComplex <- min(myNums) + myFavNum
whatTypeOfVectorIsThis <- as.numeric(myVec[2]) + myNums[2]


(myAtomicVector_01 <- c(99.1, 98.2, 97.3, 96.4, NA))
sum(myAtomicVector_01)
mean(myAtomicVector_01)
sum(myAtomicVector_01, na.rm = TRUE)
mean(myAtomicVector_01, na.rm = TRUE)


myVec <- c(1, NA)
myVec[2] == NA # NEVER DO THIS!
is.na(myVec[2])


data() # AirPassengers


(myAtomicVector_01 <- c(99.1, 98.2, 97.3, 96.4))
myAtomicVector_01[myAtomicVector_01 > 98] # equivalent to myAtomicVector_01[c(T, T, F, F)]


myAtomicVector <- c(1, 4, 3, 2, NA, 3.22, -44, 2, NA, 0, 22, 34)

length(myAtomicVector[myAtomicVector>0 & !is.na(myAtomicVector)])
length(myAtomicVector[myAtomicVector<0 & !is.na(myAtomicVector)])
length(myAtomicVector[myAtomicVector==0 & !is.na(myAtomicVector)])
length(myAtomicVector[is.na(myAtomicVector)])
length(myAtomicVector[myAtomicVector!=0 & !is.na(myAtomicVector)])
sum(myAtomicVector[myAtomicVector>0 & !is.na(myAtomicVector)])
sum(myAtomicVector[myAtomicVector<0 & !is.na(myAtomicVector)])

# Solution
sum(myAtomicVector > 0, na.rm = T)
sum(myAtomicVector < 0, na.rm = T)
sum(myAtomicVector == 0, na.rm = T)
sum(is.na(myAtomicVector))
sum(myAtomicVector != 0 & !is.na(myAtomicVector), na.rm = T) # sum(myAtomicVector != 0, na.rm = T)
sum(myAtomicVector[myAtomicVector > 0], na.rm = T)
sum(myAtomicVector[myAtomicVector < 0], na.rm = T)


(x <- c(1, NULL, 3))


x <- factor(c("M","F","F","M"))
bodyType <- factor(c("healthy", "healthy", "healthy", "obese", "overweight",
                     "overweight", "skinny"))
bodyType
levels(bodyType)
bodyType < "obese"

(bodyType <- factor(c("h", "h", "h", "ob", "ov", "ov", "s"),
                    levels = c("s", "h", "ov", "ob"),
                    labels = c("Skinny", "Healthy", "Overweight", "Obese"),
                    ordered = TRUE))
levels(bodyType)
bodyType < "Obese"
bodyType[bodyType < "Obese"]


myCyl <- mtcars$cyl
ord <- factor(myCyl, levels = sort(unique(myCyl)), labels = c("Small", "Medium", "Large"), ordered = T)
sum(ord<="Medium")


(xyz <- data.frame(numberColumn = 1:3, letterColumn = c("a","b","c"),
                   stringsAsFactors = F))
str(xyz)


myDataFrame_05 <- data.frame(x = 1:3, y = 98:100, z = 1000:1002)
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = -1))
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = -1:-2))
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = -1:-99))
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = c(-1, -2)))
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = c("-1", -2)))
str(myDataFrame_06) #EVERYTHING has been coerced into chr!
(myDataFrame_06 <- rbind(myDataFrame_05, qqq = c("a", -2, -3)))
str(myDataFrame_06) #EVERYTHING has been coerced into chr!


myDataFrame_07 <- data.frame(x = 1:3, y = 98:100, z = 1000:1002)
(myDataFrame_08 <- cbind(myDataFrame_07, qqq = -1))
(myDataFrame_08 <- cbind(myDataFrame_07, qqq = -1:-2)) #Error
(myDataFrame_08 <- cbind(myDataFrame_07, qqq = -1:-99))
(myDataFrame_08 <- cbind(myDataFrame_07, qqq = c("-1", -2))) #Error
(myDataFrame_08 <- cbind(myDataFrame_07, qqq = c("a", -2, -3)))
str(myDataFrame_08) #Only qqq is coerced into chr


x <- c(3, 2, 1)
if ( x > 2) {print("first element in vector > 2")}
ifelse(x > 2, ">2", "<=2")


grades <- c("A", "D", "F")
for (i in grades) {
  print(
    switch(i,
           A = "Well Done",
           B = "Alright",
           C = "C's get Degrees!",
           D = "Meh",
           F = "Uh-Oh"
    )
  )
}














