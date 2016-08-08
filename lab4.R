myFunc_13 <- function(arg1, arg2) {
  any(c(missing(arg1), missing(arg2)))
}

myFunc_14 <- function(arg1 = NULL, arg2 = NULL) {
  any(c(is.null(arg1), is.null(arg2)))
}

myFunc_17 <- function(a = ls()){
  z <- 10
  a
}
myFunc_17() #lists local variables (a, z)
myFunc_17(4)
myFunc_17(ls()) #lists global variables in the environment

myFunc <- function(firstRow, lastRow){
  iris[firstRow:lastRow,]
}
myFunc(firstRow = 1, lastRow = 3)
#formal: firstRow, lastRow
#calling: 1, 3

codetools::findGlobals(myFunc)
#the function is not self-contained (iris)

myFunc2 <- function(firstRow, lastRow, myDataFrame){
  myDataFrame[firstRow:lastRow,]
}
myFunc2(1, 3, mtcars)

myFunc3 <- function(firstRow = 1, lastRow = 10, myDataFrame = iris){
  force(c(firstRow, lastRow, myDataFrame))
  myDataFrame[firstRow:lastRow,]
}
myFunc3(jane)
myFunc()


logVector <- function(vec){
  for(i in 1:length(vec)){
    tryCatch(print(paste("log of", vec[i], "is", log(vec[i]))),
             error = function(x){
               writeLines(paste("Error\nCannot compute the log of string '", vec[i], "'", sep = ""))
             },
             warning = function(x){
               writeLines(paste("Warning\nCannot compute log of negative number", vec[i]))
             })
  }
}

logVector(c(1,2,3))
logVector(c(1,-2,3))
logVector(c(1,-2,"a"))


(mydf <- data.frame(A=c(1,10,7,2,1,6),
                    B=c(6,4,9,9,10,2),
                    C=c(1,4,5,3,5,1),
                    D=c(5,-99,4,8,9,3),
                    E=c(-99,9,1,6,8,8),
                    F=c(1,3,4,8,6,5)))
mydf[mydf == -99] <- NA
mydf

fix99s_byCol <- function(myCol) {
  myCol[myCol == -99] <- NA
  myCol
}

(mydf <- data.frame(A=c(1,10,7,2,1,6),
                    B=c(6,4,9,9,10,2),
                    C=c(1,4,5,3,5,1),
                    D=c(5,-99,4,8,9,3),
                    E=c(-99,9,1,6,8,8),
                    F=c(1,3,4,8,6,5)))

mydf2 <- fix99s_byCol(mydf)
mydf2

mydf3 <- as.data.frame(lapply(mydf, fix99s_byCol))

fix_byCol <- function(myCol, myVal) {
  myCol[myCol == myVal] <- NA
  myCol
}

mydf4 <- as.data.frame(lapply(mydf, fix_byCol, myVal = c(-99, 8)))
mydf4


summaryByCol <- function(myCol){
  tryCatch(c(mean(myCol),
             median(myCol),
             sd(myCol),
             var(myCol),
             quantile(myCol),
             IQR(myCol)),
           error = function(x) "Error",
           warning = function(x) "Warning")
}

mydf5 <- data.frame(1:5, 2:6, 3:7)
lapply(mydf5, summaryByCol)

ptm <- system.time()
res1 <- numeric(ncol(state.x77))
for(c in 1:ncol(state.x77)){
  res1[c] <- mean(state.x77[, c])
  names(res1) <- colnames(state.x77)
}
res1
system.time() - ptm

ptm <- proc.time()
res2 <- apply(state.x77, 2, mean)
res2
proc.time() - ptm


