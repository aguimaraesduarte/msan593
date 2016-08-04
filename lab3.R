
myMod <- lm(mpg ~ wt, data = mtcars)
names(myMod)
(df <- myMod$df.residual)
names(summary(myMod))
(r2 <- summary(myMod)$r.squared)
(r2a <- summary(myMod)$adj.r.squared)

(myDataFrame <- data.frame(abc = 1:3, xyz = -4:-6, bob = 5:7, jan = 10:12))
(myDataFrame <- myDataFrame[sample(1:ncol(myDataFrame))])
(myDataFrame <- myDataFrame[sample(1:nrow(myDataFrame)),])
(myDataFrame <- myDataFrame[sample(1:nrow(myDataFrame)), sample(1:ncol(myDataFrame))])
#permute all entries of a single column?
(myDataFrame <- data.frame(abc = 1:3, xyz = -4:-6, bob = 5:7, jan = 10:12))
(c <- sample(1:ncol(myDataFrame)), 1)
(r_perm <- sample(1:nrow(myDataFrame)))
myDataFrame[c] <- myDataFrame[c][r_perm,]
myDataFrame



table(iris$Species)
with(iris, unique(Species))
with(mtcars, which(cyl == 6 & hp == 110))
subset(mtcars, mpg > 32, select = c("hp", "disp"))
seq(1, 10, length.out = 100)
seq_len(10)
myVec <- c(LETTERS[4:8])
seq_along(myVec)
rep(letters[2:4], times = 2, each = 2)
myAtomicVector <- c(1 ,2, 99.99, NA, sqrt(2))
any(is.na(myAtomicVector))



names(iris)
table(iris$Sepal.Length)
c <- cut(iris$Sepal.Length, 8)
levels(c)
pretty(iris$Sepal.Length, 8)
length(unique(iris$Species))
subset(iris, iris$Petal.Width > 2.3)
oddNumbers <- seq(11, 87, 2)
sample(oddNumbers, 3, replace = F)



fun1 <- function(a, b){
  return(subset(mtcars[a:b,]))
}
fun1(1, 3)

fun2 <- function(vec){
  sq <- vec^2
  df <- data.frame(vec, sq)
  return(head(df, 10))
}
v <- c(1, 2, 3, 4, 5, 6)
fun2(v)

fun3 <- function(vec) {
  sq <- vec^2
  df <- data.frame(vec, sq)
  #vec <<- df #only works if the global variable is named "vec"
  #assign(as.character(substitute(vec)), df, env = .GlobalEnv)
  #eval(parse(text = paste(substitute(vec), "<<- data.frame(vec, vec^2)")))
}
v <- fun3(v)
v



rm(list=ls())
cat("\014")

myAtomicVector <- 1
myFunc_06 <- function() {
  if(!exists("myAtomicVector")){
    myAtomicVector <- 999
  } else {
    myAtomicVector <- myAtomicVector + 1
  }
  print(myAtomicVector)
}
myFunc_06()
rm(myAtomicVector)
myFunc_06()


myFunc <- function(x, y, z){
  x+y+z
}





