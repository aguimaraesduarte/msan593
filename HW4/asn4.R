# Reset R session
rm(list=ls())
cat("\014")

# Question 1
tweets = read.csv("tweets.csv", header = F, stringsAsFactors = F)

# 1
# tweets with the word <flight> in them
(q1 <- tweets[grepl("\\<flight\\>", tweets[,]),])

# 2
# number of tweets ending in a <?>
(q2 <- sum(grepl("\\?$", tweets[,])))
#grep("\\?$", tweets[,], value = T)

# 3
# number of tweets with three uppercase letters (airport name)
(q3 <- sum(grepl("[A-Z]{3}", tweets[,]))) # problem with tweet #7 EZP in url
#grep("[A-Z]{3}", tweets[,], value = T)

# 4
# tweets with a url (<http> or <https>)
(q4 <- tweets[grepl("http[s]?:\\/\\/", tweets[,]),])

# 5
# replace 2 or more <!> with only one <!>
(q5 <- tweets[,] <- gsub("[\\!]{2,}", "\\!", tweets[,]))

# 6
# replace 2 or more <?>, <!>, <.> with only one <.>.
q6 <- tweets[,] <- gsub("[\\!\\?\\.]{2,}", "\\.", tweets[,])
# split on <.> and save into list
(split_tweets <- strsplit(q6, "\\.")) # problem with urls

# 7
# define hashtag pattern < #<statement> >
pattern <- "\\#[a-zA-z0-9]*"
# get all matches for each tweet
m <- gregexpr(pattern, tweets[,])
# catch the hashtag and save into list
(hashtags <- regmatches(tweets[,], m))

#########################################################
#########################################################
# Reset R session
rm(list=ls())
cat("\014")

# Question 2
dates <- read.csv("dates.csv", header = T, stringsAsFactors = F,
                  na.strings = "")
dates <- dates[complete.cases(dates),] #remove NAs

# 1 --> commented out for grader
#toRemove_for = c()
#for(i in 1:nrow(dates)){
#  r <- strptime(dates[i,], "%m/%d/%y %H:%M")
#  if(r[1] > r[2]){
#    toRemove <- append(toRemove, i)
#  }
#}
#dates_fixed_for <- dates[-toRemove_for,]

# 2
fun_toRemove <- function(x){
  x <- strptime(x, "%m/%d/%y %H:%M")
  return(x[1] > x[2])
}
toRemove_apply <- apply(dates, 1, fun_toRemove)
dates_fixed_apply <- dates[!toRemove_apply,]

#########################################################
#########################################################
# Reset R session
rm(list=ls())
cat("\014")

# Question 3
sotu <- readLines("stateoftheunion1790-2012.txt", encoding = "UTF-8")

# results vectors
# some will be created in the loop
presidents <- c()
dates <- c()
num_lines <- c()
num_sentences <- c()
num_words <- c()

counter = 1
# indices of *** in the text
indices_separators <- which(grepl("\\*\\*\\*", sotu))
for(i in indices_separators){
  if(i == indices_separators[length(indices_separators)]){
    break #no text after the last speech
  }
  
  presidents <- append(presidents, sotu[i+3]) #the president's name is always in this position
  
  dates <- append(dates, sotu[i+4]) #the date is always in this position (format = %B %d, %Y)
  
  begin <- i+6 #index of beginning of speech
  end <- indices_separators[counter+1]-2 #index of end of speech
  speech <- sotu[begin:end]
  
  num_lines <- append(num_lines, length(speech != "")) #an empty line does not count as a line
  # num_sentences: 
  #  1) gregexpr("\\.", speech) returns list with -1 if no <.> found, or indices if <.> found
  #  2) sapply(gregexpr("\\.", speech), function(x) sum(x > 0)) returns vector with the number of <.> found in each line
  #  3) sum(sapply(gregexpr("\\.", speech), function(x) sum(x > 0))) returns the sum of previous list: the number of sentences
  num_sentences <- append(num_sentences, sum(sapply(gregexpr("\\.", speech), function(x) sum(x > 0))))
  
  # num_words: similar approach as previously, but catching every instance of [[:alnum:]]+ i.e. words (numbers count too)
  num_words <- append(num_words, sum(sapply(gregexpr("[[:alnum:]]+", speech), function(x) sum(x > 0))))
  
  # don't forget to update the counter to go to the next speech
  counter <- counter + 1
}

# dates is a vector of dates in the format %B %d, %Y
#  1) convert to Date
#  2) format to get only the year (as a numeric)
#  3) apply to all elements
years <- as.numeric(lapply(dates, function(x) as.numeric(format(as.Date(x, "%B %d, %Y"),'%Y'))))

# same as previously, but months are characters
months <- as.character(lapply(dates, function(x) format(as.Date(x, "%B %d, %Y"),'%B')))

# same as previously, days are numeric
days_of_month <- as.numeric(lapply(dates, function(x) as.numeric(format(as.Date(x, "%B %d, %Y"),'%d'))))

# same as previously, days of week are character
days_of_week <- as.character(lapply(dates, function(x) weekdays(as.Date(x, "%B %d, %Y"))))

# create dataframe
# presidents are factors with levels unique(presidents)
# months are factors with levels "January", "Februrary", ...
# days of week are factors with levels "Monday", "Tuesday", ...
DF_sotu <- data.frame("President" = factor(presidents, levels = unique(presidents)),
                      "Year"= years,
                      "Month"= factor(months, levels = c("January",
                                                         "February",
                                                         "March",
                                                         "April",
                                                         "May",
                                                         "June",
                                                         "July",
                                                         "August",
                                                         "September",
                                                         "October",
                                                         "November",
                                                         "December"), ordered = T),
                      "DayOfMonth" = days_of_month,
                      "DayOfWeek" = factor(days_of_week, levels = c("Monday",
                                                                    "Tuesday",
                                                                    "Wednesday",
                                                                    "Thursday",
                                                                    "Friday",
                                                                    "Saturday",
                                                                    "Sunday"), ordered = T),
                      "NumLines" = num_lines,
                      "NumSentences" = num_sentences,
                      "NumWords" = num_words,
                      stringsAsFactors = F)

# verify that the data frame was created as wished
str(DF_sotu)
