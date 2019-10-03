ibrary(stringr)
library(dplyr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)


clean_tweets = function(Tweets) {
  
  Tweets$cleaner_Tweet = gsub("&amp", "", Tweets)
  
  Tweets$cleaner_Tweet <- str_replace(Tweets$cleaner_Tweet,"RT @[a-z,A-Z]*: ","")
  
  Tweets$cleaner_Tweet <- str_replace(Tweets$cleaner_Tweet,"RT:[a-z,A-Z]*: ","")
  
  Tweets$cleaner_Tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("http\\w+", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("[ \t]{2,}", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("^\\s+|\\s+$", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("@\\w+", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("[[:punct:]]", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("[[:digit:]]", "", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("\n", " ", Tweets$cleaner_Tweet)
  
  Tweets$cleaner_Tweet = gsub("^\\s+", "", Tweets$cleaner_Tweet) 
  
  Tweets$cleaner_Tweet <- str_replace_all(Tweets$cleaner_Tweet," "," ")
  
  Tweets$cleaner_Tweet <- str_replace_all(Tweets$cleaner_Tweet,"#[a-z,A-Z]*","")
  
  Tweets$cleaner_Tweet <- str_replace_all(Tweets$cleaner_Tweet,"@[a-z,A-Z]*","")
  
  return(Tweets)
  
}


data<-read.csv("concat_data.csv", header = 1, sep = ";")
data["clean_text"]<-""

for (i in 1:nrow(data)) {
  data$clean_text[i]<-clean_tweets(data$text[i])[2]
}

data <- apply(data,2,as.character)

write.csv(data, file = "concat_cleaned_data.csv",row.names=FALSE, sep=",")
