setwd("~/Documents/CPC Lab/Blog/Election 2019")

library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(data.table)

final<- read.csv("alltweets.csv")
final$engagement <- final$likes_count+final$replies_count+final$retweets_count
final$count <- 1

#total number of tweets
tot <- final %>% group_by(date, channel) %>% summarise(tot=sum(count))

##########################

tophash <- final[,c(5, 20,25,26,27)]

tophash <- tophash %>%   mutate(hashtags = strsplit(as.character(hashtags), " ")) %>%
  unnest(hashtags)

tophash$hashtags <- gsub("\\[", "", tophash$hashtags)
tophash$hashtags <- gsub("\\]", "", tophash$hashtags)
tophash$hashtags <- gsub("'", "", tophash$hashtags)

tophash1 <- tophash %>% filter(channel=="republic")
tophash1<- tophash1 %>% group_by(hashtags) %>% summarise(freq=sum(count))

########################

issues <- ("gdp|GDP|inflation|petrol|deisel|unemployment|jobs|farmer|trade|
           xxx|agriculture|aadhar|infrastructure|riots|communal|lynching|gender|women|
           xxx|dalit|tribal|RBI reform|judicial reform|police reform|education|health|environment|
           xxx|forests|corruption|rafale|foreign policy|national security|army|soldier|pulwama|jawan|
           xxx|balakot|pakistan|terrorist|ram mandir|ram temple|babri masjid|hindu|muslim|business| 
           xxxindo-pak|ind-pak")

final$tweet<-tolower(final$tweet)
final$issue <- str_extract(final$tweet, issues)

lookup <- data.frame(c("gdp","GDP","inflation","petrol","deisel","unemployment","jobs","farmer", "trade",
                       "agriculture","aadhar","infrastructure","riots","communal","lynching","gender","women",
                       "dalit","tribal","RBI reform","judicial reform","police reform","education","health","environment",
                       "forests", "corruption","rafale","foreign policy","national security","army","soldier","pulwama", "jawan",
                       "balakot","pakistan","terrorist","ram mandir","ram temple","babri masjid","hindu","muslim","business", 
                       "indo-pak", "ind-pak"),
                     c("economy","economy","inflation", "inflation","inflation","jobs","jobs","farmers","economy","farmers","aadhar",
                       "infrastructure","religion","religion", "religion", "vulnerable sections", "vulnerable sections",
                       "vulnerable sections","vulnerable sections","reforms","reforms","reforms", "education", "health",
                       "environment","environment","corruption","corruption", "foreign policy", "foreign policy", "pakistan",
                       "pakistan", "pakistan","pakistan","pakistan","pakistan","pakistan","religion","religion","religion",
                       "religion","religion","economy","pakistan", "pakistan"))

colnames(lookup) <- c("issue","word")

final <- left_join(final, lookup)

issues2<- final[,c("date","channel","issue","word","count","engagement", "tweet", "hashtags")]
issues2 <- na.omit(issues2)

#write.csv(issues2, "issues.csv")

issues3 <- issues2 %>% group_by(channel, issue) %>% summarise(count=sum(count), engagement=sum(engagement))
issues3_count<- dcast(issues3, issue~channel, fun=sum, value.var="count")

issues3_count$ndtv <- issues3_count$ndtv/sum(issues3_count$ndtv) * 100 
issues3_count$republic <- issues3_count$republic/sum(issues3_count$republic) * 100 

#toggle between ndtv and republic in issues2$channel=="___"
vulnerable <- subset(issues2, issues2$word=="vulnerable sections"&issues2$channel=="ndtv")
pakistan<- subset(issues2, issues2$word=="pakistan"&issues2$channel=="republic")
religion<- subset(issues2, issues2$word=="religion"&issues2$channel=="ndtv")
corruption<- subset(issues2, issues2$word=="corruption"&issues2$channel=="ndtv")
economy<- subset(issues2, issues2$word=="economy"&issues2$channel=="ndtv")
jobs <- subset(issues2, issues2$word=="jobs"&issues2$channel=="republic")

#repeat for various issues (line 89-94)
tophash <- corruption%>%   mutate(hashtags = strsplit(as.character(hashtags), " ")) %>%
  unnest(hashtags)

tophash$hashtags <- gsub("\\[", "", tophash$hashtags)
tophash$hashtags <- gsub("\\]", "", tophash$hashtags)
tophash$hashtags <- gsub("'", "", tophash$hashtags)

tophash1<- tophash %>% group_by(hashtags) %>% summarise(freq=sum(count))

####
