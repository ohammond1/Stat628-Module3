install.packages("tidytext")
install.packages("textstem")
install.packages("textdata")
install.packages("lexicon")
library(tidytext)
library(data.table)
library(stringr)
library(jsonlite)
library(tokenizers)
library(textstem)
library(dplyr)
library(textdata)
library(lexicon)

#read data
review=fread("elite_reviews_sample.csv")
tidy=data.frame(review$review_id,review$text,review$user_id,review$business_id,review$stars,
                review$date,review$elite,review$average_stars)
colnames(tidy)=c("review_id","text","user_id","business_id","stars"
                 ,"date","elite","average_stars")
tidy=as_tibble(tidy)

#token

words= tidy %>% 
  unnest_tokens(word, text) 
  

#Afinn
afinn=get_sentiments("afinn")
sentiment <- words %>%
  inner_join(afinn, by = "word") %>%
  group_by(review_id,user_id,business_id,stars,
           date,elite,average_stars) %>%
  summarise(sentiment = 0.2*mean(value))

write.csv(sentiment,file = "elite_sentiment.csv")



