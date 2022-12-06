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

rm(list = ls())
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


#relationship between sentiment scores and average stars
sentiment=read.csv("elite_sentiment.csv")

review_star <- function(business_df, reviews_df,user_df, categories) {
  # Get list of businesses by specific business type
  business_subset <- business_df
  for(cat in categories) {
    business_subset <- business_subset[str_detect(tolower(business_df$categories), tolower(cat)),]
  }
  
  # Filter reviews down to the specified list
  reviews_subset <- reviews_df[reviews_df$business_id %in% business_subset$business_id,]
  reviews_subset <- reviews_subset[reviews_subset$business_id %in% sentiment$business_id]
  # Join users to reviews
  review_users_df = merge(x=reviews_subset, y=user_df, by='user_id')
  
  # Convert date column to a date type
  review_users_df$date_format <- as.Date(ymd_hms(format(review_users_df[,'date'],'%Y-%m-%d %H:%M:%S'), truncated=6))
  review_users_df$year <- year(format(review_users_df[,'date_format'],'%Y-%m-%d %H:%M:%S'))
  review_users_df$day=as.Date(review_users_df$date_format)
  
  
  sentiment$date_format=as.Date(ymd_hms(sentiment[,'date'], truncated=6))
  sentiment$year=year(sentiment[,'date_format'])
  sentiment$day=as.Date(sentiment$date_format)
  sen_subset <- sentiment[sentiment$business_id %in% business_subset$business_id,]
  #sen_subset=sen_subset[1:350,]
  # Get only reviews when the user was elite
  elite_reviews_df <- review_users_df[review_users_df$year %in% review_users_df$elite]
  
  for (j in 1:length(sen_subset$business_id)) {
    per=numeric(length = 30)
    sum=0
    for (i in 1:30) {
      cou=review_users_df%>%
        select(review_id,business_id,stars,day)%>%
        filter(business_id==sen_subset$business_id[j],day %in% (sen_subset$day[j]+i-1))
      if (length(cou$stars)>0){
        per[i]=cou$stars
      }
      
    }
    sen_subset$after[j]= mean(per[per!=0])
  }
  reg=lm(sen_subset$after~sen_subset$sentiment)
  summary(reg)
  plot(sen_subset$sentiment,sen_subset$after,col="blue",main="Average stars in the following one month vs sentiment scores"
       ,xlab = "Sentiment score",ylab = "Average stars",pch=16)
  abline(reg,col="red")
}





#elite stars and after one-month star

# sentiment and stars
review_star_star <- function(business_df, reviews_df,user_df, categories) {
  # Get list of businesses by specific business type
  business_subset <- business_df
  for(cat in categories) {
    business_subset <- business_subset[str_detect(tolower(business_df$categories), tolower(cat)),]
  }
  
  # Filter reviews down to the specified list
  reviews_subset <- reviews_df[reviews_df$business_id %in% business_subset$business_id,]
  reviews_subset <- reviews_subset[reviews_subset$business_id %in% sentiment$business_id]
  # Join users to reviews
  review_users_df = merge(x=reviews_subset, y=user_df, by='user_id')
  
  # Convert date column to a date type
  review_users_df$date_format <- as.Date(ymd_hms(format(review_users_df[,'date'],'%Y-%m-%d %H:%M:%S'), truncated=6))
  review_users_df$year <- year(format(review_users_df[,'date_format'],'%Y-%m-%d %H:%M:%S'))
  review_users_df$day=as.Date(review_users_df$date_format)
  
  
  sentiment$date_format=as.Date(ymd_hms(sentiment[,'date'], truncated=6))
  sentiment$year=year(sentiment[,'date_format'])
  sentiment$day=as.Date(sentiment$date_format)
  sen_subset <- sentiment[sentiment$business_id %in% business_subset$business_id,]
  sen_subset=sen_subset[1:100,]
  # Get only reviews when the user was elite
  elite_reviews_df <- review_users_df[review_users_df$year %in% review_users_df$elite]
  for (j in 1:length(sen_subset$business_id)) {
    per=numeric(length = 30)
    sum=0
    for (i in 1:30) {
      cou=review_users_df%>%
        select(review_id,business_id,stars,day)%>%
        filter(business_id==sen_subset$business_id[j],day %in% (sen_subset$day[j]+i-1))
      if (length(cou$stars)>0){
        per[i]=cou$stars
      }
      
    }
    sen_subset$after[j]= mean(per[per!=0])
  }
  reg=lm(sen_subset$after~sen_subset$stars)
  summary(reg)
  plot(sen_subset$sentiment,sen_subset$after,col="blue",main="Average stars in the following one month vs sentiment scores"
       ,xlab = "Elite reviewers' star",ylab = "Average stars")
  abline(reg,col="red")
}


#result

review_star(business_df, reviews_df, user_df,
  c('Chinese','Restaurant'))

review_star(business_df, reviews_df, user_df,"Restaurant")

review_star_star(business_df, reviews_df, user_df,"Restaurant")
