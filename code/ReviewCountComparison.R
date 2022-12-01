library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)

source("comparison_functions.R")

# Import our Data
business_df <- stream_in(file('../data/yelp_dataset_2022/business.json'))
user_df <- read.csv('../data/csv_dataset/elite_users.csv')
reviews_df <- read.csv('../data/csv_dataset/reviews_no_text.csv')

print("Chinese Restaurants")
chinese_30_review_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                c('Chinese','Restaurant'),
                                                30)
chinese_30_star_comparison <- review_star_comparison(business_df, reviews_df, user_df,
                                                        c('Chinese','Restaurant'),
                                                        30)


chinese_30_comparison$review_change <- chinese_30_comparison$reviews_after - chinese_30_comparison$reviews_before
t.test(chinese_30_comparison$reviews_before, chinese_30_comparison$reviews_after, paired=TRUE, alternative = 'less')


print("Japanese Restaurants")
japanese_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Japanese',
                                                 'Restaurant',
                                                 30)

t.test(japanese_30_comparison$reviews_before, japanese_30_comparison$reviews_after, paired=TRUE, alternative = 'less')

print("American Restaurants")

american_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                'American',
                                                'Restaurant',
                                                30)

t.test(american_30_comparison$reviews_before, american_30_comparison$reviews_after, paired=TRUE, alternative = 'less')

print("Burger Restaurants")
burgers_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Burgers',
                                                 'Restaurant',
                                                 30)

t.test(burgers_30_comparison$reviews_before, burgers_30_comparison$reviews_after, paired=TRUE, alternative = 'less')

ggplot(chinese_30_comparison, aes(x=factor(0), y=review_change)) +
    geom_boxplot(width=0.2) +
    theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    ylim(-50,50) +
    ylab("Review Change") +
    ggtitle("Difference in Number of Reviews: Chinese Restaurants")
    


ggplot(chinese_30_comparison, aes(review_change),) +
    geom_histogram(binwidth=1, aes(y=..count../sum(..count..)), fill='#E68613') +
    ylab("Percentage") +
    xlab("Review Difference") +
    xlim(-15,15)+
    ggtitle("Difference in Number of Reviews of Chinese Restaurants") +
    scale_y_continuous(label=scales::percent) +
    theme(plot.title = element_text(hjust = 0.5),aspect.ratio = 6/10)


category1 <-'Chinese'
category2 <-'Restaurant'



ggplot(rating_comparison, aes(rating_change),) +
    geom_histogram(binwidth=.15, aes(y=..count../sum(..count..)), fill='#00B4EF') +
    ylab("Percentage") +
    xlab("Rating Difference") +
    xlim(-4,4)+
    ggtitle("Difference in Rating of Chinese Restaurants") +
    scale_y_continuous(label=scales::percent) +
    theme(plot.title = element_text(hjust = 0.5),aspect.ratio = 6/10)


t.test(reviews_comparison$reviews_after, reviews_comparison$reviews_before, paired=TRUE, alternative = 'greater')

t.test(rating_comparison$rating_after, rating_comparison$rating_before, paired=TRUE, alternative = 'greater')

mean(rating_comparison$rating_after)
mean(rating_comparison$rating_before)