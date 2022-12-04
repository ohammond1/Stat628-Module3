library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)
library(reshape2)
library(boot)

source("comparison_functions.R")

# Import our Data
business_df <- stream_in(file('../data/yelp_dataset_2022/business.json'))
user_df <- read.csv('../data/csv_dataset/elite_users.csv')
reviews_df <- read.csv('../data/csv_dataset/reviews_no_text.csv')

reviews_agg <- aggregate(reviews_df$X,
                by=list(reviews_df$business_id),
                FUN=length)
hist(log(reviews_agg[reviews_agg$x<500,]$x),breaks=50,)
length(reviews_agg[reviews_agg$x < 6,]$x)

restaurant_list <- c('Brazilian','British','Cajun','Chinese','Cuban',
                     'French','Indian','Italian','Japanese','Latin American',
                     'Mediterranean','Mexican','Middle Eastern','Modern European',
                     'Korean','Peruvian','Vietnamese','American')

review_comp <- list()
rating_comp <- list()
day_range <- 30
for(i in 1:length(restaurant_list)) {
    print(i)
    print(restaurant_list[i])
    review_comp[[i]] <- review_count_comparison(business_df, reviews_df, user_df,
                                           c(restaurant_list[i],'Restaurant'),
                                           day_range,
                                           TRUE)
    rating_comp[[i]] <- review_star_comparison(business_df, reviews_df, user_df,
                                               c(restaurant_list[i],'Restaurant'),
                                               day_range,
                                               TRUE)
}
review_comp_non_elite <- list()
rating_comp_non_elite <- list()
for(i in 1:length(restaurant_list)) {
    print(i)
    print(restaurant_list[i])
    review_comp_non_elite[[i]] <- review_count_comparison(business_df, reviews_df, user_df,
                                                c(restaurant_list[i],'Restaurant'),
                                                day_range,
                                                FALSE)
    rating_comp_non_elite[[i]] <- review_star_comparison(business_df, reviews_df, user_df,
                                               c(restaurant_list[i],'Restaurant'),
                                               day_range,
                                               FALSE)
}

# bootstrapping functions
diff_in_diff_reviews <- function(dataset, indices) {
    df_subset <- dataset[indices,]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category=='elite',]$reviews_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category!='elite',]$reviews_before)
    return(d_mean1 - d_mean2)
}

diff_in_diff_ratings <- function(dataset, indices) {
    df_subset <- dataset[indices,]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$rating_after) - 
        mean(df_subset[df_subset$category=='elite',]$rating_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$rating_after) - 
        mean(df_subset[df_subset$category!='elite',]$rating_before)
    return(d_mean1 - d_mean2)
}

# Formatting data into bootstrap
boot_review_comp <- list()
boot_ratings_comp <- list()
for(i in 1:length(restaurant_list)) {
    temp_elite_df <- review_comp[[i]][,c('reviews_before','reviews_after')]
    temp_elite_df$category <- 'elite'
    temp_elite_df$diff <- temp_elite_df$reviews_after - temp_elite_df$reviews_before
    
    temp_normal_df <- review_comp_non_elite[[i]][,c('reviews_before','reviews_after')]
    temp_normal_df$diff <- temp_normal_df$reviews_after - temp_normal_df$reviews_before
    temp_normal_df$category <- 'normal'
    temp_df <- rbind(temp_elite_df, temp_normal_df)
    boot_review_comp[[i]] <- temp_df
    
    temp_elite_df <- rating_comp[[i]][,c('rating_before','rating_after')]
    temp_elite_df$category <- 'elite'
    temp_elite_df$diff <- temp_elite_df$rating_after - temp_elite_df$rating_before
    
    temp_normal_df <- rating_comp_non_elite[[i]][,c('rating_before','rating_after')]
    temp_normal_df$category <- 'normal'
    temp_normal_df$diff <- temp_normal_df$rating_after - temp_normal_df$rating_before
    
    temp_df <- rbind(temp_elite_df, temp_normal_df)
    boot_ratings_comp[[i]] <- temp_df
}

# Bootstrap for all of the restaurants
boot_tests_review <- list()
boot_tests_rating <- list()
review_p_values <- list()
rating_p_values <- list()
for(i in 1:length(restaurant_list)) {
    boot_tests_review[[i]] <- boot(boot_review_comp[[i]], diff_in_diff_reviews, R=1000)    
    boot_test_centered <- boot_tests_review[[i]]$t - mean(boot_tests_review[[i]]$t)
    perc.rank <- ecdf(boot_test_centered)
    review_p_values[[i]] <- 1-perc.rank(boot_tests_review[[i]]$t0)
    
    boot_tests_rating[[i]] <- boot(boot_ratings_comp[[i]], diff_in_diff_ratings, R=1000)    
    boot_test_centered <- boot_tests_rating[[i]]$t - mean(boot_tests_rating[[i]]$t)
    perc.rank <- ecdf(boot_test_centered)
    rating_p_values[[i]] <- 1-perc.rank(boot_tests_rating[[i]]$t0)
}

# plot the difference in differences boot strap p-values
boot_review_pval_df <- data.frame(p_value=unlist(review_p_values), category=restaurant_list)
boot_review_pval_df <- boot_review_pval_df[order(boot_review_pval_df$p_value),]
ggplot(data=boot_review_pval_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    geom_hline(yintercept=0.0029,linetype='dotted',col='red') +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Difference-in-Difference of Reviews")

boot_rating_pval_df <- data.frame(p_value=unlist(rating_p_values), category=restaurant_list)
boot_rating_pval_df <- boot_rating_pval_df[order(boot_rating_pval_df$p_value),]
ggplot(data=boot_rating_pval_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    geom_hline(yintercept=0.0029,linetype='dotted',col='red') +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Difference-in-Difference of Ratings")


p_values <- list()
for(i in 1:length(restaurant_list)) {
    p_values[i] <- t.test(review_comp[[i]]$reviews_before, review_comp[[i]]$reviews_after,
                          paired=TRUE, alternative = 'less')$p.value
}

















# Looking At chinese Restaurants
print("Chinese Restaurants")
chinese_30_review_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                c('Chinese','Restaurant'),
                                                30)
length(unique(chinese_30_review_comparison$business_id))
chinese_30_star_comparison <- review_star_comparison(business_df, reviews_df, user_df,
                                                        c('Chinese','Restaurant'),
                                                        30)


chinese_30_comparison$review_change <- chinese_30_comparison$reviews_after - chinese_30_comparison$reviews_before
# Do a paired samplet-test
t.test(chinese_30_review_comparison$reviews_before, chinese_30_review_comparison$reviews_after,
       paired=TRUE, alternative = 'less')

t.test(chinese_30_star_comparison$rating_before, chinese_30_star_comparison$rating_after,
       paired=TRUE, alternative = 'less')


print("Japanese Restaurants")
japanese_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Japanese',
                                                 'Restaurant',
                                                 30)

t.test(japanese_30_comparison$reviews_before, japanese_30_comparison$reviews_after,
       paired=TRUE, alternative = 'less')


print("American Restaurants")
american_30_reviews_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                c('American','Restaurant'),
                                                30)

t.test(american_30_reviews_comparison$reviews_before, american_30_reviews_comparison$reviews_after,
       paired=TRUE, alternative = 'less')

print("Burger Restaurants")
burgers_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Burgers',
                                                 'Restaurant',
                                                 30)

t.test(burgers_30_comparison$reviews_before, burgers_30_comparison$reviews_after,
       paired=TRUE, alternative = 'less')



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

ggplot(rating_comparison, aes(rating_change),) +
    geom_histogram(binwidth=.15, aes(y=..count../sum(..count..)), fill='#00B4EF') +
    ylab("Percentage") +
    xlab("Rating Difference") +
    xlim(-4,4)+
    ggtitle("Difference in Rating of Chinese Restaurants") +
    scale_y_continuous(label=scales::percent) +
    theme(plot.title = element_text(hjust = 0.5),aspect.ratio = 6/10)


t.test(reviews_comparison$reviews_after, reviews_comparison$reviews_before,
       paired=TRUE, alternative = 'greater')

t.test(rating_comparison$rating_after, rating_comparison$rating_before,
       paired=TRUE, alternative = 'greater')

mean(rating_comparison$rating_after)
mean(rating_comparison$rating_before)