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

# Complete comparison for reviews and ratings using functions in comparison_functions.R
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

# Create comparison and differences for non-elite reviews for diff-in-diff analysis
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

# bootstrapping functions for difference-in-difference
# Review bootstrap
diff_in_diff_reviews <- function(dataset, indices) {
    df_subset <- dataset[indices,]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category=='elite',]$reviews_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category!='elite',]$reviews_before)
    return(d_mean1 - d_mean2)
}

# Rating Bootstrap for difference-in-difference
diff_in_diff_ratings <- function(dataset, indices) {
    df_subset <- dataset[indices,]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$rating_after) - 
        mean(df_subset[df_subset$category=='elite',]$rating_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$rating_after) - 
        mean(df_subset[df_subset$category!='elite',]$rating_before)
    return(d_mean1 - d_mean2)
}

# Formatting data into dataframes for bootstrap
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


# bootstrapping functions for Permutation testing
# Difference-in-difference of reviews
diff_in_diff_perm_reviews <- function(dataset, indices) {
    df_subset <- dataset
    df_subset$category <- df_subset$category[indices]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category=='elite',]$reviews_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$reviews_after) - 
        mean(df_subset[df_subset$category!='elite',]$reviews_before)
    return(d_mean1 - d_mean2)
}

# Difference-in-difference of ratings
diff_in_diff_perm_ratings <- function(dataset, indices) {
    df_subset <- dataset
    df_subset$category <- df_subset$category[indices]
    d_mean1 <- mean(df_subset[df_subset$category=='elite',]$rating_after) - 
        mean(df_subset[df_subset$category=='elite',]$rating_before)
    d_mean2 <- mean(df_subset[df_subset$category!='elite',]$rating_after) - 
        mean(df_subset[df_subset$category!='elite',]$rating_before)
    return(d_mean1 - d_mean2)
}

# Run the permutation tests and record p-values
review_perm_p_values <- list()
rating_perm_p_values <- list()
for(i in 1:length(restaurant_list)) {
    # Permutation test for reviews
    print(i)
    temp_boot <- boot(boot_review_comp[[i]], diff_in_diff_perm_reviews, R=1000)   
    review_perm_p_values[[i]] <- mean(temp_boot$t > temp_boot$t0)
    
    # Permutation test for ratings
    temp_boot <- boot(boot_ratings_comp[[i]], diff_in_diff_perm_ratings, R=1000)   
    rating_perm_p_values[[i]] <- mean(temp_boot$t > temp_boot$t0)
    
    # Include adjustment for actual observed value
    review_perm_p_values[[i]] <- review_perm_p_values[[i]] + (1/1000)
    rating_perm_p_values[[i]] <- rating_perm_p_values[[i]] + (1/1000)
}

# Create our alpha vector
alpha <- 0.05 / c(18:1)

# plot the difference in differences permutation p-values
boot_review_pval_df <- data.frame(p_value=unlist(review_perm_p_values), category=restaurant_list)
boot_review_pval_df <- boot_review_pval_df[order(boot_review_pval_df$p_value, boot_review_pval_df$category),]
boot_review_pval_df$group_alpha <- alpha
boot_review_pval_min_df <- boot_review_pval_df[c(1,2,3,4,5,6,7,8),]
ggplot(data=boot_review_pval_min_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Difference-in-Difference of Reviews - Permutation Test Top 8") +
    geom_step(data=boot_review_pval_min_df, mapping=aes(x=as.numeric(as.factor(reorder(category,p_value))), y=group_alpha), col='red', linetype='dashed')

# plot the difference-in-differences of ratings permutation testing
boot_rating_pval_df <- data.frame(p_value=unlist(rating_perm_p_values), category=restaurant_list)
boot_rating_pval_df <- boot_rating_pval_df[order(boot_rating_pval_df$p_value),]
boot_rating_pval_df$group_alpha <- alpha
boot_rating_pval_min_df <- boot_rating_pval_df[c(1,2,3,4,5,6,7,8),]
ggplot(data=boot_rating_pval_min_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Difference-in-Difference of Ratings - Permutation Test Top 8") +
    geom_step(data=boot_rating_pval_min_df, mapping=aes(x=as.numeric(as.factor(reorder(category,p_value))), y=group_alpha), col='red', linetype='dashed')

# p-values for paired t-test
p_values_review <- list()
p_value_rating <- list()
for(i in 1:length(restaurant_list)) {
    p_values_review[[i]] <- t.test(review_comp[[i]]$reviews_before, review_comp[[i]]$reviews_after,
                          paired=TRUE, alternative = 'less')$p.value
    p_value_rating[[i]] <- t.test(rating_comp[[i]]$rating_before, rating_comp[[i]]$rating_after,
                          paired=TRUE, alternative = 'less')$p.value
}

# plot the paired t-test of reviews
paired_review_df <- data.frame(p_value=unlist(p_values_review), category=restaurant_list)
paired_review_df <- paired_review_df[order(paired_review_df$p_value),]
paired_review_df$group_alpha <- alpha
paired_review_pval_min_df <- paired_review_df[c(1,2,3,4,5,6,7,8),]
ggplot(data=paired_review_pval_min_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Paired T-test Number of Reviews - Top 8") +
    geom_step(data=paired_review_pval_min_df, mapping=aes(x=as.numeric(as.factor(reorder(category,p_value))), y=group_alpha), col='red', linetype='dashed')

# plot the paired t-test of ratings
paired_rating_df <- data.frame(p_value=unlist(p_value_rating), category=restaurant_list)
paired_rating_df <- paired_rating_df[order(paired_rating_df$p_value),]
paired_rating_df$group_alpha <- alpha
paired_rating_pval_min_df <- paired_rating_df[c(1,2,3,4,5,6,7,8),]
ggplot(data=paired_rating_pval_min_df,mapping=aes(x=reorder(category,p_value), y=p_value)) +
    geom_bar(stat='identity', fill='lightblue') +
    theme(axis.text.x = element_text(angle = 60, vjust = 1.05, hjust=1.1)) +
    xlab("Restaurant Type") +
    ylab('P-Value') +
    ggtitle("Paired T-test Number of Ratings - Top 8") +
    geom_step(data=paired_rating_pval_min_df, mapping=aes(x=as.numeric(as.factor(reorder(category,p_value))), y=group_alpha), col='red', linetype='dashed')



# Create Dataframe for shinyapp
review_elite_diff <- c()
rating_elite_diff <- c()
review_diff_in_diff <- c()
rating_diff_in_diff <- c()
for(i in 1:length(restaurant_list)) {
    # create temp variables for each difference
    temp_elite_review_diff <- mean(review_comp[[i]]$reviews_after - review_comp[[i]]$reviews_before)
    temp_elite_rating_diff <- mean(rating_comp[[i]]$rating_after - rating_comp[[i]]$rating_before)
    temp_normal_review_diff <- mean(review_comp_non_elite[[i]]$reviews_after - review_comp_non_elite[[i]]$reviews_before)
    temp_normal_rating_diff <- mean(rating_comp_non_elite[[i]]$rating_after - rating_comp_non_elite[[i]]$rating_before)
    
    # add them to vectors
    review_elite_diff[i] <- temp_elite_review_diff
    rating_elite_diff[i] <- temp_elite_rating_diff
    
    review_diff_in_diff[i] <- temp_elite_review_diff - temp_normal_review_diff
    rating_diff_in_diff[i] <-temp_normal_review_diff - temp_normal_rating_diff
}

# create Dataframe of variables
shiny_app_df <- data.frame(restaurant=restaurant_list, 
                           review_elite_diff= review_elite_diff, rating_elite_diff=rating_elite_diff,
                           review_diff_in_diff = review_diff_in_diff, rating_diff_in_diff= rating_diff_in_diff,
                           review_paired_pval = unlist(p_values_review),
                           rating_paired_pval = unlist(p_value_rating),
                           diff_in_diff_reviews_pval = unlist(review_perm_p_values),
                           diff_in_diff_rating_pval = unlist(rating_perm_p_values)
                           )

save(shiny_app_df,file='../data/shiny_app_data.Rda')


# Get some summary statistics about data analyzed
count_df <- data.frame(matrix(ncol=5,nrow=0,dimnames=list(NULL,c('restaurant', 'elite_review_count','non_elite_review_count', 'elite_rating_count', 'non_elite_rating_count'))))
for(i in 1:length(restaurant_list)) {
    count_df[i,] <- list(restaurant_list[i],
                    dim(review_comp[[i]])[1],
                    dim(rating_comp[[i]])[1],
                    dim(review_comp_non_elite[[i]])[1],
                    dim(rating_comp_non_elite[[i]])[1])
}

sum(count_df[,'elite_review_count']) +
sum(count_df[,'non_elite_review_count'])
min(reviews_df[,'date'])
max(reviews_df[,'date'])
