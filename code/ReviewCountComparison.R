library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)
library(ggplot2)
library(stringr)

# Import our Data
business_df <- stream_in(file('../data/yelp_dataset_2022/business.json'))
user_df <- read.csv('../data/csv_dataset/elite_users.csv')
reviews_df <- read.csv('../data/csv_dataset/reviews_no_text.csv')

review_count_comparison <- function(business_df, reviews_df, user_df, category1,
                                    category2, day_range) {
    # Get list of businesses by specific business type
    business_subset <- business_df[(str_detect(tolower(business_df$categories), tolower(category1))) &
                                        (str_detect(tolower(business_df$categories), tolower(category1))),]
    
    # Filter reviews down to the specified list
    reviews_subset <- reviews_df[reviews_df$business_id %in% business_subset$business_id,]
    
    # Join users to reviews
    review_users_df = merge(x=reviews_subset, y=user_df, by='user_id')
    
    # Convert date column to a date type
    review_users_df$date_format <- as.Date(ymd_hms(review_users_df[,'date'], truncated=6))
    review_users_df['year'] <- year(review_users_df[,'date_format'])
    
    # Format the string column to be a date
    reviews_df$date_format <- ymd_hms(reviews_df[,'date'],truncated=6)
    reviews_df$date_format <- as.Date(reviews_df$date_format)
    
    # Add year column since elite status is by year
    reviews_df['year'] <- year(reviews_df[,'date_format'])
    reviews_df$day <- as.Date(reviews_df$date_format)
    
    # Check if user is elite when posting review
    review_users_df['flag'] <- mapply(grepl, review_users_df$year, review_users_df$elite)
    
    # Get only reviews when the user was elite
    elite_reviews_df <- review_users_df[review_users_df['flag']==TRUE,]
    
    
    # Group Number of reviews by businesses and days
    review_business_agg <- aggregate(reviews_df$X, 
                                     by=list(business_id = reviews_df$business_id,
                                             day = reviews_df$day),
                                     FUN=length)
    
    setDT(review_business_agg)
    
    # Add beginning and end dates for the specified date range 
    review_business_agg[, date_lb := day - day_range]
    review_business_agg[, date_ub := day + day_range]
    
    # Calculate the number of reviews before the elite review
    review_business_before <- review_business_agg[review_business_agg[, .(business_id, day, date_lb)], on=.(business_id, day<day, day >= date_lb)] %>% .[, lapply(.SD, sum), by=.(day,business_id), .SDcols = 'x']
    review_business_before[is.na(review_business_before), ] <- 0
    setnames(review_business_before, 'x', 'reviews_before')
    
    # Calculate the number of reviews after the elite review
    review_business_after <- review_business_agg[review_business_agg[, .(business_id, day, date_ub)], on=.(business_id, day>day, day <= date_ub)] %>% .[, lapply(.SD, sum), by=.(day,business_id), .SDcols = 'x']
    review_business_after[is.na(review_business_after), ] <- 0
    setnames(review_business_after, 'x', 'reviews_after')
    
    setDT(elite_reviews_df)
    
    # Merge tables to include review counts before and after
    reviews_comparison <- elite_reviews_df[review_business_before[,.(business_id,day, reviews_before)], on=c(business_id='business_id', date_format = 'day'), nomatch=0]
    reviews_comparison <- reviews_comparison[review_business_after[,.(business_id,day, reviews_after)], on=c(business_id='business_id', date_format = 'day'), nomatch=0]
    
    # Glance at the means
    print(mean(reviews_comparison$reviews_before))
    print(mean(reviews_comparison$reviews_after))
    
    # t-test (Paired)
    t.test(reviews_comparison$reviews_before, reviews_comparison$reviews_after, paired=TRUE, alternative = 'less')
    
    return(reviews_comparison)
}

print("Chinese Restaurants")
chinese_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                'Chinese',
                                                'Restaurant',
                                                30)

print("Japanese Restaurants")
japanese_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Japanese',
                                                 'Restaurant',
                                                 30)

print("American Restaurants")
american_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                'American',
                                                'Restaurant',
                                                30)

print("Burger Restaurants")
burgers_30_comparison <- review_count_comparison(business_df, reviews_df, user_df,
                                                 'Burgers',
                                                 'Restaurant',
                                                 30)


