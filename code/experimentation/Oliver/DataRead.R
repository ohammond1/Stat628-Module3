library(jsonlite)
library(lubridate)
library(dplyr)
library(data.table)


business_df <- stream_in(file('../data/yelp_dataset_2022/business.json'))
user_df <- read.csv('../data/csv_dataset/elite_users.csv')
reviews_df <- read.csv('../data/csv_dataset/reviews_no_text.csv')

# Join users to reviews
review_users_df = merge(x=reviews_df, y=user_df, by='user_id')

# Convert date column to a date type
review_users_df$date_format <- ymd_hms(review_users_df[,'date'], truncated=6)
review_users_df['year'] <- year(review_users_df[,'date_format'])

reviews_df$date_format <- ymd_hms(reviews_df[,'date'],truncated=6)
reviews_df$date_format <- as.Date(reviews_df$date_format)
reviews_df['year'] <- year(reviews_df[,'date_format'])
reviews_df$day <- as.Date(reviews_df$date_format)

review_users_df['flag'] <- mapply(grepl, review_users_df$year, review_users_df$elite)

# Get only reviews when the user was elite
elite_reviews_df <- review_users_df[review_users_df['flag']==TRUE,]

# Group Number of reviews by businesses and days

review_business_agg <- aggregate(reviews_df$X, 
                       by=list(business_id = reviews_df$business_id,
                                       day = reviews_df$day),
                       FUN=length)
unique(review_business_agg$x)

# Create the date range
day_range <- 30
elite_reviews_df$after_end_date <- elite_reviews_df$date_format + day_range
elite_reviews_df$before_start_date <- elite_reviews_df$date_format - day_range

get_prior_reviews <- function(business_id, before_start_date, review_date, agg_df) {
    prior <- sum(agg_df[(agg_df$business_id == business_id) &
                    (agg_df$day >= before_start_date) & 
                    (agg_df$day < review_date),]$x)
    return(prior)
}

apply(elite_reviews_df[1:100,c('business_id','before_start_date','date_format')],1, 
                                             function(x) get_prior_reviews(x['business_id'], x['before_start_date'], x['date_format'], review_business_agg))


setDT(review_business_agg)

review_business_agg[, date_lb := day - day_range]
review_business_agg[, date_ub := day + day_range]

review_business_before <- review_business_agg[review_business_agg[, .(business_id, day, date_lb)], on=.(business_id, day<day, day >= date_lb)] %>% .[, lapply(.SD, sum), by=.(day,business_id), .SDcols = 'x']
review_business_before[is.na(review_business_before), ] <- 0

review_business_after <- review_business_agg[review_business_agg[, .(business_id, day, date_lb)], on=.(business_id, day<day, day >= date_lb)] %>% .[, lapply(.SD, sum), by=.(day,business_id), .SDcols = 'x']
review_business_after[is.na(review_business_after), ] <- 0


