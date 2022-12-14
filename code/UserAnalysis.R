library(ggplot2)

all_user_df <- read.csv('../data/users.csv')
reviews_df <- read.csv('../data/csv_dataset/reviews_no_text.csv')

user_df$elite_user <- user_df$elite != ''

review_users_df = merge(x=reviews_df, y=user_df, by='user_id')

ggplot(data=user_df, aes(elite_user)) +
    geom_bar(aes(y=(..count..)/sum(..count..), fill=elite_user), show.legend = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    ylab("Percentage of Users") +
    xlab("User Type") +
    ggtitle("Percentage of Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=12)) +
    scale_x_discrete(labels=c("Normal","Elite"))


ggplot(data=user_df, aes(x=elite_user, y=review_count)) +
    geom_bar(stat='identity', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Number of Reviews") +
    xlab("User Type") +
    ggtitle("Number of reviews by Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))

    

ggplot(data=user_df, aes(x=factor(elite_user), y=useful)) +
    geom_bar(stat='summary', fun='mean', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Average 'Useful' per Review") +
    xlab("Elite User") +
    ggtitle("Usefulness of Reviews by Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))



ggplot(data=user_df, aes(x=factor(elite_user), y=funny)) +
    geom_bar(stat='summary', fun='mean', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Average 'Funny' per Review") +
    xlab("Elite User") +
    ggtitle("Funniness of Reviews by Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))



ggplot(data=user_df, aes(x=factor(elite_user), y=cool)) +
    geom_bar(stat='summary', fun='mean', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Average 'Cool' per Review") +
    xlab("Elite User") +
    ggtitle("Coolness of Reviews by Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))


ggplot(data=user_df, aes(x=factor(elite_user), y=fans)) +
    geom_bar(stat='summary', fun='mean', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Average 'Fans' per Reviewer") +
    xlab("Elite User") +
    ggtitle("Number of Average Fans of Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))


ggplot(data=user_df, aes(x=factor(elite_user), y=fans)) +
    geom_bar(stat='summary', fun='mean', aes(fill=elite_user), show.legend = FALSE) +
    ylab("Average 'Fans' per Review") +
    xlab("Elite User") +
    ggtitle("Number of Average Fans of Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size=10)) +
    scale_x_discrete(labels=c("Normal","Elite"))



star_reviews <- aggregate(review_users_df$stars,
                          by=list(Stars=review_users_df$stars,
                                  Elite_User=review_users_df$elite_user),
                          FUN=length)
total_reviews <- aggregate(star_reviews$x,
                           by=list(star_reviews$Elite_User),
                           FUN=sum)

star_reviews <- merge(x=star_reviews,y=total_reviews, by.x='Elite_User', by.y='Group.1')
star_reviews$percentage = star_reviews$x.x / star_reviews$x.y

ggplot(data=star_reviews, aes(x=Stars, y=percentage, fill=Elite_User)) +
    geom_bar(stat='identity', position='dodge') +
    scale_y_continuous(label=scales::percent) +
    ylab("Number of Reviews") +
    xlab("Star Rating") +
    ggtitle("Review Distribution of Elite vs Non-Elite Users") +
    theme(plot.title = element_text(hjust = 0.5),aspect.ratio = 6/10)+
    scale_fill_discrete(name="User Type", labels=c("Normal","Elite"))
