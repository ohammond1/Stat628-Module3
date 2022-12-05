# Stat628-Module3

The project aims to analyze the effect of reviews of Yelp incluencers on overall ratings and reviews of businesses. The analysis informs business owners of potential benefits of inviting Yelp influencers to visit the business and to write reviews.

We picked top 5% if Yelp influencers (or Yelp elites) and filtered out businesses that they reviewed. For each business, we looked at all reviews 1 month before and 1 month after the influencer visited the business and investigated the change in the number of reviews and average ratings.

## Data

The majority of the data is not stored on github due to size limitations. The files referenced in the preprocessing scripts are assumed to be stored in the data directory and are from the dropbox provided. During preprocessing additional csvs are saved and used for future analysis. These are once again saved into the data directory.

## Code

Our code is split between both R and python, due to difficulties loading the initial data into R we did the majority of preprocessing in python, but due to comfortability most of the analysis was conducted in R.

### File Description
- `ReviewPreprocess.ipynb` 
  - Jupyter notebook that takes in the `user.json` and and `review.json` provided in the drop box opens them and does preprocessing. These files are opened from the `data/yelp_dataset_2022` directory. This results in output files of `elite_users.csv`, `users.csv`, and `elite_reviews_sample.csv`. The output files are saved to the `data/` directory
- `comparison_functions.R`
  - Helper functions for `ReviewCountComparison.R` for processing dataframes to calculate statistics in a date range before and date range after reviews were written.
- `ReviewCountComparison.R`
  - File completing analysis of paired t-test of number of reviews and average ratings of reviews, and difference-in-difference analysis. Also includes some code for creating plots for executive report and presentation, and summary statistics.
- `UserAnalysis.R`
  - File for doing EDA of the user comparisons, and plots
- `sentiment.R`
  - File for completing the sentiment analysis correlation between elite user reviews and average rating.
- `chongxuan.ipynb`
  - File for doing paired t-test on a sample of the entire restaurant reviews instead of by category. Also is completing an analysis of the distributions.

The `code/experimentation` files are older files that were made during initial data exploration and testing.

## Images 
Contains all of the images created for our analysis.
