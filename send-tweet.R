rm(list = ls())
gc()

# To Do
# catch duplicate names and use most recent or fighter with most bouts (print warning in tweet?)

library(data.table)
library(ggplot2)
library(scales)
library(ggpubr)
library(grid)
library(RColorBrewer)
library(ggdark)
library(ggpattern)
library(rtweet)

# source functions
source('./ufc-matchup-comparison-functions.R')

# Create Twitter token
twitter_bot_token <- rtweet::rtweet_bot(
  api_key       = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  api_secret    = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token  = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

#--------------
# READ IN DATA
#--------------

# career stats
career_df <- fread('./data/careers.csv')

# recent outcomes
recent_df <- fread('./data/recent.csv')

# prior max ID
prior_max_id <- fread('./supplementary-files/ufc-matchup-max-id-reply.csv')

# as of date
as_of_date <- recent_df[, format.Date(max(Date), format = '%B %d, %Y')]

#-----------------
# COLLECT TWEETS
#-----------------

# set hashtags
matchup_hashtag <- '#UFCMatchupRequest'

# collect tweets
matchup_tweets <- data.table(search_tweets(q = matchup_hashtag, token = twitter_bot_token, since_id = prior_max_id[, as.character(id_str)]))

# subset tweets to include proper tag
#matchup_tweets <- matchup_tweets[grepl('@MMAStatsBot', full_text)]

#---------------------
# LOOP THROUGH TWEETS
#---------------------

if(matchup_tweets[, .N] > 0){
  for(i in 1:matchup_tweets[, .N]){
    # select tweet
    tweet_df <- matchup_tweets[i]
    tweet_text <- tweet_df$full_text
    tweet_id <- tweet_df$id_str
    
    # parse tweet for fighter names
    tweet_text <- gsub(paste0('.*', matchup_hashtag, ' '), '', tweet_text)
    tweet_text <- gsub('\\..*', '', tweet_text)
    f1_name <- gsub(' vs .*', '', tweet_text)
    f2_name <- gsub('.* vs ', '', tweet_text)
    
    # check for proper number of names
    name_check_pass <- career_df[Name %in% c(f1_name, f2_name), uniqueN(Link)] == 2 & recent_df[Name %in% c(f1_name, f2_name), uniqueN(Link)] == 2
    
    # ensure that names yield exactly 2 UFC fighters, otherwise send tweet with error
    if(name_check_pass){
      
      # build figure
      matchup_figure <- UFC_Matchup_Comparison(career_df, recent_df, f1_name, f2_name)
      
      # save figure
      matchup_file <- './supplementary-files/ufc-matchup-comparison.png'
      
      png(matchup_file, height = 1200, width = 1000, res = 100, bg = 'white')
      print(matchup_figure)
      dev.off()
      
      # send tweet
      matchup_reply_text <- paste0('Thanks for your request!\n\n', 
                                   'Here is your #UFC Matchup Comparison for:\n\n', f1_name, ' vs ', f2_name, 
                                   '\n\nFollow @NateLatshaw for more #MMA insights via stats, figures, & more!')
      matchup_media_alt_text <- 'UFC Matchup Comparison figure'
      
    } else {
      
      # send tweet flagging error in parsing tweet
      matchup_file <- './supplementary-files/how-to-screenshot.png'
      matchup_reply_text <- paste0('Unfortunately, I could not process your request.\n\n', 
                                   'Are you sure you correctly followed all steps?\n\n', 
                                   'See all tips & reminders in the figure below to troubleshoot.\n\n', 
                                   'Please reach out to @NateLatshaw for further assistance.')
      matchup_media_alt_text <- 'UFC Matchup Comparison figure error'
      
    }
    
    # send tweet
    post_tweet(status = matchup_reply_text, 
               media = matchup_file, 
               media_alt_text = matchup_media_alt_text, 
               in_reply_to_status_id = tweet_id, 
               token = twitter_bot_token)
    
  }
  
  # save max ID of tweets replied to
  matchup_max_id <- matchup_tweets[id == max(id), .(id_str)]
  fwrite(matchup_max_id, './supplementary-files/ufc-matchup-max-id-reply.csv')
  
}

