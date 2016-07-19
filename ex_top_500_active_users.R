# Exploration: top 500 most active users

# Set parameters ####

use_true_champid <- T # Pick a version of the user table to base calculations on.
combine_Cru <- T      # Combine all Cru champions under one umbrella?
writeloc <- "/Users/johnhower/Google Drive/Analytics_graphs/top_500_active_users"
remove_active_actions_from_top3 <- T
output_graph_format <- "html"
user_cutoffs <- c(10, 50, 100, 250, 500)

# Chart Names
active_casual_core_chart_name <- "avg_pct_active_casual_core"
dau_to_mau_vs_time_chart_name <- "avg_engagement_rate_vs_time"
dau_to_mau_current_chart_name <- "avg_engagement_rate_by_champion_current"
dau_to_mau_ytd_chart_name <- "avg_engagement_rate_by_champion_ytd"



#
# Load packages and functions ####


library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(scales)
library(magrittr)
library(webshot)
library(htmlwidgets)
library(stringdist)
library(chron)

layout <- plotly::layout

source("fn_bar_chart_layout.r")
source("fn_save_or_print.r")
source("fn_name_as_looker_output.r")
source("fn_chisquare_analysis.r")

# Load Data ####

champion_facts <- 
  read.table(
    "champion_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) 

platformaction_facts <- 
  read.table(
    "platformaction_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  )

date_user_table <- 
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_platformaction_datetime <- 
  read.table(
    "user_platformaction_datetime.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(datetime = chron(dates. = date, times. = time, format = c(dates = "y-m-d", times = "h:m:s"))) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_demographics <- 
  "input_csvs/user_demographics" %>%
  name_as_looker_output %>%
  grep(dir(recursive = T), value = T) %>%
  read.table(
    header = T
    , sep = ','
    , stringsAsFactors = F
  ) %>%
  rename(
    user_id = User.Dimensions.ID
    , age = User.Dimensions.Age
    , age_tier = User.Dimensions.Age.Tier
    , user_email = User.Dimensions.Email
    , gender = User.Dimensions.Gender
    , relationship_status = User.Dimensions.Relationship.Status
  ) %>% 
  mutate(email_domain = tolower(gsub("\\S+(@)", "",user_email)))

userid_cohortid <-
  "userid_cohortid.csv" %>%
  read.table(
    header = T
    , sep = ','
    , stringsAsFactors = F
  )

cohortid_cohortname <-
  "cohortid_cohortname.csv" %>%
  read.table(
    header = T
    , sep = ','
    , stringsAsFactors = F
  )

# Read in the user_facts table ####

user_facts <- 
  read.table(
    ifelse(use_true_champid, "user_table_true_first.csv", "user_table.csv")
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_facts %<>%
  merge(
    select(user_demographics, -user_email) # Had to take out email b/c the email values don't agree between tables
  )

# User Subsets ####

# 1. Standard subset
# End users who have never taken a "champion/internal only" platform action, and who
# don't belong to the list of flagged champions.

standard_user_subset <- user_facts %>% 
#  merge(select(champion_facts, champion_id, dont.exclude)) %>% 
  filter(
    account_type == "End User"
  ) %>% 
  {.$user_id} 

# 2. REVEAL for Church

REVEAL_Church_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "REVEAL For Church") %>%
  {.$user_id}

# 3. FamilyLife

FamilyLife_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "FamilyLife") %>%
  {.$user_id}

# Determining who the top 500, 250, 100, 50, and 10 are ####
# Measure: Number of active days in last month

user_activitymetrics_pastmonth <- user_platformaction_datetime %>%
  filter(date > max(date) - 28) %>%
  group_by(user_id) %>%
  summarise(
    number_of_actions_past_month = n()
    , number_of_active_days_past_month = length(unique(date))
  )

top_users_list <- user_cutoffs %>%
  lapply(
    FUN = function(i){
      user_activitymetrics_pastmonth %>%
        filter(user_id %in% standard_user_subset) %>%
        arrange(desc(number_of_actions_past_month)) %>%
        slice(1:i) %>%
        {.$user_id} %>% return
    }
  ) %>%
  {
    names(.) <- paste("top", user_cutoffs, sep = "_")
    return(.)
  }

# Primary Champions
top_user_facts <- user_facts %>%
#  filter(user_id %in% standard_user_subset) %>%
  merge(
    select(user_activitymetrics_pastmonth, user_id, number_of_actions_past_month)
    , all = T
  ) %>%
  arrange(desc(number_of_actions_past_month)) %>%
  mutate(
    in_top_10 = user_id %in% top_users_list$top_10
    , in_top_50 = user_id %in% top_users_list$top_50
    , in_top_100 = user_id %in% top_users_list$top_100
    , in_top_250 = user_id %in% top_users_list$top_250
    , in_top_500 = user_id %in% top_users_list$top_500
  )

# Number of champions connected to

top_user_facts <- user_platformaction_datetime %>%
  merge(
    select(champion_facts, champion_id, dont.exclude)
  ) %>%
  filter(
    grepl("Connected to Champion", platform_action)
    , dont.exclude
  ) %>%
  group_by(user_id) %>%
  summarise(number_of_champion_connections = n()) %>%
  merge(top_user_facts, all.y = T) %>% 
  {
    .$number_of_actions_past_month[is.na(.$number_of_actions_past_month)] <- 0
    return(.)
  } %>%
  arrange(desc(number_of_actions_past_month)) %>%
  mutate(gave_age = !is.na(age)) 

# Which champions have been connected to? ####

topuser_champion_list <- top_users_list %>%
  lapply(
    FUN = function(u){
      user_platformaction_datetime %>%
        merge(select(champion_facts, champion_id, dont.exclude, champion_name)) %>%
        filter(
          user_id %in% u
          , grepl("Connected to Champion", platform_action)
          , dont.exclude
        ) %>%
        select(user_id, champion_id, champion_name) %>%
        unique %>%
        group_by(champion_id, champion_name) %>%
        summarise(number_of_users = n()) %>%
        arrange(desc(number_of_users)) %>%
        return
    }
  )

# What cohorts do the top users belong to? ####

topuser_cohort_list <- top_users_list %>%
  lapply(
    FUN = function(u){
      userid_cohortid %>%
        filter(user_id %in% u) %>%
        group_by(cohort_id) %>%
        summarise(number_of_users = n()) %>%
        merge(cohortid_cohortname) %>%
        arrange(desc(number_of_users)) %>%
        select(-X) %>%
        return
    }
  )


