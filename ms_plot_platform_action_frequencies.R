# Master script: plots of platform action frequencies, including program and 
# assessment starts.

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(plotly)
library(chron)
library(utils)
library(htmlwidgets)

source("fn_bar_chart_layout.r")
source("fn_save_or_print.r")

save.plots <- T
saveloc <- "/Users/johnhower/Google Drive/Analytics_graphs"

# Read in the segmented/classified data frames ####

user_facts <- 
  read.table(
    "user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

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
  mutate(date = as.Date(date,format="%Y-%m-%d"))

#

# User Subsets ####

# 1. Standard subset
# End users who have never taken a "champion/internal only" platform action, and who
# don't belong to the list of flagged champions.

standard_user_subset <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    dont.exclude
    , 
    !fake_end_user
    , 
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

#

# user_platformaction_datetime$platform_action %>% unique

# Prepare data for plots ####
subset_pafreq <- standard_user_subset

plot_data_pafreq <- user_platformaction_datetime %>%
  filter(
    user_id %in% subset_pafreq
    , date >= "2016-01-01") %>%
  merge(
    select(platformaction_facts, platform_action, group)
  ) %>%
  group_by(group) %>%
  summarise(
    number_of_users = length(unique(user_id))
    , number_of_actions = n()
  )

# Make plots ####

# Number of users
plotname <- "content_start_count_by_users"

plot_data_pafreq %>%
  filter(
    !(group %in% c("Started Session", "Account Created", ""))
    , group %in% c("Started Moment", "Started Assessment", "Started Program", "FamilyLife - Listened to Broadcast")
  ) %>%
  arrange(desc(number_of_users)) %>%
  plot_ly(x = group, y = number_of_users, type = "bar") %>%
  bar_chart_layout(
    charttitle = "Platform Action Frequency"
    , yaxistitle = "Number of Users"
    , bottommargin = 150
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = saveloc
    , plot_name = plotname
  )

# Number of times taken
plotname <- "content_start_count_by_actions" 

plot_data_pafreq %>%
  filter(
    !(group %in% c("Started Session", "Account Created", ""))
    , group %in% c("Started Moment", "Started Assessment", "Started Program", "FamilyLife - Listened to Broadcast")
  ) %>%
  arrange(desc(number_of_actions)) %>%
  plot_ly(x = group, y = number_of_actions, type = "bar") %>%
  bar_chart_layout(
    charttitle = "Content Starts"
    , yaxistitle = "Number of Actions"
    , bottommargin = 150
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = saveloc
    , plot_name = plotname
  )