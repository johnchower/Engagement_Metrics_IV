# Master Script: content starts

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(plotly)
library(chron)
library(utils)

source("fn_bar_chart_layout.r")

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

user_platformaction_datetime$platform_action %>% unique

# Downloading program and assesment start data.

path_program_starts = "input_csvs/program_starts"
  
program_starts <- path_program_starts %>%
  name_as_looker_output %>%
  grep(dir(recursive = T), value = T) %>%
  read.csv(
    header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  rename(
    user_id = User.Dimensions.ID
    , program_id = Program.Dimensions.ID
    , date = Date.Dimensions.Content.Progress.Date
    , time = Date.Dimensions.Content.Progress.Time
  ) %>%
  select(-time) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))
  

path_assessment_starts = "input_csvs/assessment_engagement_metrics_IV"
  
assessment_starts <- path_assessment_starts %>%
  name_as_looker_output %>%
  grep(dir(recursive = T), value = T) %>%
  read.csv(
    header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  rename(
    user_id = User.Dimensions.ID
    , date = Date.Dimensions.Assessment.Date
    , assessment_id = Assessment.Dimensions.ID
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

# Summarising to get start dates

program_starts %<>%
  group_by(user_id, program_id) %>%
  summarise(
    date = min(date)
    , platform_action = "Started Program"
  )

assessment_starts %<>%
  group_by(user_id, assessment_id) %>%
  summarise(
    date = min(date)
    , platform_action = "Started Assessment"
  )