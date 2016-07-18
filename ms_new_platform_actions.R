# Master script: New platform actions

# Whenever new platform actions are added, we have to re-categorize them
# according to group and mode. This scrit (sort of) automates this process by detecting 
# new platform actions that don't appear in platform_action_list.csv and opening
# an edit window to allow you to categorize. 
# Run this script after ms_segment_and_classify

library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(plyr)
library(dplyr)
library(chron)
library(plotly)

path_platform_action_list <- 
   "input_csvs/platform_action_list.csv"

platform_action_list <- 
  read.table(
    path_platform_action_list
    , header = T
    , sep = ","
  ) %>%
  rename(platform_action = User.Platform.Action.Facts.Platform.Action)

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

platformaction_facts %<>%
  merge(platform_action_list, all = T) %>% 
  arrange(!is.na(X), X) %>%
  {
    if(
      nrow(filter(., is.na(X))) > 0
    ){edit(.)}
    else {return(.)}
  } 

platformaction_facts %<>%
  select(-X) %>%
  {
    .[is.na(.)] <- ""
    return(.)
  }

