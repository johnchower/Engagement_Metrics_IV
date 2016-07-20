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

source('fn_name_as_looker_output.r')
use_editor <- T

path_platform_action_list <- 
   "input_csvs/platform_action_list.csv"

path_platform_action_list_new <-
  "input_csvs/platform_action_list"

platform_action_list <- 
  read.table(
    path_platform_action_list
    , header = T
    , sep = ","
  ) %>%
  rename(platform_action = User.Platform.Action.Facts.Platform.Action)

platform_action_list_new <- path_platform_action_list_new %>%
  name_as_looker_output %>%
  grep(dir(recursive = T), value = T) %>%
  read.table(
    header = T
    , sep = ","
    , stringsAsFactors = F
  ) %>%
  rename(platform_action = User.Platform.Action.Facts.Platform.Action)

# Read in the segmented/classified data frames ####

platformaction_facts <- 
  read.table(
    "platformaction_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  )
#

platformaction_facts %<>%
  merge(platform_action_list_new, all = T) %>% 
#  arrange(!is.na(X), X) %>%
  arrange(platform_action) %>%
  {
    if(
      nrow(filter(., is.na(X))) > 0
      & use_editor
    ){
      requires_edit <- F
      edit(.)
    }
    else if(nrow(filter(., is.na(X))) > 0){
      requires_edit <- T
      print("Edit platformaction_facts before proceeding.")
      return(.)
    }
    else {
      requires_edit <- F
      print("No editing required.")
      return(.)
    }
  } 

# Run this code snippet after updating platformaction_facts
if(
  nrow(filter(platformaction_facts, is.na(X))) == 0
) {
    platformaction_facts %<>%
      select(-X) %>%
      {
        .[is.na(.)] <- ""
        return(.)
      }
    
    write.csv(
      x = platformaction_facts
      , file = "input_csvs/platformaction_facts.csv"
    )
}



