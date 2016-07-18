# Master Script - segment and classify

# Packages ####

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(plotly)
library(chron)
library(utils)

# Functions ####

source("fn_get_user_streaks.r")
source("fn_moving_sum.r")
source("fn_get_reactivated01.r")
source("fn_get_wentdormant01.r")
source("fn_segment_user.r")

source("fn_get_fake_end_users.r")
source("fn_load_basic_schema.r")
source("fn_name_as_looker_output.r")

# Start the timer ####

starttime <- Sys.time()

# Load basic schema ####

df.list <- load_basic_schema()

basic_user_datetime_platformaction <- df.list[[1]]
basic_user <- df.list[[2]]
basic_champ <- df.list[[3]]
basic_platformaction <- df.list[[4]]
basic_program_starts <- df.list[[5]]
basic_assessment_starts <- df.list[[6]]
basic_programid_champid <- df.list[[7]]
basic_assessmentid_champid <- df.list[[8]]
basic_cohortid_cohortname <- df.list[[9]]
basic_userid_cohortid <- df.list[[10]]
rm(load_basic_schema, df.list)

# Add program and assessment starts to platform action list ####

# Get rid of the mutate(champion_id = "", isfirst = F) line. 
# Instead, download the content progress adn assessment fact tables from Looker,
# so that you can match each piece of content with the corresponding champion.
# Then, use the user fact table to decide on the isfirst column.

program_starts <- basic_program_starts %>%
  group_by(user_id, program_id) %>%
  summarise(
    datetime = min(datetime)
    , platform_action = "Started Program"
  ) %>%
  mutate(
    date = as.Date(format(datetime, "%Y-%m-%d"))
    , time = substr(datetime, 11, 18)
  ) %>%
  merge(basic_programid_champid) %>%
  {
    ddply(
      .
      , .variables = colnames(.)
      , .fun = function(df)
      {
        u <- df$user_id[1]
        c <- df$champion_id[1]
        out <- nrow(basic_user[basic_user$user_id == u & basic_user$champion_id == c,]) >0
        return(cbind(df, isfirst = out))
      }
    )
  }

assessment_starts <- basic_assessment_starts %>%
  group_by(user_id, assessment_id) %>%
  summarise(
    datetime = min(datetime)
    , platform_action = "Started Assessment"
  ) %>%
  mutate(
    date = as.Date(format(datetime, "%Y-%m-%d"))
    , time = substr(datetime, 11, 18)
  ) %>%
  merge(basic_assessmentid_champid) %>%
  {
    ddply(
      .
      , .variables = colnames(.)
      , .fun = function(df)
      {
        u <- df$user_id[1]
        c <- df$champion_id[1]
        out <- nrow(basic_user[basic_user$user_id == u & basic_user$champion_id == c,]) >0
        return(cbind(df, isfirst = out))
      }
    )
  }

basic_user_datetime_platformaction %<>%
  rbind(
    select(program_starts, -program_id)
    , select(assessment_starts, -assessment_id)
  )




# Aggregate basic_platformaction into user_platformactiondate ####


alldates <- basic_user_datetime_platformaction %>%
  {seq.Date(min(.$date), max(.$date), by = 1)}

user_alldates <-   basic_user %>%
  select(user_id, date) %>%
  rename(createddate = date) %>%
  merge(
    data.frame(date = alldates)
    , all = T
  ) %>% 
  filter(date >= createddate) %>%
  select(-createddate)

user_platformactiondate_activitytoday <- basic_user_datetime_platformaction %>%
  group_by(user_id, date) %>%
  summarise(activitytoday = 1) %>%
  ungroup %>%
  merge(user_alldates, all = T) %>% 
  {
    .$activitytoday[is.na(.$activitytoday)] <- 0
    return(.)
  } 

date_user_table <- user_platformactiondate_activitytoday %>%
  group_by(user_id) %>%
  mutate(
    past7 = moving_sum(activitytoday, 7)
    , 
    past28 = moving_sum(activitytoday, 28)
    , 
    account_created = as.numeric(date == min(date))
    , 
    reactivated = get_reactivated01(past28)
    , 
    churned = get_wentdormant01(past28)
    , 
    segment = segment_user(reactivated, past28, activitytoday)
    , 
    DAU = as.numeric(activitytoday > 0)
    , 
    WAU = as.numeric(past7 > 0)
    , 
    MAU = as.numeric(past28 > 0)
    , 
    dormant_user = as.numeric(MAU == 0)
    , 
    streaks = c(get_user_streaks(activitytoday),rep(0, times = n() - length(get_user_streaks(activitytoday))))
  )

# Add current_segment field to user_table ####

user_table <- date_user_table %>%
  filter(date == max(date)) %>%
  select(user_id, segment) %>%
  merge(
    basic_user
    , all = T # Comment this out to revert to original version.
  ) %>%
  rename(current_segment = segment)

# Check for new platform actions. If there are any, classify them before moving on ####

path_platform_action_list <- 
  "input_csvs/platform_action_list.csv"

platform_action_list <- 
  read.table(
    path_platform_action_list
    , header = T
    , sep = ","
  ) %>%
  rename(platform_action = User.Platform.Action.Facts.Platform.Action)

platformaction_facts <- basic_platformaction %>%
  merge(platform_action_list, all = T) %>% 
  arrange(!is.na(end_user_allowed), end_user_allowed, platform_action) %>%
  {
    if(
      nrow(filter(., is.na(end_user_allowed))) > 0
    ){edit(.)}
    else {return(.)}
  } 

platformaction_facts %<>%
  {
    .[is.na(.)] <- ""
    return(.)
  } %>%
  {
    if("X" %in% colnames(.)){
      return(select(.,-X))
    } else {return(.)}
  }

# Add ambiguous champions to 



# Write calculated tables to disk ####

write.csv(
  x = date_user_table
  , file = "date_user_table.csv"
)

write.csv(
  x = user_table
  , file = "user_table.csv"
)

write.csv(
  x = basic_user_datetime_platformaction
  , file = "user_platformaction_datetime.csv"
)

write.csv(
  x = basic_champ
  , file = "champion_facts.csv"
)

write.csv(
  x = platformaction_facts
  , file = "platformaction_facts.csv"
)

write.csv(
  x = platformaction_facts
  , file = "input_csvs/platformaction_facts.csv"
)

write.csv(
  x = basic_userid_cohortid
  , file = "userid_cohortid.csv"
)

write.csv(
  x = basic_cohortid_cohortname
  , file = "cohortid_cohortname.csv"
)

 

# Clear workspace ####


endtime <- Sys.time()
endtime - starttime

rm(list=ls())
