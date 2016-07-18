# Exploration: Charts for Deanna

  # Average engagement rate
  # Average percentage of active users
  # Average percentage of casual users
  # Average percentage of core users
  # What are the typical actions of active, casual and core users?

# Set parameters ####

use_true_champid <- F # Pick a version of the user table to base calculations on.
combine_Cru <- T      # Combine all Cru champions under one umbrella?
writeloc <- "/Users/johnhower/Google Drive/Analytics_graphs/Engagement_Seminar_Deanna"
remove_active_actions_from_top3 <- T
output_graph_format <- "png"

# Chart Names
active_casual_core_chart_name <- "avg_pct_active_casual_core"
dau_to_mau_vs_time_chart_name <- "avg_engagement_rate_vs_time"
dau_to_mau_current_chart_name <- "avg_engagement_rate_by_champion_current"
dau_to_mau_ytd_chart_name <- "avg_engagement_rate_by_champion_ytd"



#
# Load packages and functions ####


library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(scales)
library(magrittr)
library(webshot)
library(htmlwidgets)

layout <- plotly::layout

source("fn_bar_chart_layout.r")
source("fn_plot_MAU_bar_chart.r")
source("fn_save_or_print.r")

# Read in the segmented/classified data frames ####

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

# Read in the user_facts table ####

user_facts <- 
  read.table(
    ifelse(use_true_champid, "user_table_true_first.csv", "user_table.csv")
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

# Average engagement rate: These charts exist already; just re-run the whole
# thing with new data. (Using "naive" first champ or "true" first champ?)

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

# ####
# Average percentage of active, casual, core users. ####
subset_for_analysis <- standard_user_subset

plotname <- active_casual_core_chart_name
date_user_table %>%
  filter(
    user_id %in% subset_for_analysis
    , date >= "2016-01-01"
  ) %>%
  group_by(date, segment) %>%
  summarise(number_of_users = n()) %>%
  mutate(segment = gsub(" ", "_", segment)) %>%
  dcast(
    date ~ segment
    , value.var = "number_of_users"
  ) %>% 
  {
    .[is.na(.)] <- 0
    return(.)
  } %>%
  mutate(
    active_user_count = Casual + Core + Marginal + New + Reactivated
    , inactive_user_count = One_Session_Only + Dormant
    , total_user_count = active_user_count + inactive_user_count
    , active_user_pct = active_user_count/total_user_count
    , core_user_pct = Core/total_user_count
    , casual_user_pct = Casual/total_user_count
  ) %>% 
  summarise(
    Active = mean(active_user_pct)
    , Casual = mean(casual_user_pct)
    , Core = mean(core_user_pct)
  ) %>%
  melt(
    variable.name = "user_state"
    , value.name = "mean_pct_of_users_2016"
  ) %>% 
  arrange(desc(mean_pct_of_users_2016)) %>% 
  plot_ly(
    x = user_state
    , y = mean_pct_of_users_2016
    , type = "bar"
    , text = paste(round(100*mean_pct_of_users_2016,1), "%", sep = "")
    , hoverinfo = "x+text"
  ) %>% 
  bar_chart_layout(
    charttitle = "Average Percent of Active, Casual, and Core Users in 2016"
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = T
    , outloc = writeloc
    , plot_name = plotname
    , outformat = output_graph_format
  )
# ####
# Average engagement rate ####
# DAU/MAU ratio, through time ####

DAU_to_MAU_subset <- standard_user_subset


DAU_to_MAU_daily_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_subset) %>%
  filter(
    date >= as.Date(format(Sys.Date(), "%Y-01-01"))
    , date < as.Date(format(Sys.Date(), "%Y-%m-01"))
  ) %>%
  select(user_id, date, DAU, MAU) %>% 
  group_by(date) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup 

DAU_to_MAU_monthly_data <- DAU_to_MAU_daily_data %>%
  mutate(
    month = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  group_by(month) %>%
  summarise(DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio))

DAU_to_MAU_overall <- DAU_to_MAU_daily_data %>%
{.$DAU_to_MAU_ratio} %>%
  mean %>%
  round(3) %>%
  percent

plotname <- dau_to_mau_vs_time_chart_name
DAU_to_MAU_monthly_data %>%
  plot_ly(type = "bar", x = month, y = DAU_to_MAU_ratio
          , text = paste(round(100*DAU_to_MAU_ratio,1), "%", sep = "")
          , hoverinfo = "x+text"
  ) %>%
  bar_chart_layout(
    yaxisformat = "%"
    , leftmargin = 200
    , charttitle = paste("Ratio of Daily to Monthly Active Users (Overall ratio for 2016 is ", as.character(DAU_to_MAU_overall), ")", sep = "")
  ) %>%
  save_or_print(
    save_plots = T
    , outloc = writeloc
    , plot_name = plotname
    , outformat = output_graph_format
  )
# ####
# Current DAU/MAU ratio, by champion (data done, still needs plotting)####
DAU_to_MAU_by_champion_subset <- standard_user_subset



DAU_to_MAU_by_champion_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_by_champion_subset) %>%
  filter(
    date > max(date) - 7
    , 
    date <= max(date)
  ) %>%
  merge(
    select(user_facts, user_id, champion_id)
  ) %>%
  {
    if(combine_Cru){
      out <- 
        merge(.,
              select(champion_facts, champion_id, champion_organization)  
        )
      rename(out, champion_name = champion_organization)
    } else{
      out <-
        merge(.,
              select(champion_facts, champion_id, champion_name)  
        )
    }
  } %>%
  group_by(date, champion_name) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup %>%
  group_by(champion_name) %>%
  summarise(overall_DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio, na.rm = T)) %>%
  ungroup %>%
  arrange(desc(overall_DAU_to_MAU_ratio))

plotname <- dau_to_mau_current_chart_name
DAU_to_MAU_by_champion_data %>%
  plot_ly(type = "bar", x = champion_name, y = overall_DAU_to_MAU_ratio
          , text = paste(round(100*overall_DAU_to_MAU_ratio,1), "%", sep = "")
          , hoverinfo = "x+text"
  ) %>%
  bar_chart_layout(
    charttitle = "Current DAU/MAU Ratio, by Champion"
    , bottommargin = 400
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = T
    , outloc = writeloc
    , plot_name = plotname
    , outformat = output_graph_format
  )
# ####
# Year-to-date DAU/MAU ratio, by champion ####
DAU_to_MAU_by_champion_subset <- standard_user_subset

DAU_to_MAU_by_champion_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_by_champion_subset) %>%
  filter(
    date > as.Date(format(max(date), "%Y-01-01"))
    , 
    date <= max(date)
  ) %>%
  merge(
    select(user_facts, user_id, champion_id)
  ) %>%
  {
    if(combine_Cru){
      out <- 
        merge(.,
              select(champion_facts, champion_id, champion_organization)  
        )
      rename(out, champion_name = champion_organization)
    } else{
      out <-
        merge(.,
              select(champion_facts, champion_id, champion_name)  
        )
    }
  } %>%
  group_by(date, champion_name) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup %>%
  group_by(champion_name) %>%
  summarise(overall_DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio, na.rm = T)) %>%
  ungroup %>%
  arrange(desc(overall_DAU_to_MAU_ratio))

plotname <- dau_to_mau_ytd_chart_name
DAU_to_MAU_by_champion_data %>%
  plot_ly(type = "bar", x = champion_name, y = overall_DAU_to_MAU_ratio
          , text = paste(round(100*overall_DAU_to_MAU_ratio,1), "%", sep = "")
          , hoverinfo = "x+text") %>%
  bar_chart_layout(
    charttitle = paste("Year-to-Date DAU/MAU Ratio, by Champion (", format(max(date_user_table$date), "%Y"), ")", sep = "")
    , bottommargin = 400
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = T
    , outloc = writeloc
    , plot_name = plotname
    , outformat = output_graph_format
  )
# ####
# Typical actions of active, casual and core users by raw number of actions ####

platform_action_counts <- user_platformaction_datetime %>%
  filter(
    user_id %in% standard_user_subset
    , !(platform_action %in% c("Account Created", "Started Session"))
    , date >= "2016-01-01"
  ) %>%
  merge(select(user_facts, user_id, current_segment)) %>%
  merge(select(platformaction_facts, platform_action, group)) %>%
  filter(group != "") %>%
  group_by(current_segment, group) %>%
  summarise(number_of_actions = n()) %>%
  ungroup %>%
  filter(!(current_segment %in% c("One Session Only", "Dormant"))) %>%
  dcast(
    group ~ current_segment
    , value.var = "number_of_actions"
  ) %>%
  mutate(Active = Core + Casual + Marginal + New + Reactivated) %>%
  melt(
    id.vars = "group"
    , variable.name = "current_segment"
    , value.name = "number_of_actions"
  ) %>%
  {
    .[is.na(.)] <- 0
    return(.)
  } %>%
  filter(current_segment %in% c("Core", "Casual", "Active")) 

# Version 1: straight up take the top 3
rank_actions_by_count <- platform_action_counts %>%
  group_by(current_segment) %>%
  mutate(
    total_actions = sum(number_of_actions)
    , pct_of_actions = number_of_actions/total_actions
  ) %>%
  arrange(desc(pct_of_actions)) 

top3_actions_by_count_v1 <- rank_actions_by_count %>% 
  slice(1:3) 

# Version 2: Find the top 3 for active users, then remove those. THEN find 
# the top 3 for Core and Casual users.

top3_actions_for_active_users <- rank_actions_by_count %>%
  ungroup %>%
  filter(current_segment == "Active") %>%
  arrange(desc(pct_of_actions)) %>%
  slice(1:3) %>%
  {.$group}

top3_actions_by_count_v2 <- rank_actions_by_count %>%
  filter(
    !((current_segment != "Active") & (group %in% top3_actions_for_active_users))
  ) %>%
  slice(1:3)
  
if(remove_active_actions_from_top3){
  top3_actions_by_count <- top3_actions_by_count_v2
} else{top3_actions_by_count <- top3_actions_by_count_v1}

for(seg in unique(top3_actions_by_count$current_segment)){
  total_number <- top3_actions_by_count %>%
    ungroup %>%
    filter(current_segment == seg) %>%
    {.$total_actions[1]}
  
  plotname <- 
    paste(
      "top_3_actions_of"
      , seg
      , "users"
      , ifelse(use_true_champid, "extra_users", "")
      , sep = "_"
    )
  
  top3_actions_by_count %>%
    ungroup %>%
    filter(current_segment == seg) %>%
    plot_ly(
      x = group
      , y = pct_of_actions
      , text = 
          paste(
            round(100*pct_of_actions, 1)
            , "% ("
            , prettyNum(number_of_actions, big.mark = ',')
            , " actions)"
            , sep = "")
      , type = "bar"
      , hoverinfo = "text"
    ) %>%
    bar_chart_layout(
      charttitle = 
        paste(
          "Top 3 Actions of "
          , seg
          , " users in 2016 ("
          , prettyNum(total_number, big.mark = ",")
          , " actions total)"
          , sep = ""
        )
      , bottommargin = 250
      , yaxisformat = "%"
      , yaxistitle = "Percent of Actions Taken"
    ) %>%
    save_or_print(
      save_plots = T
      , outloc = writeloc
      , plot_name = plotname
      , outformat = output_graph_format
    )
}

