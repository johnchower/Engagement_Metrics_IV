# Master Script: DAU/MAU by platform action

# Packages

library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(magrittr)
library(plotly)
library(chron)
library(utils)


source("fn_bar_chart_layout.r")
source("fn_save_or_print.r")
source("fn_moving_sum.r")

combine_Cru <- T
group_pas <- T

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

# Prepare data ####
# Note: had to keep the data in wide format and use plyr for much of the computation 
# in order to deal with memory issues on laptop. If this gets run on a high-powered 
# machine, it should be changed to the standard long format plyr approach.



alldates <- user_platformaction_datetime %>%
{seq.Date(min(.$date), max(.$date), by = 1)}

user_alldates <-   user_facts %>%
  filter(user_id %in% standard_user_subset) %>%
  select(user_id, date) %>%
  rename(createddate = date) %>% 
  merge(
    data.frame(date = alldates)
  )%>% filter(date >= createddate) %>% 
  select(-createddate) 

temp <- user_platformaction_datetime %>%
  {
    if(group_pas){
      x <- merge(., select(platformaction_facts, platform_action, group)) %>%
        select(-platform_action) %>%
        rename(platform_action = group)
    } else{x <- .}
    return(x)
  } %>%
  filter(
    user_id %in% standard_user_subset
    , !(platform_action %in% c("Started Session", "Account Created", ""))
  ) %>%
  group_by(user_id, date, platform_action) %>%
  summarise(activitytoday = 1) %>% 
  ungroup %>%
  dcast(
    user_id + date ~ platform_action, value.var = "activitytoday"
  ) %>% 
  merge(user_alldates, all = T) %>% 
  {
    .[is.na(.)] <- 0
    return(.)
  } 


colnames(temp) %<>%
  {gsub(" ", "_", .)} %>%
  {gsub("-", ".", .)} %>%
  {gsub("[(]first[)]", "first", .)} 

platform_actions <- as.list(colnames(temp)[3:length(temp)])

starttime <- Sys.time()
date_platformaction_DAUMAUratio <- platform_actions %>%
  ldply(
    .fun = 
      function(pa){
        temp %>%
          select_("user_id", "date", pa) %>%
          {
            colnames(.)[3] <- "activitytoday"
            return(.)
          } %>%
          group_by(user_id) %>%
          arrange(date) %>%
          mutate(
            past28 = moving_sum(activitytoday, 28)
            , 
            DAU = as.numeric(activitytoday > 0)
            , 
            MAU = as.numeric(past28 > 0)
          ) %>%
          ungroup %>%
          group_by(date) %>%
          summarise(
            number_DAUs = sum(DAU)
            , number_MAUs = sum(MAU)
            , DAU_to_MAU_ratio = ifelse(is.na(number_DAUs/number_MAUs), 0, number_DAUs/number_MAUs)
            , platform_action = pa
          ) %>%
          ungroup 
      }
  ) 
endtime <- Sys.time()

# Data for showing "current" ratio (average of past weeks')
plot_data_current_ratio <- date_platformaction_DAUMAUratio %>% 
  filter(date > (max(date) - 7)) %>%
  group_by(platform_action) %>%
  summarise(
    current_ratio_avg = mean(DAU_to_MAU_ratio)
    , current_ratio_totals = sum(number_DAUs)/sum(number_MAUs)
  ) 

plot_data_yearly_ratio <- date_platformaction_DAUMAUratio %>% 
  filter(date >= as.Date(format(max(date), "%Y-01-01"))) %>% 
  group_by(platform_action) %>%
  summarise(
    current_years_ratio_avg = mean(DAU_to_MAU_ratio)
    , current_years_ratio_totals = sum(number_DAUs)/sum(number_MAUs)
  ) 

# Make plots ####
plotname <- "current_dau_to_mau_by_platform_action_grouped"

plot_data_current_ratio %>%
  arrange(desc(current_ratio_totals)) %>%
  plot_ly(x = platform_action, y = current_ratio_totals, type = "bar") %>%
  bar_chart_layout(
    charttitle = "Current DAU/MAU Ratio by Platform Action"
    , yaxisformat = "%"
    , bottommargin = 400
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = saveloc
    , plot_name = plotname
  )

plotname <- 
  paste(
    "year"
    , format(Sys.Date(), "%Y")
    , "dau_to_mau_by_platform_action_grouped"
    , sep = "_"
  )

plot_data_yearly_ratio %>%
  arrange(desc(current_years_ratio_totals)) %>%
  plot_ly(x = platform_action, y = current_years_ratio_totals, type = "bar") %>%
  bar_chart_layout(
    charttitle = paste("Overall DAU/MAU Ratio by Platform Action (", format(max(date_platformaction_DAUMAUratio$date), "%Y"), ")", sep = "") 
    , yaxisformat = "%"
    , bottommargin = 400
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = saveloc
    , plot_name = plotname
  )

  
  
  

