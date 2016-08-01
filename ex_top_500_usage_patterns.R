# Exploration: Top 500 usage patterns

# Runs after ex_top_500_active_users.r (need to restart R session first)

# Packages and functions ####
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(plyr)
library(dplyr)
library(chron)
library(plotly)

source("fn_create_mode_pct_data.r")
source("fn_create_mode_pct_data_by_date.r")
source("fn_create_convex_data.r")
source("fn_create_convex_data_v2.r")
source("fn_plot_triangle_diagram_v2.r")
source("fn_prepare_modepctdata_for_createconvexdata.r")

layout <- plotly::layout

# User subsets ####

standard_user_subset <- user_facts %>% 
  #  merge(select(champion_facts, champion_id, dont.exclude)) %>% 
  filter(
    account_type == "End User"
  ) %>% 
  {.$user_id} 

# Prepare data for triangle plot ####

# Create "time since account creation" field ####
starttime <- Sys.time()
user_platformaction_date_time_mode_timesincecreation <- user_platformaction_datetime %>%
  merge(
    select(platformaction_facts, platform_action, mode)
  ) %>% #filter(user_id %in% sample(user_facts$user_id, 1000, replace = F)) %>%
  group_by(user_id) %>%
  mutate(
    created_date = rep(date[platform_action == "Account Created"][1], times = n())
    , time_since_creation = difftime(date, created_date, units = "days")
  ) %>% 
  ungroup 

endtime <- Sys.time()
(endtime - starttime)

# First round of aggregations

mode_pct_data <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data

convex_data <- mode_pct_data %>%
  create_convex_data

user_table <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) 

mode_pct_data_by_date <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data_by_date

rm(user_table)

# Plot activity of each tier through time.

top_users_list %>%
  names %>%
  lapply(
    FUN = function(name){
      v <- unlist(top_users_list[name])
      
      for(i in c(1, 7, 14, 21, 28)){
        
        mode_pct_data_by_date %>%
          prepare_modepctdata_for_createconvexdata(user_subset = v, number_of_days = i, maxdate = Sys.Date()) %>%
          create_convex_data_v2 %>%
          plot_triangle_diagram_v2 %>%
          {
            title <- 
              paste("Day", i, sep = " ")
            return(. + ggtitle(paste(name, "-", title, sep = " ")))
          } %>%
          print
        
      } 
    }
  )

# Take the top cohorts from each tier. Plot those cohorts through time.






