# Exploration: Cornerstone metrics

source("fn_moving_sum.r")

date_user_table <- 
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_facts <- 
  read.table(
    "user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

# Subsets

end_users <- user_facts %>%
  filter(account_type == "End User") %>%
  {.$user_id} %>%
  unique

# Number of core users (defined monthly) who have showed up in past 7, 14 and 28 days

core_user_by_window <- date_user_table %>%
  filter(date >= max(date) - 28, user_id %in% end_users) %>% 
  group_by(user_id) %>%
  mutate(
    past14 = moving_sum(activitytoday, 14)
  ) %>%
  ungroup %>%
  filter(date == max(date)) %>%
  mutate(
    core_past_7 = (past7 > 0) & (past28 > 12)
    , core_past_14 = (past14 > 0) & (past28 > 12)
    , core_past_28 = past28 > 12
  ) %>%
  summarise(sum(core_past_7), sum(core_past_14), sum(core_past_28))
   
  
# Number of "core" users in past 7, 14 and 28 days

core_user_by_actual_actions <- date_user_table %>%
  filter(date >= max(date) - 28, user_id %in% end_users) %>% 
  group_by(user_id) %>%
  mutate(
    past14 = moving_sum(activitytoday, 14)
  ) %>%
  ungroup %>%
  filter(date == max(date)) %>%
  mutate(
    core_past_7 = past7 > 3
    , core_past_14 = past14 > 6
    , core_past_28 = past28 > 12
  ) %>%
  summarise(sum(core_past_7), sum(core_past_14), sum(core_past_28))

# User states

user_breakdown_by_current_state_data <- user_facts %>%
  filter(user_id %in% end_users) %>%
  group_by(current_segment) %>%
  summarise(number_of_users = n()) %>%
  mutate(
    total_number_of_users = sum(number_of_users)
    , percent_of_users = number_of_users/total_number_of_users
  ) %>% 
  arrange(desc(percent_of_users)) %>%
  mutate(percent_of_users = round(percent_of_users, 3))

# Produce user breakdown plot
user_breakdown_by_current_state_data %>%
  plot_ly(
    type = "bar"
    , x = current_segment
    , y = percent_of_users
    , text = paste(100*percent_of_users, "% (", as.character(number_of_users), " users)", sep = "")
    , mode = "text"
    , hoverinfo = "text"
  ) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users in Each State ("
        , user_breakdown_by_current_state_data$total_number_of_users[1]
        , " Total Users)"
        , sep = ""
      ) 
    , yaxisformat = "%"
    , bottommargin = 150
  )