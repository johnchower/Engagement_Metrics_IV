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

