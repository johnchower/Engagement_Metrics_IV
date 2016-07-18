# weekly counts of core users:

# Method 1: use the date_user_table produced by my scripts
date_user_table %>%
  merge(select(user_facts, user_id, account_type)) %>%
  filter(
    date >= Sys.Date() - 7
    , date <= Sys.Date() - 1
    , account_type == "End User"
    ) %>%
  group_by(user_id, account_type) %>%
  summarise(number_of_active_days = sum(activitytoday)) %>%
  filter(number_of_active_days >= 4) %>%
  select(user_id, number_of_active_days) %>%
  merge(user_facts) %>% 
#  filter(!fake_end_user) %>% 
  nrow

# Method 2: Use the session_duration_fact table in Looker (already filtered on
# previous 7 days and end users.)
session_duration_fact_path <- "/Users/johnhower/Google Drive/Engagement_Metrics_IV/gloo session_duration_fact 2016-07-12T1639.csv"

session_duration_fact_path %>%
  read.table(
    header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>% 
  rename(
    user_id = User.Dimensions.ID
    , date = Date.Dimensions.Session.Duration.Date
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d")) %>%
  group_by(user_id) %>%
  summarise(number_of_active_days = n()) %>%
  filter(number_of_active_days >= 4) %>%
  View