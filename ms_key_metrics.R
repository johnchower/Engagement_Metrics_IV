# Exploration: Cornerstone metrics

source("fn_moving_sum.r")
source("fn_bar_chart_layout.r")
library(googlesheets)
library(dplyr)
library(plyr)
library(plotly)
library(RColorBrewer)

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

core_user_metrics_oldgs <- gs_title("core_user_metrics") %>% 
  gs_read

# Subsets

end_users <- user_facts %>%
  filter(account_type == "End User") %>%
  {.$user_id} %>%
  unique

# Number of core users (defined monthly) who have showed up in past 7, 14 and 28 days
current_date <- date_user_table %>%
  summarise(max_date = max(date)) %>%
  {.$max_date}

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
  summarise(
    date = current_date
    , core_users_past_week = sum(core_past_7)
    , core_users_past_two_weeks = sum(core_past_14)
    , core_users_past_month = sum(core_past_28)
  )
   
  
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
  summarise(
    date = current_date
    , core_users_past_week = sum(core_past_7)
    , core_users_past_two_weeks = sum(core_past_14)
    , core_users_past_month = sum(core_past_28)
  )

# Add new row to google sheet

# First, check to make sure that we're not adding a duplicate row

new_row_duplicated <- core_user_metrics_oldgs %>%
  match_df(core_user_by_actual_actions) %>% 
  nrow %>%
  {. > 0}
  
if(!new_row_duplicated){
  gs_add_row(
    gs_key("1j2aOKj7pxkvkwHgNzq72mFBBF9AgMz5nP4Tny6LzrfM")
    , input = core_user_by_window
  ) 
}

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
a <- list()
for (i in seq_len(nrow(user_breakdown_by_current_state_data))) {
  m <- user_breakdown_by_current_state_data[i, ]
  a[[i]] <- list(
    x = m$current_segment
    , y = m$percent_of_users
    , text = prettyNum(m$number_of_users, big.mark = ",")
    , xref = "x"
    , yref = "y"
    , showarrow = F
    , yanchor = "bottom"
  )
}


user_breakdown_by_current_state_data %>%
  {
    plot_ly(.,
      type = "bar"
      , marker = list(color = brewer.pal(nrow(.), "Dark2"))
      , x = current_segment
      , y = percent_of_users
      , text = paste(100*percent_of_users, "% (", as.character(number_of_users), " users)", sep = "")
      , mode = "markers"
      , hoverinfo = "text"
      , color = current_segment
    )
  } %>%
  layout(showlegend = F, annotations = a) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users in Each State ("
        , prettyNum(user_breakdown_by_current_state_data$total_number_of_users[1], big.mark = ",")
        , " Total Users)"
        , sep = ""
      ) 
    , yaxisformat = "%"
    , bottommargin = 150
  )