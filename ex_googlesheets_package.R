# Exploration: googlesheets package

library(googlesheets)
library(dplyr)
gs_auth()
gs_user()
View(gs_ls())


x <- gs_title("today_vs_yesterday_core") %>% 
  gs_read

setdiff(x$today_core, x$yesterday_core) %>% 
  as.character %>%
  paste(collapse = ", ", sep = "") %>%
  noquote


users_lost <- gs_title("users_lost_by_moving_window") %>%
  gs_read %>%
  {
    colnames(.)[1] <- "user_id"
    return(.)
  }

users_lost %>%
  group_by(user_id) %>%
  summarise(number_of_session_duration_dates = n()) %>%
  arrange(desc(number_of_session_duration_dates)) %>% 
  {.$number_of_session_duration_dates} %>%
  {. > 3} %>%
  sum
  View