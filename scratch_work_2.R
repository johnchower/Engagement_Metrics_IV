# This stuff is for putting the true champion_id and "isfirst" stuff into the program
# and assessment starts



program_starts %>%
  mutate(
    isfirst = 
      (
        nrow(
          basic_user
      ) > 0)
  ) %>%
  select(-u) %>%
  select(-c) %>% View

test_user <- sample(basic_user$user_id, 1)

starttime <- Sys.time()  
program_starts %>%
#  filter(user_id == test_user) %>%
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
  } %>% {.$isfirst} %>% unique
endtime <- Sys.time()
(endtime -starttime)# * nrow(program_starts)/ nrow(filter(program_starts, user_id == test_user))

# Comparing two formulas for "overall" DAU to MAU ratio

plot_data_yearly_ratio %>%
  plot_ly(
    x = current_years_ratio_avg
    , y = current_years_ratio_totals
    , text = platform_action
    , type = "scatter"
    , mode = "markers"
  ) 

plot_data_current_ratio %>%
  plot_ly(
    x = current_ratio_avg
    , y = current_ratio_totals
    , text = platform_action
    , type = "scatter"
    , mode = "markers"
  ) 
