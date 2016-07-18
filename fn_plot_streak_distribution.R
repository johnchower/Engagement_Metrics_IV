# Function: plot_streak_distribution

plot_streak_distribution <-
  function(
    champid = NULL
    , ut = user_table
    , dut = date_user_table
    , yaxis.log = F
    , return.data = F
  ){
    users_belonging_to_champ <- get_users_belonging_to_champ(champid, ut)
    
    aggregated_streaks <-
      dut %>%
      filter(streaks > 0, user_id %in% users_belonging_to_champ) %>%
      select(streaks) %>%
      group_by(streak_length = streaks) %>%
      summarise(count = n(), log_count = log(n()+1,base = 10))
    
    linear_histogram <- aggregated_streaks %>%
      select(streak_length, count) %>%
      plot_ly(type = "bar", x = streak_length, y = count) %>%
      layout(
        title = "Histogram of User Active Streaks"
        , font = 
          list(
            size = 20
          )
        , xaxis = 
          list(
            title = "Streak Length"
          )
        , yaxis = 
          list(
            title = "Number of Streaks"
          )
        , margin = 
          list(
            r = 200
            , t = 100
            , b = 100
            , l = 100
          )
      )
    
    log_histogram <- linear_histogram %>%
      layout(title = "Histogram of User Active Streaks (Log Scale)",yaxis = list(type = "log"))
    
    if(return.data){
      return(aggregated_streaks)
    } else if(yaxis.log) {return(log_histogram)} else {return(linear_histogram)}
  }