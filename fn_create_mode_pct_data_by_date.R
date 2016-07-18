# Function: create_mode_pct_data_by_date

create_mode_pct_data_by_date <- 
  function(
    upadtm = user_platformaction_date_time_mode 
    , ut = user_table[user_table$user_id %in% unique(upadtm$user_id),]
  ){
    upadtm %>%
      merge(select(ut, user_id, champion_name, current_segment)) %>%
      mutate(days_since_creation = floor(time_since_creation)) %>%
      group_by(user_id, champion_name, current_segment, date, days_since_creation) %>%
      summarise(
        Receive.Value.count = sum(mode == "Receive.Value")
        , Champion.Others.count = sum(mode == "Champion.Others")
        , Invest.for.Self.Us.count = sum(mode == "Invest.for.Self.Us")
      )  
  }