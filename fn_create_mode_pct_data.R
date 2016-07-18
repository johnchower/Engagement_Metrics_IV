# Function: create_mode_pct_data

create_mode_pct_data <- 
  function(
    upadtm = user_platformaction_date_time_mode 
    , ut = user_facts[user_facts$user_id %in% unique(upadtm$user_id),]
  ){
    upadtm %>%
#      merge(select(ut, user_id, champion_name, current_segment)) %>%
      mutate(max_date = max(date)) %>%
      group_by(
        user_id
#        , champion_name
#        , current_segment
      ) %>%
      summarise(
        number_of_actions = sum(mode %in% c("Receive.Value", "Invest.for.Self.Us", "Champion.Others"))
        , Receive.Value_pct = 
            ifelse(
              number_of_actions == 0
              , 1/3
              , sum(mode == "Receive.Value")/number_of_actions
            )
        , Invest.for.Self.Us_pct = 
            ifelse(
              number_of_actions == 0
              , 1/3
              , sum(mode == "Invest.for.Self.Us")/number_of_actions
            )
        , Champion.Others_pct = 
            ifelse(
              number_of_actions == 0
              , 1/3
              , sum(mode == "Champion.Others")/number_of_actions
            )
        , actions_per_day_past_month = sum(date >= (max_date - 28))/28
        , largest_pct = max(Receive.Value_pct, Invest.for.Self.Us_pct, Champion.Others_pct)
        , mode = 
          ifelse(
            largest_pct == Receive.Value_pct
            , "Receive Value"
            , ifelse(
              largest_pct == Champion.Others_pct
              , "Champion Others"
              , "Invest for Self/Us"
            )
          )
      ) %>% 
      mutate(mode = factor(mode, levels = c("Receive Value", "Invest for Self/Us", "Champion Others"))) %>%
      ungroup 
  }