# Function: prepare_modepctdata_for_createconvexdata

prepare_modepctdata_for_createconvexdata <-
  function(
    mpdbd = mode_pct_data_by_date
    , user_subset = NULL
    , number_of_days = 1
    , maxdate = max(user_platformaction_date_time_mode$date)
  ){
    x <- mpdbd %>%
      filter(
        days_since_creation >= 0
        , days_since_creation <= number_of_days
#        , date < maxdate - number_of_days
      ) 
    
    if(is.null(user_subset)){
      df <- x
    } else {df <- filter(x, user_id %in% user_subset)}
    
    df %>%
      group_by(user_id) %>%
      summarise(
        total_actions = sum(c(Receive.Value.count, Invest.for.Self.Us.count, Champion.Others.count))
        , Receive.Value_pct = sum(Receive.Value.count)/total_actions
        , Invest.for.Self.Us_pct = sum(Invest.for.Self.Us.count)/total_actions
        , Champion.Others_pct = sum(Champion.Others.count)/total_actions
        , actions_per_day = total_actions/number_of_days
      ) %>% 
      filter(actions_per_day > 0) %>%
      mutate(
        mode = 
          ifelse(
            (Invest.for.Self.Us_pct >= Champion.Others_pct) & (Invest.for.Self.Us_pct > Receive.Value_pct)
            , "Invest for Self/Us"
            , ifelse(
              (Champion.Others_pct > Invest.for.Self.Us_pct) & (Champion.Others_pct > Receive.Value_pct)
              , "Champion Others"
              , "Receive Value"
            )
          )
      ) %>%
      return
    
  }