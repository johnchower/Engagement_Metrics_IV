# Function: create_convex_data

create_convex_data <- 
  function(mpd = mode_pct_data){
    mpd %>%
      arrange(desc(actions_per_day_past_month)) %>%
      mutate(
        xval = Invest.for.Self.Us_pct + 0.5*Receive.Value_pct
        , yval = (sqrt(3)/2)*Receive.Value_pct
      ) %>%
      mutate(mode = factor(mode, levels = c("Receive Value", "Invest for Self/Us", "Champion Others"))) %>%
      return
  }