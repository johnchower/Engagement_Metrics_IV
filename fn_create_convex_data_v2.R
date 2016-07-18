# Function: create_convex_data_v2

create_convex_data_v2 <- 
  function(mpd = mode_pct_data){
    mpd %>%
      mutate(
        xval = Invest.for.Self.Us_pct + 0.5*Receive.Value_pct
        , yval = (sqrt(3)/2)*Receive.Value_pct
      ) %>%
      mutate(mode = factor(mode, levels = c("Receive Value", "Invest for Self/Us", "Champion Others"))) %>%
      return
  }