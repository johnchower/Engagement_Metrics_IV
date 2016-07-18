# Function: current_DAU_to_MAU_by_champ

current_DAU_to_MAU_by_champ <- 
  function(
    dut = date_user_table
    , ut = user_table
    , cncid = champname_champid
  ){
      
    user_champname <- ut %>%
      merge(cncid) %>%
      select(user_id, champion_name)
    
    dut %>%
      merge(user_champname) %>%
      filter(date >= Sys.Date() - 7) %>%
      group_by(date, champion_name) %>%
      summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>% 
      ungroup %>%
      group_by(champion_name) %>%
      summarise(DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio)) %>%
      arrange(desc(DAU_to_MAU_ratio)) %>%
      plot_ly(x = champion_name, y = DAU_to_MAU_ratio, type = "bar") %>%
      layout(
        title = "DAU to MAU Ratio by Champion"
        , font = 
          list(
            size = 20
          )
        , xaxis = 
          list(
            title = ""
          )
        , yaxis = 
          list(
            title = ""
            , tickformat = "%"
          )
        , margin = 
          list(
            r = 100
            , t = 100
            , b = 250
            , pad = 20
            , l = 100
          )
      )
      
  }