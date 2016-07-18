# Function: plot_MAU_bar_chart

plot_MAU_bar_chart <- 
  function(
    data_relative
    , data_absolute
    , title_append = ""
    , include_dormant = F
  ){
    absolute_chart <- data_absolute %>%
      plot_ly(type = "bar", x = date, y = Core, name = "Core") %>%
      add_trace(
          x = date
          , y = data_absolute$Casual
          , type = "bar"
          , name = "Casual"
      ) %>%
      add_trace(
        x = date
        , y = data_absolute$Marginal
        , type = "bar"
        , name = "Marginal"
      ) %>%
      add_trace(
        x = date
        , y = data_absolute$Reactivated
        , type = "bar"
        , name = "Reactivated"
      ) %>%
      add_trace(
        x = date
        , y = data_absolute$New
        , type = "bar"
        , name = "New"
      ) %>%
      {
        if(include_dormant){
          
          out <- 
            add_trace(.
              , x = date
              , y = data_absolute$Dormant
              , type = "bar"
              , name = "Dormant"
            )
          out <- 
            add_trace(out
              , x = date
              , y = data_absolute$One_Session_Only
              , type = "bar"
              , name = "One Session Only"
            )
          return(out)
        } else {return(.)}
      } %>%
      bar_chart_layout(
        yaxisformat = "n"
        , charttitle = paste("Number of Monthly Active Users in Each State", title_append, sep = " ")
        , bar_mode = "stack"
      )
    
    relative_chart <- data_relative %>%
      plot_ly(type = "bar", x = date, y = Core, name = "Core") %>%
      add_trace(
        x = date
        , y = data_relative$Casual
        , type = "bar"
        , name = "Casual"
      ) %>%
      add_trace(
        x = date
        , y = data_relative$Marginal
        , type = "bar"
        , name = "Marginal"
      ) %>%
      add_trace(
        x = date
        , y = data_relative$Reactivated
        , type = "bar"
        , name = "Reactivated"
      ) %>%
      add_trace(
        x = date
        , y = data_relative$New
        , type = "bar"
        , name = "New"
      ) %>%
      {
        if(include_dormant){
          
          out <- 
            add_trace(.
                      , x = date
                      , y = data_relative$Dormant
                      , type = "bar"
                      , name = "Dormant"
            )
          out <- 
            add_trace(out
                      , x = date
                      , y = data_relative$One_Session_Only
                      , type = "bar"
                      , name = "One Session Only"
            )
          return(out)
        } else {return(.)}
      } %>%
      bar_chart_layout(
        yaxisformat = "%"
        , charttitle = paste("Percent of Monthly Active Users in Each State", title_append, sep = " ")
        , bar_mode = "stack"
      )
    
    return(list(absolute_chart, relative_chart))
  }