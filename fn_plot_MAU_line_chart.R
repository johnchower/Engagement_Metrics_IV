# Function: MAU_line_chart

plot_MAU_line_chart <- 
  function(
    champid = NULL
    , dut = date_user_table
    , ut = user_table
    , cncid = champname_champid
  ){
    users_belonging_to_champ <- get_users_belonging_to_champ(champid, ut)
    champname <- champname_champid %>%
    {
        if(is.null(champid)){
          return(NULL)
        } else {return(filter(., champion_id == champid)$champion_name)}
      }
    
    base <- dut %>%
      select(user_id, date, MAU, WAU, DAU) %>%
      filter(user_id %in% users_belonging_to_champ)%>%
      group_by(date) %>%
      summarise(Total_MAUs = sum(MAU), Total_WAUs = sum(WAU), Total_DAUs = sum(DAU))
    
    all.together <- base %>%
      melt(
        id.vars = "date"
        , variable.name = "User_Class"
        , value.name = "Number_of_Users"
        ) %>%
      plot_ly(type = "scatter", x = date, y = Number_of_Users, group = User_Class) %>%
      layout(
        title = paste(champname, "Total Number of Active Users",sep=" ")
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
            , tickformat = "n"
          )
        , margin = 
            list(
              r = 100
              , t = 100
            )
      )
    
    DAU.only <- base %>%
      select(date, Total_DAUs) %>%
      plot_ly(type = "scatter", x = date, y = Total_DAUs)%>%
      layout(
        title = paste(champname, "Total Number of Daily Active Users",sep=" ")
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
            , tickformat = "n"
          )
        , margin = 
          list(
            r = 100
            , t = 100
          )
      )
    
    WAU.only <- base %>%
      select(date, Total_WAUs) %>%
      plot_ly(type = "scatter", x = date, y = Total_WAUs)%>%
      layout(
        title = paste(champname, "Total Number of Weekly Active Users",sep=" ")
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
            , tickformat = "n"
          )
        , margin = 
          list(
            r = 100
            , t = 100
          )
      )
    
    MAU.only <- base %>%
      select(date, Total_MAUs) %>%
      plot_ly(type = "scatter"
              , x = date
              , y = Total_MAUs
        )%>%
      layout(
        title = paste(champname, "Total Number of Monthly Active Users",sep=" ")
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
            , tickformat = "n"
          )
        , margin = 
          list(
            r = 100
            , t = 100
          )
      )
    
    return(list(all.together, DAU.only, WAU.only, MAU.only))
  }
