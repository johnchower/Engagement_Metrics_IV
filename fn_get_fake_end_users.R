# function: get_fake_end_users

get_fake_end_users <-
  function(
    upadt = user_platformaction_date_time
    , path_to_allowed_actions = "platformaction_enduserallowed.csv"
  ){
    platformaction_allowed <-
      path_to_allowed_actions %>%
      read.table(
        header = T
        , sep = ','
        , stringsAsFactors = F
      ) %>%
      rename(Truth1 = Truth_1) %>%
      dplyr::filter(Truth1 < 0.5) %>%
      {.$User.Platform.Action.Facts.Platform.Action}
    
    fake_end_users <- upadt %>%
      dplyr::filter(!(platform_action %in% platformaction_allowed)) %>%
      {unique(.$user_id)}
    
    return(fake_end_users)
  }

  