# Scratch work

# Champion subset relation ####

champion_subset_relation %>%
  filter(grepl("Date Night", champion_name_x, ignore.case = T), grepl("Date Night", champion_name_y, ignore.case = T)) %>%
  View


# Why do so many get left out of standard_user_subset? ####

user_facts %>%
  filter(!(user_id %in% standard_user_subset), account_type == "End User") %>% 
  merge(
    select(champion_facts, champion_id, champion_name, dont.exclude)
#    , by = "champion_id"
  ) %>%
  filter(dont.exclude) %>% nrow
  View

# Getting comma-separated list of user ids
 
useridvec %>% 
  as.character %>%
  paste(collapse = ", ", sep = "") %>%
  noquote

# Finding TYRO users who ended up in champion others mode

TYRO_champs <- c(60890, 56356, 58614, 55721, 58605, 56353, 58598, 56358, 55725, 58600, 56346)

user_facts %>%
  filter(user_id %in% TYRO_champs) %>%
  View

plot_data <- current_convex_data %>%
  filter(user_id %in% TYRO_champs) 

number_of_users <- nrow(plot_data)

triangle_diagram_current_mode_TYRO_end_champs <- plot_data %>%
  rename(actions_per_day = actions_per_day_past_month) %>%
  plot_triangle_diagram_v2 %>%
  {
    title <- 
      paste(
        "Current Learning Triangle Distribution - "
        , "TYRO end champs"
        , " ("
        , prettyNum(number_of_users, big.mark = ",")
        , " Users)"
        , sep = ""
      )
    return(. + ggtitle(title))
  }

triangle_diagram_current_mode_TYRO_end_champs

# Saving triangle diagrams to disk

plotlist_current_mode_all_users$All %>%
  as.widget

# Finding the automatic gloo connections
user_x <- user_facts %>%
  filter(current_segment == "Dormant", user_id %in% standard_user_subset) %>%
  {.$user_id} %>% 
  sample(1, replace = F) 

champion_connection_actions <- user_platformaction_date_time %>%
  filter(
    grepl("Followed Champion", platform_action)
    | grepl("Connected to Champion", platform_action)
    | grepl("Champion Membership Invitation Accepted", platform_action)
#    , user_id == user_x
  ) %>%
  arrange(user_id, datetime, platform_action, desc(champion_id))

potential_gloo_autoconnect <- champion_connection_actions %>%
  filter(champion_id == 1, !isfirst) %>%
  mutate(
    platform_action = paste(platformaction, "(first)")
    , matched = 
        match(
          c(user_id, datetime, platform_action)
          , select(champion_connection_actions, user_id, datetime, platform_action)
        )
  ) %>% 
  

