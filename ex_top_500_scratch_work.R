# Exploration: top_500_scratchwork

# Seeking unusually high or low occurrences of certain demographics.####

gender_analysis <- user_facts %>%
  filter(user_id %in% standard_user_subset) %>% 
  mutate(in_top_500 = (user_id %in% top_500_users_by_number_of_actions_overall)) %>%
  group_by(gender, in_top_500) %>%
  summarise(number_of_users = n()) %>%
  {
    .[.==""] <- "undefined"
    return(.)
  } %>%
  ungroup %>%
  chisquare_analysis

age_analysis <- user_facts %>%
  filter(user_id %in% standard_user_subset) %>% 
  mutate(
    in_top_500 = (user_id %in% top_500_users_by_number_of_actions_overall)
    , gave_age = !is.na(age)
  ) %>%
  group_by(gave_age, in_top_500) %>%
  summarise(number_of_users = n()) %>%
  {
    .[.==""] <- "undefined"
    return(.)
  } %>%
  ungroup %>%
  chisquare_analysis

# Occurences of posting/sharing in first week

# When did they sign up?






# 

# Bringing in cohorts

# Clustering similar domain names

domain_clust <- user_facts %>% 
{.$email_domain} %>%
  unique %>% 
  {stringdistmatrix(.,., method = "lv")} %>% #as.dist %>%
  Mclust(G = 1:nrow(user_facts))




plot_ly(
  x = age
  , y = number_of_active_days_past_month
  , type = "scatter"
  , mode = "markers"
)