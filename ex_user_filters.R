# Exploration : Big filter

users_filtered_out_by_bogus_champ <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    !dont.exclude
    , 
    account_type == "End User"
  )

user_facts %>%
  filter(user_id %in% users_filtered_out_by_bogus_champ) %>%
  