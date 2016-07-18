# Clean out fake end users and internal champions

fake_end_users <- 
  get_fake_end_users(upadt = user_platformaction_date_time 
                     , path_to_allowed_actions)

user_platformaction_date_time %<>% dplyr::filter(!(user_id %in% fake_end_users))
user_createddate_champid %<>% dplyr::filter(!(user_id %in% fake_end_users))

champname_champid %<>%
  mutate(
    dont.exclude = !(
      grepl("Gloo", champion_name) 
      | grepl("Emily Shirk", champion_name)
      | grepl("Fray", champion_name)
      | grepl("Justin Vallelonga", champion_name)
      | grepl("yolo", champion_name)
      | grepl("Jimmy Mellado", champion_name)
    )
  )

user_createddate_champid %<>%
  merge(champname_champid) %>%
  dplyr::filter(dont.exclude) %>%
  select(user_id, date, champion_id)

champname_champid %<>% 
  dplyr::filter(dont.exclude) %>%
  select(-dont.exclude)