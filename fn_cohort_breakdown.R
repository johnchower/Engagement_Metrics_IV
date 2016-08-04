# Function: cohort_breakdown

cohort_breakdown <- function(
  user.subset
  , userid.cohortid = userid_cohortid
  , cohortid.champid = cohortid_champid
  , champion.facts = champion_facts
#  , mutate.commands = list() # Eventually, we'll build in the ability to combine cohorts in any way you want here
){
  user.subset_breakdown <- user.subset %>%
    {filter(userid.cohortid, user_id %in% .)} %>%
    merge(cohortid.champid, by.x = "cohort_id", by.y = "cohort_id", all.x = T) %>%
    rename(cohort_champion = champion_id) %>%
    merge(select(user_facts, user_id, champion_id), by = "user_id", all.x = T) %>%
    rename(user_champion = champion_id) %>%
    merge(
      select(champion.facts, champion_id, champion_name)
      , by.x = "cohort_champion"
      , by.y = "champion_id"
      , all.x = T
    ) %>%
    rename(cohort_champion_name = champion_name) %>%
    merge(
      select(champion.facts, champion_id, champion_name)
      , by.x = "user_champion"
      , by.y = "champion_id"
      , all.x = T
    ) %>%
    rename(user_champion_name = champion_name) %>%
    mutate(
      has.cohort = ifelse(
        is.na(cohort_id)
        , "No cohort"
        , "Cohort"
      )
      , description = ifelse(
        is.na(cohort_id)
        , paste(has.cohort, user_champion_name, sep = " - ")
        , paste(has.cohort, cohort_champion_name, sep = " - ")
      )
    ) %>%
#    mutate(
#      description = ifelse(
#        grepl("Cru", description)
#        , "Cohort - Cru"
#        , description
#      )
#    ) %>%
    mutate(
      description = ifelse(
        grepl("Christian Character Formation Project", description)
        , "Cohort - Christian CFP"
        , description
      )
    ) %>%
    mutate(
      description = ifelse(
        grepl("Civic Character Formation Project", description)
        , "Cohort - Civic CFP"
        , description
      )
    ) %>%
    mutate(
      description = ifelse(
        grepl("Gloo", description) & !grepl("No cohort", description)
        , "Cohort - Gloo"
        , description
      )
    ) %>%
    select(-X.x, -X.y) %>%
    rename(user_champion_id = user_champion, cohort_champion_id = cohort_champion)
  
  return(user.subset_breakdown)
}