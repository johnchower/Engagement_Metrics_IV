# Master Script: Gloo autoconnects
# Run directly after ms_segment_and_classify_4_0.r

library(chron)
library(plyr)
library(dplyr)
library(utils)

source("fn_find_true_first_champ.r")

starttime <- Sys.time()

#
# Read in the segmented/classified data frames ####

user_facts <- 
  read.table(
    "user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

champion_facts <- 
  read.table(
    "champion_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) 

platformaction_facts <- 
  read.table(
    "platformaction_facts.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  )

date_user_table <- 
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_platformaction_datetime <- 
  read.table(
    "user_platformaction_datetime.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(datetime = chron(dates. = date, times. = time, format = c(dates = "y-m-d", times = "h:m:s"))) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

champion_subset_relation <- 
  read.table(
    "input_csvs/champion_subset_relation.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) 



# User/champion Subsets ####

# 1. Standard subset
# End users who have never taken a "champion/internal only" platform action, and who
# don't belong to the list of flagged champions.

standard_user_subset <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    dont.exclude
    , 
    !fake_end_user
    , 
    account_type == "End User"
  ) %>% 
  {.$user_id} 

standard_user_subset_allchamps <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    !fake_end_user
    , 
    account_type == "End User"
  ) %>% 
  {.$user_id} 

# 2. REVEAL for Church

REVEAL_Church_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "REVEAL For Church") %>%
  {.$user_id}

# 3. FamilyLife

FamilyLife_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "FamilyLife") %>%
  {.$user_id}

# 4. Gloo

Gloo_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "Gloo") %>%
  {.$user_id}

# Internal champs

internal_champs <- champion_facts %>%
  filter(!dont.exclude) %>%
  {.$champion_id}

# Cru champs

cru_champs <- champion_facts %>%
  filter(champion_organization == "Cru") %>%
  {.$champion_id}

# Reclassify users ####

potential_autoconnect_users <- intersect(Gloo_users, standard_user_subset_allchamps)

user_truefirstchamp <- user_platformaction_datetime %>% 
  filter(
    platform_action %in% c("Connected to Champion", "Connected to Champion (first)")
    , 
    user_id %in% potential_autoconnect_users
  ) %>% 
  select(user_id, champion_id, isfirst, datetime) %>% 
  group_by(user_id) %>% 
  filter(datetime == min(datetime)) %>%
  ungroup %>%
  group_by(user_id) %>%
  summarise(true_first_champion = find_true_first_champ(champion_id))

# Merge changes into user_facts

user_facts_extra_columns_truechamp <- user_facts %>%
  merge(user_truefirstchamp, all.x = T) %>% 
  mutate(
    true_champion_id =
      ifelse(
        is.na(true_first_champion)
        , champion_id
        , ifelse(
          true_first_champion %in% c(-1,0)
          , 1
          , true_first_champion
        )
      )
  )

user_facts_0 <- user_facts_extra_columns_truechamp %>%
  select(-champion_id, -true_first_champion) %>% 
  rename(champion_id = true_champion_id)

write.csv(
  x = user_facts_0
  , file = "user_table_true_first.csv"
)

endtime <- Sys.time()
endtime-starttime
rm(list = ls())


