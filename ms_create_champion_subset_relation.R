# Create champion subset relation

library(chron)
library(plyr)
library(dplyr)
library(utils)

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
  filter(dont.exclude) %>%
  {.$champion_id}

# Cru champs

cru_champs <- champion_facts %>%
  filter(champion_organization == "Cru") %>%
  {.$champion_id}


starttime <- Sys.time()


# Champion subset relation ####

outloc <- "/Users/johnhower/Google Drive/Engagement_Metrics_IV/input_csvs"

champion_facts %>% filter(dont.exclude) %>% arrange(champion_name) %>% {.$champion_name} %>% unique

champion_facts %>%
  filter(dont.exclude) %>%
  select(champion_id) %>%
  rename(champion_id_x = champion_id) %>%
  {merge(., rename(., champion_id_y = champion_id_x), all = T)} %>% 
  merge(
    select(champion_facts, champion_id, champion_name_x = champion_name)
    , by.x = "champion_id_x"
    , by.y = "champion_id"
  ) %>% 
  merge(
    select(champion_facts, champion_id, champion_name_y = champion_name)
    , by.x = "champion_id_y"
    , by.y = "champion_id"
  ) %>% 
  arrange(champion_name_y, champion_name_x) %>%
  mutate(y_contains_x = F) %>%
  filter(!(champion_id_y == champion_id_x)) %>%
  mutate(
    y_contains_x = 
      ifelse(
        (champion_name_y == "Prism Insights") & (grepl("REVEAL", champion_name_x))
        , T
        , y_contains_x
      )
  ) %>%
  mutate(
    y_contains_x = 
      ifelse(
        (champion_name_y == "Cru") & (champion_id_x %in% cru_champs)
        , T
        , y_contains_x
      )
  ) %>%
  mutate(
    y_contains_x = 
      ifelse(
        (champion_name_y == "Date Night") & grepl("Date Night", champion_name_x)
        , T
        , y_contains_x
      )
  ) %>%
  write.csv(
    file = paste(outloc, 'champion_subset_relation.csv', sep = "/")
  ) 

