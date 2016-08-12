# Master Script: cornerstone metrics

library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)

# Parameters

account_types_to_accept <- c("End User", "Champion User")

weeks_to_calc <- c(1, 3, 6)

week_string <- paste(weeks_to_calc, collapse = "")

rundate <- as.Date("2016-07-29")

# Load label-match data ####
label_includes <- read.csv("label_includes.csv", stringsAsFactors = F) %>%
  select(Label, Includes) %>%
  filter(Label != "")

# Load all other data ####
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

date_user_table <- 
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

# Label champions according to ScottB_Metrics ####

label_includes_list <- label_includes %>%
  dlply(
    .variables = .(Label)
    , .fun = function(df){
      df %>%
        {.$Includes[1]} %>%
        strsplit(", ") %>%
        return
    }
  )

label_includes_df <- label_includes_list %>%
  {do.call(rbind.data.frame, .)} %>% 
  {
    colnames(.) <- "Includes"
    return(.)
  } %>% 
  {mutate(., Label = rownames(.))} %>% 
  mutate(Label = gsub('\\.([0-9])+', "", Label)) %>%
  mutate(Label = as.character(Label), Includes = as.character(Includes))

champion_facts_labels <- champion_facts %>%
  #select(champion_id, champion_name) %>%
  merge(label_includes_df, by.x = "champion_name", by.y = "Includes", all.x = T) 
  

# Dates of interest ####

#rundate <- as.Date("2016-08-05")

weeklist <- list(
  week1 = (rundate - weeks_to_calc[1]*7) + c(0, 6)
  , week3 = (rundate - weeks_to_calc[2]*7) + c(0, 6)
  , week6 = (rundate - weeks_to_calc[3]*7) + c(0, 6)
)


# User Subsets ####
champion_subset <- champion_facts_labels %>%
  filter(!is.na(Label)) %>%
  dlply(
    .variables = .(Label)
    , .fun = function(df){
      champion_ids <- df$champion_id
      user_facts %>%
        filter(
          account_type %in% account_types_to_accept | is.na(account_type)
          , champion_id %in% champion_ids
        ) %>%
        {.$user_id} %>%
        unique %>%
        return
    }
  )

all <- c()
for(i in 1:length(champion_subset)){
  new.all <- champion_subset[i] %>%
    unlist
  
  all <- c(all, new.all)
}

champion_subset <- c(champion_subset, list(all_users = all))

weekly_existing_user_subset <- weeklist %>%
  lapply(
    FUN = function(v){
      
      startdate <- min(v)
      enddate <- max(v)
      
      date_user_table %>%
        merge(select(user_facts, user_id, account_type), by.x = "user_id", by.y = "user_id", all.x = T) %>% 
        filter(
          date == enddate
          , account_type %in% account_types_to_accept | is.na(account_type)
        ) %>%
        {.$user_id} %>%
        unique %>%
        return
    }
  )

weekly_WAU_subset <- weeklist %>%
  lapply(
    FUN = function(v){
      
      startdate <- min(v)
      enddate <- max(v)
      
      date_user_table %>%
        merge(select(user_facts, user_id, account_type), by.x = "user_id", by.y = "user_id", all.x = T) %>%
        filter(
          date == enddate
          , WAU == 1
          , account_type %in% account_types_to_accept | is.na(account_type)
        ) %>%
        {.$user_id} %>%
        unique %>%
        return
    }
  )

weekly_core_subset <- weeklist %>%
  lapply(
    FUN = function(v){
      
      startdate <- min(v)
      enddate <- max(v)
      
      date_user_table %>%
        merge(select(user_facts, user_id, account_type), by.x = "user_id", by.y = "user_id", all.x = T) %>%
        filter(
          date <= enddate
          , date >= enddate
          , segment == "Core"
          , activitytoday == 1
          , account_type %in% account_types_to_accept | is.na(account_type)
        ) %>%
        {.$user_id} %>%
        unique %>%
        return
    }
  )

# Loop through and calculate counts ####

core_count_df <- weekly_core_subset %>%
  ldply(
    .fun = function(v){
      champion_subset %>%
        ldply(
          .fun = function(w){
            w %>% 
              unlist %>%
              intersect(v) %>%
              length %>% 
              return
          }
        ) %>%
        {
          data.frame(count = .)
        }
    }
  ) %>% 
  rename(
    week = .id
    , champion = count..id
    , count = count.V1 
  ) %>% 
  mutate(variable = "core_users") 

WAU_count_df <- weekly_WAU_subset %>%
  ldply(
    .fun = function(v){
      champion_subset %>%
        ldply(
          .fun = function(w){
            w %>% 
              unlist %>%
              intersect(v) %>%
              length %>% 
              return
          }
        ) %>%
        {
          data.frame(count = .)
        }
    }
  ) %>% 
  rename(
    week = .id
    , champion = count..id
    , count = count.V1 
  ) %>% 
  mutate(variable = "weekly_active_users") 

existing_count_df <- weekly_existing_user_subset %>%
  ldply(
    .fun = function(v){
      champion_subset %>%
        ldply(
          .fun = function(w){
            w %>% 
              unlist %>%
              intersect(v) %>%
              length %>% 
              return
          }
        ) %>%
        {
          data.frame(count = .)
        }
    }
  ) %>% 
  rename(
    week = .id
    , champion = count..id
    , count = count.V1 
  ) %>% 
  mutate(variable = "existing_users") 

count_df <- 
  rbind(core_count_df, WAU_count_df, existing_count_df)

result_df <- count_df %>%
  dcast(week + champion ~ variable, value.var = "count") %>%
  mutate(pct_WAU = round(weekly_active_users/existing_users,3)) %>%
  select(
    -existing_users
  #  , -weekly_active_users
  )

# Format results to match Scott's Excel file ####

result_df %<>%
  melt %>%
  dcast(champion ~ variable + week, value.var = "value") %>%
  arrange(champion == "all_users", champion) %>%
  {
    colnames(.) <- gsub("week1", paste("week", weeks_to_calc[1], sep = ""), names(.))
    colnames(.) <- gsub("week3", paste("week", weeks_to_calc[2], sep = ""), names(.))
    colnames(.) <- gsub("week6", paste("week", weeks_to_calc[3], sep = ""), names(.))
    return(.)
  } 

# Export result csv ####

write.csv(
  result_df
  , paste(
      "~/Google Drive/Analytics_graphs/Cornerstone_Metrics/core_and_WAU_csvs/core_and_WAU"
      , rundate
      , "weeks"
      , paste(weeks_to_calc, collapse = "")
      , ".csv"
      , sep = "_"
    )
)

# Open the pdf files to check that they look good.


paste(
  "~/Google Drive/Analytics_graphs/Cornerstone_Metrics/core_and_WAU_csvs/core_and_WAU"
  , rundate
  , "weeks"
  , paste(weeks_to_calc, collapse = "")
  , ".csv"
  , sep = "_"
) %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("open ", ., sep = "")} %>%
  system
