# Exploration: Reactivated users

# Based off of ms_KEM_modes and ms_platformaction_distributions

# Need:
#   user_platformaction_datetime
#   day they reactivated (date_user_table)
#   Connected to champion table? (To see who they reactivated under)

source("fn_chisquare_analysis.r")
source("fn_bar_chart_layout.r")

exclude_REVEAL <- T

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
  mutate(date = as.Date(date,format="%Y-%m-%d"))

#

# User Subsets ####

# 1. Standard subset
# End users who have never taken a "champion/internal only" platform action, and who
# don't belong to the list of flagged champions.

standard_user_subset <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(dont.exclude, !fake_end_user, account_type == "End User") %>%
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



starttime <- Sys.time()

#

#

# Data setup ####

user_reactivationdate_platformaction <- date_user_table %>%
  arrange(user_id, date) %>% 
  filter(reactivated == 1) %>% 
  select(user_id, reactivation_date = date) %>%
  merge(
    user_platformaction_datetime
    , by.x = c("user_id", "reactivation_date")
    , by.y = c("user_id", "date")
  ) %>% 
  select(user_id, reactivation_date, platform_action)

platformaction_reactivated <- user_platformaction_datetime %>%
  merge(select(platformaction_facts, platform_action, mode)) %>%
  merge(
    select(date_user_table
           ,user_id, date, reactivated
    )
    , by.x = c("user_id", "date")
    , by.y = c("user_id", "date")
  ) %>%
  {
    if(exclude_REVEAL){
      
      out <- filter(., user_id %in% setdiff(standard_user_subset, REVEAL_Church_users))
    } else{
        out <- filter(., user_id %in% standard_user_subset)
    }
    return(out)
  }

platformaction_reactivated %<>% select(platform_action, reactivated)

platformaction_df <- platformaction_reactivated %>%
  merge(select(platformaction_facts, platform_action, group)) %>%
  select(reactivated, group) %>%
  group_by(reactivated, group) %>%
  summarise(count = n()) %>%
  dcast(reactivated ~ group, value.var = "count") %>%
  {
    .[is.na(.)] <- 0
    return(.)
  } %>% 
  melt(id.vars = "reactivated", variable.name = "platformaction_group", value.name = "number_of_actions") %>%
  filter(
    !(platformaction_group %in% c("Account Created", "Started Session"))
  )

# Run chisquare analysis ####


chisquare_results_platformaction_vs_segment <- platformaction_df %>%
  chisquare_analysis(err_pvalue = .99)

hallmark_actions <- chisquare_results_platformaction_vs_segment$results %>% 
  group_by(platformaction_group) %>%
  filter(min(pvalue) < .01) %>%
  ungroup %>%
  {.$platformaction_group} %>%
  unique

# Plots ####

for(i in 0:1){
  df_i <- chisquare_results_platformaction_vs_segment$results %>%
    filter(
      reactivated == i
    ) %>%
    rename(observed = number_of_actions) %>%
    arrange(desc(marginal2)) %>%
    select(platformaction_group, observed, expected, err) %>%
    mutate(
      #      expected = round(expected)
      #      , err = round(err)
      total_actions = sum(observed)
      , percent_observed = observed/sum(observed)
      , percent_expected = expected/sum(expected)
      , err = err/total_actions
    ) %>%
    filter(platformaction_group %in% hallmark_actions)
  
  total_actions <- df_i
  
  plot_ly(
    df_i
    , x = platformaction_group
    , y = percent_expected
    , type = "bar"
    , name = "Expected Percent of Platform Actions"
    , error_y = list(array = err, width = 5)
  ) %>%
    add_trace(
      x = platformaction_group
      , y = df_i$percent_observed
      , type = "bar"
      , name = "Observed Percent of Platform Actions"
    ) %>%
    bar_chart_layout(
      charttitle = 
        paste(
          "Platform Action Distribution for '"
          , ifelse(i==0, "Normal", "Reactivation")
          , "' Days"
          , ifelse(exclude_REVEAL, " (REVEAL for Church excluded)", " (REVEAL for Church included)")
          , sep = ""
        )
      , yaxisformat = "%"
      , bottommargin = 300
      , leftmargin = 150
      , yaxisrange = c(0, .45)
    ) %>%
    print
}







  
  



