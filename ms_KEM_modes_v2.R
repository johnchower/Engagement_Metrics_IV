# Master Script: KEM_modes_v2

library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(plyr)
library(dplyr)
library(chron)
library(plotly)

source("fn_create_mode_pct_data.r")
source("fn_create_mode_pct_data_by_date.r")
source("fn_create_convex_data.r")
source("fn_create_convex_data_v2.r")
source("fn_plot_triangle_diagram_v2.r")
source("fn_prepare_modepctdata_for_createconvexdata.r")

layout <- plotly::layout

combine_Cru <- T

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
  filter(
#    dont.exclude
#    , 
#    !fake_end_user
#    , 
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

#

# Output Location and parameters ####
save_plots <- T
outloc <- "/Users/johnhower/Google Drive/Analytics_graphs/Engagement_Performance_Presentation_Slides/2016_07_17"
current_wd <- getwd()

# Prepare current mode data for plotting ####

current_convex_data <- user_platformaction_datetime %>%
  merge(select(platformaction_facts, platform_action, mode)) %>%
#  filter(date <= "2016-05-31", date >= "2016-01-01") %>%
  create_mode_pct_data %>%
  merge(select(user_facts, user_id, champion_id, current_segment)) %>%
  {
    if(combine_Cru){
      out <- 
        merge(.,
              select(champion_facts, champion_id, champion_organization)  
        )
      out <- rename(out, champion_name = champion_organization)
    } else{
      out <-
        merge(.,
              select(champion_facts, champion_id, champion_name)  
        )
    }
    return(select(out, -champion_id))
  } %>% 
  create_convex_data_v2 %>%
  filter(user_id %in% standard_user_subset)

#

# Plot current mode data ####

# All users
plot_data <- current_convex_data

number_of_users <- nrow(plot_data)

triangle_diagram_current_mode_all_users <- plot_data %>%
  rename(actions_per_day = actions_per_day_past_month) %>%
  plot_triangle_diagram_v2 %>%
  {
    title <- paste("Current Learning Triangle Distribution - All Users (", prettyNum(number_of_users, big.mark = ","), " total)", sep = "")
    
    return(. + ggtitle(title))
  }

# By champion

plotlist_current_mode_all_users <- champion_facts %>%
  filter(dont.exclude) %>%
  dlply(
    .variables = ifelse(combine_Cru, "champion_organization", "champion_name")
    , .fun = 
        function(df){
          champ <- df$champion_name[1]
          
          plot_data <- current_convex_data %>%
            filter(champion_name == champ)
          
          number_of_users <- nrow(plot_data)
          
          triangle_diagram_current_mode_all_users <- plot_data %>%
            rename(actions_per_day = actions_per_day_past_month) %>%
            plot_triangle_diagram_v2 %>%
            {
              title <- 
                paste(
                  "Current Learning Triangle Distribution - "
                  , champ
                  , " ("
                  , prettyNum(number_of_users, big.mark = ",")
                  , " Users)"
                  , sep = ""
                )
              return(. + ggtitle(title))
            }
          
          return(triangle_diagram_current_mode_all_users)
        }
  )

plotlist_current_mode_all_users %<>% 
  {
    c(
      .
      , list(All = triangle_diagram_current_mode_all_users)
    )
  }

rm(triangle_diagram_current_mode_all_users)


#

# Save plots out ####

plot_name <- "28_triangle_distribution_TYRO"

plotlist_current_mode_all_users$TYRO %>%
  { 
    if(save_plots){
      setwd(outloc)
      ggsave(paste(plot_name, ".png", sep = ""), plot = ., width = 12, height = 9)  
      setwd(current_wd)
    } else{print(.)}   
  }








# Create "time since account creation" field ####

user_platformaction_date_time_mode_timesincecreation <- user_platformaction_date_time_mode %>%
  group_by(user_id) %>%
  mutate(time_since_creation = difftime(datetime, datetime[platform_action == "Account Created"], units = "days")) %>% 
  ungroup 

# First round of aggregations

mode_pct_data <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data

convex_data <- mode_pct_data %>%
  create_convex_data

mode_pct_data_by_date <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data_by_date

# Plots: Triangle distributions for users in their first day, week and month.

subsetcontents <- NULL

subsetname <- "All Users"

for(i in c(1,7,28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
} 

# Plots: Triangle distributions for users in first day, week, month. (Only look at FamilyLife)


subsetcontents <- user_table %>%
  filter(champion_name == "FamilyLife") %>%
  {.$user_id} %>%
  unique

subsetname <- "FamilyLife Users"

for(i in c(1,7,28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: Triangle distributions for users in first day, week, month. (Only look at eventual
# "Champion Others's")

subsetcontents <- mode_pct_data %>%
  filter(mode == "Champion Others") %>% nrow
  {.$user_id} %>% 
  unique

subsetname <- "Eventual Champions"

for(i in c(1, 7, 28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: Triangle distributions for users in first day, week, month. (Only look at TYRO)
  

subsetcontents <- user_table %>%
  filter(champion_name == "TYRO") %>%
  {.$user_id} %>%
  unique

subsetname <- "TYRO Users"

for(i in c(1,7,28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: Eventual champions, don't belong to TYRO

TYRO_users <- user_table %>%
  filter(champion_name == "TYRO") %>%
  {.$user_id} %>%
  unique

eventual_champs <- mode_pct_data %>%
  filter(mode == "Champion Others") %>% 
  {.$user_id} %>% 
  unique

subsetcontents <- setdiff(eventual_champs, TYRO_users)


subsetname <- "Eventual Champions (non-TYRO)"

for(i in c(1,7,28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: TYRO userse that don't go on to champion others

TYRO_users <- user_table %>%
  filter(champion_name == "TYRO") %>%
  {.$user_id} %>%
  unique

eventual_champs <- mode_pct_data %>%
  filter(mode == "Champion Others") %>% 
  {.$user_id} %>% 
  unique

subsetcontents <- setdiff(TYRO_users, eventual_champs)


subsetname <- "TYRO users that don't champion others"

for(i in c(1,7,28)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- 
        ifelse(
          i == 1
          , 'First Day'
          , ifelse(
            i == 7
            , 'First Week'
            , 'First Month'
          )
        )
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: Core users

subsetcontents <- core_users

subsetname <- "Core Users"

for(i in c(1, 7, 7*4, 7*8, 7*12)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- floor(i/7) %>%
        as.character %>%
        {paste("Week", ., sep = " ")}
      
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
}  

# Plots: Summer Connect


subsetcontents <- user_table %>%
  filter(champion_name == "Summer Connect") %>%
  {.$user_id} %>%
  unique

subsetname <- "Summer Connect Users"

for(i in c( 28*7)){
  
  mode_pct_data_by_date %>%
    prepare_modepctdata_for_createconvexdata(user_subset = subsetcontents, number_of_days = i) %>%
    create_convex_data_v2 %>%
    plot_triangle_diagram_v2 %>%
    {
      title <- floor(i/7) %>%
        as.character %>%
        {paste("Week", ., sep = " ")}
      
      return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
    } %>%
    print
  
} 

