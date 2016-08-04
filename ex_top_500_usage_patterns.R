# Exploration: Top 500 usage patterns

# Runs after ex_top_500_active_users.r (need to restart R session first)

# Packages and functions ####
library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(plyr)
library(dplyr)
library(chron)
library(plotly)
library(htmlwidgets)
library(webshot)

source("fn_create_mode_pct_data.r")
source("fn_create_mode_pct_data_by_date.r")
source("fn_create_convex_data.r")
source("fn_create_convex_data_v2.r")
source("fn_plot_triangle_diagram_v2.r")
source("fn_prepare_modepctdata_for_createconvexdata.r")
source("fn_plot_triangle_progress.r")
source("fn_cohort_breakdown.r")
source("fn_bar_chart_layout.r")
source("fn_chisquare_analysis.r")
source("fn_produce_chisquare_plot_data.r")
source("fn_create_chisquare_plots.r")

layout <- plotly::layout

# Parameters 

out.loc <- "/Users/johnhower/Google Drive/Analytics_graphs/top_500_active_users"
  
  
# User subsets ####

standard_user_subset <- user_facts %>% 
  #  merge(select(champion_facts, champion_id, dont.exclude)) %>% 
  filter(
    account_type == "End User"
  ) %>% 
  {.$user_id} 

FamilyLife_users %>% user_facts %>%

top_10 <- top_users_list$top_10
top_50 <- top_users_list$top_50
top_100 <- top_users_list$top_100
top_250 <- top_users_list$top_250
top_500 <- top_users_list$top_500

FamilyLife_users <- user_facts %>%
  filter(champion_id == 6) %>%
  {.$user_id} %>%
  unique

cohort_none <- userid_cohortid %>%
  filter(is.na(cohort_id)) %>%
  {.$user_id} %>%
  unique

cohort_students <- userid_cohortid %>%
  merge(cohortid_cohortname, by = "cohort_id") %>% 
  merge(cohortid_champid, by = "cohort_id") %>%
  merge(select(champion_facts, champion_id, champion_name), by = "champion_id") %>%
  select(user_id, cohort_name, champion_name) %>%
  dlply(
    .variables = .(cohort_name, champion_name)
    , .fun = function(df)unique(df$user_id)
  )

cohort_none_by_champ <- user_facts %>%
  filter(user_id %in% cohort_none) %>%
  merge(select(champion_facts, champion_name, champion_id), by = "champion_id") %>%
  dlply(
    .variables = .(champion_name)
    , .fun = function(df)unique(df$user_id)
  )


# Prepare data for triangle plot ####

# Create "time since account creation" field ####
starttime <- Sys.time()
user_platformaction_date_time_mode_timesincecreation <- user_platformaction_datetime %>%
  merge(
    select(platformaction_facts, platform_action, mode)
  ) %>% #filter(user_id %in% sample(user_facts$user_id, 1000, replace = F)) %>%
  group_by(user_id) %>%
  mutate(
    created_date = rep(date[platform_action == "Account Created"][1], times = n())
    , time_since_creation = difftime(date, created_date, units = "days")
  ) %>% 
  ungroup 

endtime <- Sys.time()
(endtime - starttime)

# First round of aggregations

mode_pct_data <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data

convex_data <- mode_pct_data %>%
  create_convex_data

user_table <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) 

mode_pct_data_by_date <- user_platformaction_date_time_mode_timesincecreation %>%
  create_mode_pct_data_by_date

rm(user_table)

# Plot activity of each tier through time. ####

top_users_list %>%
  names %>%
  lapply(
    FUN = function(name){
      v <- unlist(top_users_list[name])
      
      for(i in c(1, 7, 14, 21, 28)){
        
        mode_pct_data_by_date %>%
          prepare_modepctdata_for_createconvexdata(user_subset = v, number_of_days = i, maxdate = Sys.Date()) %>%
          create_convex_data_v2 %>%
          plot_triangle_diagram_v2 %>%
          {
            title <- 
              paste("Day", i, sep = " ")
            return(. + ggtitle(paste(name, "-", title, sep = " ")))
          } %>%
          print
        
      } 
    }
  )

# Cohort breakdowns ####



cohort_breakdown_list <- top_users_list %>%
  llply(
    .fun = cohort_breakdown
  )

big_cohorts <- c(
  "No cohort - FamilyLife"
  , "Cohort - Christian CFP"
  , "Cohort - Civic CFP"
  , "No cohort - Gloo"
  #  , "Cohort - Cru"
  , "Cohort - Family Bridges"
)

# Histogram of cohort breakdowns by tier ####

cohort_summary_list <- cohort_breakdown_list %>%
  names %>%
  lapply(
    FUN = function(name){
      chartname <- gsub("_", " ", name)
      df <- cohort_breakdown_list[[name]]
      user_count <- df %>%
        {.$user_id} %>%
        unique %>%
        length
      
      summary_df <- df %>%
        group_by(description) %>%
        summarise(number_of_users = length(unique(user_id))) %>% 
        arrange(desc(number_of_users)) %>%
        filter(
          description %in% big_cohorts
        )
      
      summary_df %>%
        plot_ly(x = description, y = number_of_users, type = "bar") %>%
        bar_chart_layout(
          charttitle = paste("Cohort Breakdown for", chartname, "Users", sep = " ")
          , bottommargin = 250
          , yaxistitle = "Number of Users"
        ) %>%
        save_or_print(
          save_plots = T
          , outloc = out.loc
          , plot_name = paste("cohort_breakdown_", name, sep = "")
          , outformat = "pdf"
        )
      
      summary_df %>%
        {.$description} %>%
        return
    }
  )

out.loc %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("open ", ., "/cohort_breakdown*.pdf", sep = "")} %>%
  system

# Triangle diagrams of each cohort

# Plot triangle diagrams for each intersect(top_500), important_cohort) combination. ####

for(i in length(user_cutoffs)){
  important_cohorts <- cohort_summary_list[[i]]
  user_subset_0 <- top_users_list[[i]]
  cohort_breakdown <- cohort_breakdown_list[[i]]
  tier_size <- user_cutoffs[i]
  
  for(cohort in important_cohorts){
    friendly_cohort <- cohort %>%
      {gsub(" ", "_", .)} %>%
      {gsub("_-_", "_", .)}
    
    outloc <- out.loc
    plot_name <- paste("triangle_distribution_top_500_intersect", friendly_cohort, sep = "_")
    
    top_user_cohort_set <- cohort_breakdown %>%
      filter(description == cohort) %>%
      {.$user_id} %>%
      unique
    
    
    
    mode_pct_data_by_date %>% 
      plot_triangle_progress(
        user_subset = top_user_cohort_set
        , subsetname = paste("Users from top", tier_size, "who belong to", cohort, sep = " ")
        , days_since_signup = 28
        , filename = paste(plot_name, ".pdf", sep = "")
        , path = outloc
        , width = 12
        , height = 9
      ) 
  }
}

# Open the pdf files to check that they look good.
out.loc %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("open ", ., "/*intersect*.pdf", sep = "")} %>%
  system

# Plot CFP cohorts that show clustering ####
library(chron)

cohort_untitled_invitation_facts <- 
  read.csv("cohort_untitled_invitation_facts.csv", stringsAsFactors = F) %>%
  mutate(
    createddate = substr(created_at, 1, 10)
    , createdtime = substr(created_at, 11, 19)
    , created_date_time = chron(createddate, createdtime, format = c(dates = "y-m-d", times = "h:m:s"))
  ) 

for(j in c(14, 32)){
  if(j == 14){project <- "Christian"} else {project <- "Civic"}
  
  CFP_cohort_cluster <- cohort_untitled_invitation_facts %>%
    filter(champion_id == j) %>% # Christian = 14, Civic = 32
    arrange(created_date_time) 
  
  
  for(i in 1:nrow(CFP_cohort_cluster)){
    
    cohort <- CFP_cohort_cluster %>%
      slice(i) %>%
      {.$id}
    
    user_subset <- cohort %>%
      {filter(userid_cohortid, cohort_id %in% .)} %>%
      {.$user_id} %>%
      unique %>%
      intersect(top_users_list$top_500)
    
    if(length(user_subset) > 10){
      subset.name = paste(project, "CFP", "cohort", cohort, sep = " ")
      
      friendly_subset.name <- subset.name %>%
        {gsub(" ", "_", .)} %>%
        {gsub("_-_", "_", .)}
      
      plot_name <- paste("triangle_distribution", friendly_subset.name, sep = "_")
      
      
      mode_pct_data_by_date %>% 
        plot_triangle_progress(
          user_subset = user_subset
          , subsetname = subset.name
          , days_since_signup = 28
          , save_plots = T
          , filename = paste(plot_name, ".pdf", sep = "")
          , path = outloc
          , width = 12
          , height = 9 
        )
    }
     
  }
}

  # Open the pdf files to check that they look good.
  out.loc %>%
    {gsub("Google Drive", "'Google Drive'", .)} %>%
    {paste("open ", ., "/triangle_distribution_C*.pdf", sep = "")} %>%
    system
  
# Make triangle distribution for the top_n users ####
  
for(i in 1:length(top_users_list)){
  tier_name <- names(top_users_list)[i]
  tier_name_spaces <- gsub("_", " ", tier_name)
  plot_name <- paste("triangle_distribution", tier_name, sep = "_")
  user.set <- top_users_list[[tier_name]]
  
  mode_pct_data_by_date %>% 
    plot_triangle_progress(
      user_subset = user.set
      , subsetname = paste("Users from", tier_name_spaces, sep = " ")
      , days_since_signup = 28
      , filename = paste(plot_name, ".pdf", sep = "")
      , path = outloc
      , width = 12
      , height = 9
    ) 
}

# Open the pdf files to check that they look good.
out.loc %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("open ", ., "/triangle_distribution_top_*.pdf", sep = "")} %>%
  system

# Chisquare analysis of platform actions taken by cohorts, etc. ####

chisquare_results_top500 <- user_platformaction_datetime %>%
  merge(select(platformaction_facts, platform_action, group)) %>%
  merge(select(cohort_breakdown_list$top_500,user_id, description), all.x = T) %>%
  rename(cohort_description = description) %>%
  mutate(
    cohort_description = ifelse(
      is.na(cohort_description)
      , "Standard User"
      , cohort_description
    )
  ) %>%
  group_by(cohort_description, group) %>%
  summarise(count = n()) %>% 
  ungroup %>%
  chisquare_analysis

tempout <- chisquare_results_top500$results %>%
  produce_chisquare_plot_data %>% 
  {.[big_cohorts]} %>%
  llply(
    .fun = function(df){
      df %>% 
        filter(
          pvalue < .01
          , !(group %in% c(
            ""
            , "Answered Assessment Item"
            , "Account Created"
            , "Started Session"
          ))
        ) %>%
        return
    }
  )
   

plotlist <- tempout %>%
  create_chisquare_plots(
    yaxisformat = "%"
    , yaxisrange = c(0, .16)
    , bottommargin = 200
  )

plotlist %>%
  names %>%
  lapply(
    FUN = function(name){
      out_plot <- plotlist[[name]]
      
      friendly_name <- name %>%
        {gsub(" ", "_", .)} %>%
        {gsub("_-_", "_", .)}
      
      save_or_print(
        out_plot
        , outloc = out.loc
        , plot_name = paste("platform_actions_top_500", friendly_name, sep = "_")
        , outformat = "pdf"
        , v.width = 1200
        , v.height = 900
      )
    }
  )

 