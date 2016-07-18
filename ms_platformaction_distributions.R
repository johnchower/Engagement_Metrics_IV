# Master Script: platformaction_distributions

library(dplyr)
library(ggplot2)
library(reshape2)
library(plotly)
library(RColorBrewer)
library(scales)
library(magrittr)
library(htmlwidgets)

layout <- plotly::layout

source("fn_bar_chart_layout.r")
source("fn_chisquare_analysis.r")
source("fn_save_or_print.r")

# Output Location and parameters ####
save.plots <- T
out.loc <- "/Users/johnhower/Google Drive/Analytics_graphs/Engagement_Performance_Presentation_Slides/2016_07_17"
out.format <- "html"
hallmark.action.threshold <- .001


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

# Define User Subsets ####

# 1. Standard subset
# End users who have never taken a "champion/internal only" platform action, and who
# don't belong to the list of flagged champions.

standard_user_subset <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
#    dont.exclude
#    , !fake_end_user
#    , 
    account_type == "End User") %>%
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

# Set user subset ####

chisquare_subset <- standard_user_subset

# Output Location and parameters ####
save_plots <- T
outloc <- "FamilyLife_chisquare_analysis"

# Set up data for chisquare analysis ####

platformaction_chisquare_analysis_data <- user_platformaction_datetime %>%
  filter(user_id %in% chisquare_subset) %>%
  merge(select(user_facts, user_id, current_segment)) %>% 
  merge(select(platformaction_facts, platform_action, group)) %>%
  group_by(current_segment, group) %>%
  summarise(count = n()) %>%
  filter(
    group != ""
    , group != "Started Session"
    , group != "Account Created"
  ) %>%
  rename(platform_action = group)

# Run Chisquare analysis ####
platformaction_chisquare_analysis_results <-  
  chisquare_analysis(
    platformaction_chisquare_analysis_data
    , err_pvalue = 1 - hallmark.action.threshold
  )
  

# Set hallmark actions ####

hallmark_actions <- platformaction_chisquare_analysis_results$results %>% 
  group_by(platform_action) %>%
  filter(min(pvalue) < hallmark.action.threshold) %>%
  ungroup %>%
  {.$platform_action} %>%
  unique

# Prepare data for plot ####

chisquare_plot_data_All <- user_platformaction_datetime %>%
  filter(user_id %in% chisquare_subset) %>%
  merge(select(platformaction_facts, platform_action, group)) %>%
  group_by(group) %>%
  summarise(count = n()) %>%
  filter(
    group != ""
    , group != "Started Session"
    , group != "Account Created"
  ) %>%
  rename(platform_action = group, number_of_actions = count) %>%
  mutate(
    total_actions = sum(number_of_actions)
    , percent_of_actions = number_of_actions/total_actions
  ) %>%
  filter(platform_action %in% hallmark_actions) %>%
  arrange(desc(number_of_actions))

for(s in c("Core", "Casual", "Marginal", "Dormant", "Reactivated", "New", "One Session Only")){
  
  df <- platformaction_chisquare_analysis_results$results %>%
    filter(
      current_segment == s
    ) %>%
    arrange(desc(marginal2)) %>%
    rename(observed = count) %>%
    select(platform_action, observed, expected, err) %>%
    mutate(
      total_actions = sum(observed)
      , percent_observed = observed/sum(observed)
      , percent_expected = expected/sum(expected)
      , err = err/total_actions
    ) %>%
    filter(platform_action %in% hallmark_actions)
  
  assign(
    paste("chisquare_plot_data_", s, sep = "")
    , df
  )
}

# Generate Plots ####

plot_data <- chisquare_plot_data_All
plot.name <- "20_padistribution_all"

plot_ly(
  plot_data
  , x = platform_action
  , y = percent_of_actions
  , text = 
    paste("(", prettyNum(round(number_of_actions), big.mark = ","), " actions)", sep = "")
  , type = "bar"
  , name = "Percent of Platform Actions"
) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Platform Action Distribution for "
        , "All"
        , " Users ("
        , prettyNum(plot_data$total_actions[1], big.mark = ",")
        , " Total Actions)"
        , sep = ""
      )
    , yaxisformat = "%"
    , bottommargin = 300
    , leftmargin = 150
    , yaxisrange = list(0, .45)
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
  )


statelist <- c("Core", "Casual", "Marginal", "Dormant", "Reactivated", "New", "One Session Only")
for(i in 1:length(statelist)){
  s <- statelist[i]
  plot.name <- 
    paste(
      as.character(i+20)
      , "_padistribution_"
      , gsub(" ", "_", s)
      , sep = ""
    )
  
  plot_data <- get(paste("chisquare_plot_data_", s, sep = ""))
  
    plot_ly(
      plot_data
      , x = platform_action
      , y = percent_expected
      , text = 
          paste("(", prettyNum(round(expected), big.mark = ","), " actions expected)", sep = "")
      , type = "bar"
      , name = "Expected Percent of Platform Actions"
      , error_y = list(array = err, width = 5)
    ) %>%
    add_trace(
      x = platform_action
      , y = plot_data$percent_observed
      , text = 
          paste("(", prettyNum(round(plot_data$observed), big.mark = ","), " actions observed)", sep = "")
      , type = "bar"
      , name = "Observed Percent of Platform Actions"
    ) %>%
    bar_chart_layout(
      charttitle = 
        paste(
          "Platform Action Distribution for '"
          , s
          , "' Users ("
          , prettyNum(plot_data$total_actions[1], big.mark = ",")
          , " Total Actions)"
          , sep = ""
        )
      , yaxisformat = "%"
      , bottommargin = 300
      , leftmargin = 150
      , yaxisrange = list(0, .45)
    ) %>%
      save_or_print(
        save_plots = save.plots
        , outloc = out.loc
        , plot_name = plot.name
        , outformat = out.format
      )
   
}