# Segmented data for a representative champ


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
source("fn_line_chart_layout.r")
source("fn_plot_MAU_bar_chart.r")
source("fn_save_or_print.r")

combine_Cru <- T

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

# 4. TYRO

TYRO_users <- user_facts %>%
  merge(select(champion_facts, champion_id, champion_name)) %>%
  filter(champion_name == "TYRO") %>%
  {.$user_id}


# Set parameters

out.loc <- "/Users/johnhower/Google Drive/Analytics_graphs/TYRO_user_distribution"
save.plots <- T
out.format <- "png"

champion <- "TYRO"
user_facts %>%
  group_by(champion_id, current_segment) %>%
  summarise(number_of_users = n()) %>%
  merge(select(champion_facts, champion_id, champion_name)) %>% 
  filter(champion_name == champion) %>%
  arrange(desc(number_of_users)) %>% 
  plot_ly(x = current_segment, y = number_of_users, type = "bar") %>%
  bar_chart_layout(
    charttitle = paste("User State Distribution -", champion, sep = " ")
    , bottommargin = 200
  ) %>%
  save_or_print(
    save_plots = T
    , outloc = out.loc
    , plot_name = 
        gsub("-", ""
          , gsub(" ", "_"
            , paste("User State Distribution -", champion, sep = " ")
            )
        )
    , outformat = "png"
  )

  # MAU bar charts #### 
  MAU_bar_subset <- standard_user_subset
  out.format <- "png"
  
  MAU_bar_data_absolute <- date_user_table %>%
    select(user_id, date, MAU, segment) %>%
    filter(
      user_id %in% TYRO_users
      , date >= "2016-01-01") %>%
    group_by(date, segment) %>%
    summarise(number_of_users = n()) %>%
    dcast(date ~ segment, value.var = "number_of_users") %>%
    {
      .[is.na(.)] <- 0
      colnames(.)[7] <- "One_Session_Only"
      return(.)
    } 
  
  MAU_bar_data_relative <- MAU_bar_data_absolute %>%
    mutate(
      total = Casual + Core + Marginal + New + Reactivated + One_Session_Only + Dormant
      , Casual = Casual/total
      , Core = Core/total
      , Marginal = Marginal/total
      , New = New/total
      , Reactivated = Reactivated/total
      , One_Session_Only = One_Session_Only/total
      , Dormant = Dormant/total
    ) %>%
    select(-total)
  
  MAU_bar_chart_list <- plot_MAU_bar_chart(MAU_bar_data_relative, MAU_bar_data_absolute, include_dormant = T)
  
  plot.name <- "TYRO_user_distribution_through_time_absolute"
  MAU_bar_chart_list[[1]] %>%
    layout(title = "TYRO User Distribution Through Time") %>%
    save_or_print(
      save_plots = save.plots
      , outloc = out.loc
      , plot_name = plot.name
      , outformat = out.format
    )
  
  plot.name <- "TYRO_user_distribution_through_time_relative"
  MAU_bar_chart_list[[2]] %>%
    layout(title = "TYRO Distribution Through Time") %>%
    save_or_print(
      save_plots = save.plots
      , outloc = out.loc
      , plot_name = plot.name
      , outformat = out.format
    )
  