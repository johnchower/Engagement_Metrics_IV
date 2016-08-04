# Master script: user_state_breakdown_by_month

# Load functions and packages ####

source("fn_moving_sum.r")
source("fn_bar_chart_layout.r")
source("fn_save_or_print.r")
library(googlesheets)
library(dplyr)
library(plyr)
library(plotly)
library(RColorBrewer)

# Load Data ####

date_user_table <- 
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

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

# Set parameters ####

out.loc <- "/Users/johnhower/Google Drive/Analytics_graphs/Cornerstone_Metrics"
year <- 2016
month <- 6

startdate <- month %>%
  {paste(year, ifelse(nchar(.) == 2, ., paste(0, ., sep = "")), "01", sep = "-")} %>%
  as.Date

enddate <- month %>%
  {. + 1} %>%
  {paste(year, ifelse(nchar(.) == 2, ., paste(0, ., sep = "")), "01", sep = "-")}  %>%
  as.Date

interval.length <- as.numeric(enddate - startdate)

# User Subsets ####

standard_user_subset <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    dont.exclude
    , account_type %in% c("End User", "Champion User")
  ) %>% 
  {.$user_id} 

# Calculate 
# New - Did they create their account within the time interval?
# Core - Did they average > 3 active days per week within the time interval?
# Casual, Marginal, Dormant - Same def as Core, but with different #s
# One Session Only - Same def as before. Just check if their state is "one Session" on the last day
# Reactivated - Were they dormant before the time interval, yet use Gloo at some point during the interval?

