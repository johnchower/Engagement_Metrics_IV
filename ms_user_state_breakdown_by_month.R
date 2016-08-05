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
library(reshape2)
library(htmlwidgets)
library(webshot)

summarise <- dplyr::summarise

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

end_and_champion_users <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
    dont.exclude
    , account_type %in% c("End User", "Champion User")
  ) %>% 
  {.$user_id} 

# Calculate breakdown ####
# New - Did they create their account within the time interval?
# Core - Did they average > 3 active days per week within the time interval?
# Casual, Marginal, Dormant - Same def as Core, but with different #s
# One Session Only - Same def as before. Just check if their state is "one Session" on the last day
# Reactivated - Were they dormant before the time interval, yet use Gloo at some point during the interval?

user_breakdown_by_state_data_0 <- date_user_table %>% 
  filter(
    user_id %in% end_and_champion_users
    , date >= startdate - 1
    , date < enddate
  ) %>% 
  mutate(
    in_interval = ((date >= startdate) & (date < enddate))
    , day_before = date == startdate - 1
  ) %>% 
  group_by(user_id) %>% 
  dplyr::summarise(
    active_days_in_interval = sum(activitytoday[in_interval])
    , New = sum(account_created == 1 & in_interval) > 0
    , Reactivated = sum(dormant_user[day_before]) > 0 & active_days_in_interval > 0
    , One_Session_Only = !New & sum(segment == "One Session Only" & date == (enddate - 1)) > 0
    , Core = !New & !Reactivated &!One_Session_Only
        & active_days_in_interval > 3*interval.length/7
    , Casual = !New & !Reactivated &!One_Session_Only
        & active_days_in_interval > 1*interval.length/7 
        & active_days_in_interval <= 3*interval.length/7 
    , Marginal = !New & !Reactivated &!One_Session_Only
        & active_days_in_interval > 0*interval.length/7 
        & active_days_in_interval <= 1*interval.length/7
    , Dormant = !New & !Reactivated &!One_Session_Only
        & active_days_in_interval == 0
  ) %>% 
  ungroup %>%
  select(-active_days_in_interval)

user_breakdown_by_state_data <- user_breakdown_by_state_data_0 %>%
  melt(id.vars = "user_id", variable.name = "user_state", value.name = "true_false") %>%
  filter(true_false) %>%
  select(-true_false) %>%
  group_by(user_state) %>%
  summarise(number_of_users = n()) %>%
  mutate(
    total_number_of_users = sum(number_of_users)
    , percent_of_users = number_of_users/total_number_of_users) %>%
  arrange(desc(number_of_users))

# Graph breakdown ####

# Produce user breakdown plot
a <- list()
for (i in seq_len(nrow(user_breakdown_by_state_data))) {
  m <- user_breakdown_by_state_data[i, ]
  a[[i]] <- list(
    x = m$user_state
    , y = m$percent_of_users
    , text = prettyNum(m$number_of_users, big.mark = ",")
    , xref = "x"
    , yref = "y"
    , showarrow = F
    , yanchor = "bottom"
  )
}


user_breakdown_by_state_data %>%
{
  plot_ly(.,
          type = "bar"
          , marker = list(color = "gray")#list(color = brewer.pal(nrow(.), "Dark2"))
          , x = user_state
          , y = percent_of_users
          , text = paste(100*percent_of_users, "% (", as.character(number_of_users), " users)", sep = "")
          , mode = "markers"
          , hoverinfo = "text"
          #      , color = "gray"
  )
} %>%
  layout(showlegend = F, annotations = a, yaxis = list(showgrid = F)) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users in Each State - "
        , months(startdate)
        , " "
        , year
        , " ("
        , prettyNum(user_breakdown_by_state_data$total_number_of_users[1], big.mark = ",")
        , " Total Users)"
        , sep = ""
      ) 
    , yaxisformat = "%"
    , bottommargin = 150
  ) %>%
  save_or_print(
    outloc = out.loc
    , plot_name = paste("user_state_breakdown", year, months(startdate), sep = "_")
    , outformat = "pdf"
  )

# Open the pdf files to check that they look good.
out.loc %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("open ", ., "/user_state_breakdown*.pdf", sep = "")} %>%
  system

# Remove extra html file

out.loc %>%
  {gsub("Google Drive", "'Google Drive'", .)} %>%
  {paste("rm ", ., "/user_state_breakdown*.html", sep = "")} %>%
  system


