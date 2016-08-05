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

years <- c(2016, 2016)
months <- c(6, 7)


date_df <- data.frame(year = years, month = months)
rm(years, months)

user_breakdown_by_state_year_month_data <- date_df %>%
  ddply(
    .variables = .(year, month)
    , .fun = function(df){
      month <- df$month[1]
      year <- df$year[1]
      
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
        mutate(
          user_state = gsub("_", " ", user_state)
        ) %>%
        filter(true_false) %>%
        select(-true_false) %>%
        group_by(user_state) %>%
        summarise(number_of_users = n()) %>%
        mutate(
          total_number_of_users = sum(number_of_users)
          , percent_of_users = number_of_users/total_number_of_users) %>%
        arrange(desc(number_of_users))
      
      return(user_breakdown_by_state_data)
    }
  )




# Graph breakdown ####

# Produce user breakdown plot
plotdata_1 <- user_breakdown_by_state_year_month_data %>%
  filter(year == date_df$year[1], month == date_df$month[1])

plotdata_2 <- user_breakdown_by_state_year_month_data %>%
  filter(year == date_df$year[2], month == date_df$month[2])

a <- list()
for (i in seq_len(nrow(user_breakdown_by_state_data))) {
  m <- plotdata_1[i,]
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

b <- list()
for (i in seq_len(nrow(user_breakdown_by_state_data))) {
  m <- plotdata_2[i,]
  b[[i]] <- list(
    x = m$user_state
    , y = m$percent_of_users
    , text = prettyNum(m$number_of_users, big.mark = ",")
    , xref = "x"
    , yref = "y"
    , showarrow = F
    , yanchor = "bottom"
  )
}






out_plot_1 <- plotdata_1 %>%
  {
    plot_ly(.,
            type = "bar"
            , marker = list(color = "#888888")
            , x = user_state
            , y = percent_of_users
            , text = paste(100*percent_of_users, "% (", as.character(number_of_users), " users)", sep = "")
            , mode = "markers"
            , hoverinfo = "text"
            , name = paste(month.name[date_df$month[1]], date_df$year[2], sep = " ")
    )
  } 

out_plot_2 <- out_plot_1 %>%
  add_trace(
    type = "bar"
    , marker = list(color = "#1fbdd6")
    , x = plotdata_2$user_state
    , y = plotdata_2$percent_of_users
    , text = paste(100*plotdata_2$percent_of_users, "% (", as.character(plotdata_2$number_of_users), " users)", sep = "")
    , mode = "markers"
    , hoverinfo = "text"
    , name = paste(month.name[date_df$month[2]], date_df$year[2], sep = " ")
  ) %>%
  layout(yaxis = list(showgrid = F)) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users in Each State - <br>"
        , paste(month.name[date_df$month[1]], date_df$year[1], sep = " ")
        , " (N ="
        , prettyNum(user_breakdown_by_state_year_month_data$total_number_of_users[1], big.mark = ",")
        , " Users), <br>"
        , paste(month.name[date_df$month[2]], date_df$year[2], sep = " ")
        , " (N ="
        , prettyNum(user_breakdown_by_state_year_month_data$total_number_of_users[8], big.mark = ",")
        , " Users) <br>"
        , sep = ""
      ) 
    , yaxisformat = "%"
    , bottommargin = 150
  ) 
out_plot_2

out_plot_2 %>%
  save_or_print(
    outloc = out.loc
    , plot_name = paste("user_state_breakdown", date_df$year[2], month.name[date_df$month[2]], sep = "_")
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


