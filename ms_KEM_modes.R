# Master script : KEM modes

library(ggplot2)
library(RColorBrewer)
library(magrittr)
library(plyr)
library(dplyr)
library(chron)
library(plotly)

# Load Data

date_user_table <-
  read.table(
    "date_user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_platformaction_date_time <-
  "(user_date_time_platformaction )\\d{4}(-)\\d{2}(-)\\d{2}(T)\\d{4}(.csv)" %>%
  grep(dir(), value = T) %>%
  read.table(
    header = T
    , sep = ','
    , stringsAsFactors = F
  ) %>%
  rename(
    user_id = User.Dimensions.ID
    , platform_action = User.Platform.Action.Facts.Platform.Action
    , date = Date.Dimensions.Platform.Action.Date
    , time = Date.Dimensions.Platform.Action.Time.of.Day
  ) %>%
  mutate(time = paste(time,":00",sep="")) %>%
  mutate(datetime = chron(date, time, format = c('y-m-d','h:m:s'))) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d")) %>%
  filter(user_id %in% date_user_table$user_id)

platformaction_mode <- read.csv("platformaction_mode.csv", header = T) %>%
  select(-X) %>%
  {
    .$mode[.$platform_action == "Started Session"] <- ""
    return(.)
  }


user_platformaction_date_time_mode <- user_platformaction_date_time %>%
  merge(platformaction_mode)

rm(user_platformaction_date_time)

user_currentsegment <- date_user_table %>%
  filter(date == max(date)) %>%
  select(user_id, current_segment = segment)

user_table <-
  read.table(
    "user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d")) %>%
  merge(user_currentsegment)

rm(user_currentsegment, platformaction_mode)

champname_champid <-
  read.table(
    "champname_champid.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  {
    colnames(.) <- c("champion_id", "champion_name")
    return(.)
  } %>%
  mutate(
    dont.exclude = !(
      grepl("Gloo", champion_name) 
      | grepl("Emily Shirk", champion_name)
      | grepl("Fray", champion_name)
      | grepl("Justin Vallelonga", champion_name)
      | grepl("yolo", champion_name)
      | grepl("Jimmy Mellado", champion_name)
    )
  )

user_table %<>% merge(champname_champid)

rm(champname_champid)



# Subset on core users. Calculate their "class distributions" for the past month. Scatterplot.


source("fn_create_mode_pct_data.r")
mode_pct_data <-  create_mode_pct_data()

# Scatterplot showing distribution of users on triangle
convex_data <- mode_pct_data %>%
  arrange(desc(actions_per_day_past_month)) %>%
  mutate(
    xval = Invest.for.Self.Us_pct + 0.5*Receive.Value_pct
    , yval = (sqrt(3)/2)*Receive.Value_pct
  ) %>%
  mutate(mode = factor(mode)) 

source("fn_plot_triangle_diagram.r")

champions_of_interest <-
  c(
    "FamilyLife"
    , "REVEAL For Church"
    , "REVEAL For Me"
    , "Date Night"
    , "Cru"
    , "InteGREAT"
    , "CeDAR"
    , "TYRO"
    , "Compassion International"
    , "Christian Character Formation Project"
    , "San Diego Summer Mission"
    , "UMI Connection"
  )

source("fn_plot_triangle_diagram.r")
plot_triangle_diagram() %>% {. + ggtitle("Learning Triangle Distribution")} %>% print

for (j in champions_of_interest){
  plot_triangle_diagram(cd = convex_data, n = nrow(convex_data), champname = j) %>% 
    print
}




for(j in champions_of_interest){
  for(i in seq(from = 1, to = round(log(nrow(filter(convex_data, champion_name == j)))), length.out = 5)){
    plot_triangle_diagram(exp(i), j) %>% print
  }
}

# Scatterplot of users in their first week

source("fn_create_mode_pct_data.r")
user_platformaction_date_time_mode %>%
  merge(
    select(user_table, user_id, signup_date = date)
  ) %>%
  filter(date < (signup_date + 7)) %>% 
  create_mode_pct_data %>%
  View
  
  











