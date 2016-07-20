# Master script: plots_for_presentation_v2

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

combine_Cru <- F

# Read in the segmented/classified data frames ####

user_facts <- 
  read.table(
    "user_table.csv"
    , header=TRUE
    , sep=','
    , stringsAsFactors = FALSE
  ) %>%
  mutate(date = as.Date(date,format="%Y-%m-%d"))

user_facts_truefirst <-  
  read.table(
    "user_table_true_first.csv"
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
    dont.exclude
    , account_type == "End User"
  ) %>% 
  {.$user_id} 

# 1.0 Standard Subset (truefirst)

standard_user_subset_truefirst <- user_facts %>%
  merge(select(champion_facts, champion_id, dont.exclude)) %>%
  filter(
        dont.exclude
        , 
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



starttime <- Sys.time()

# Output Location and parameters ####
save.plots <- T
out.loc <- "/Users/johnhower/Google Drive/Analytics_graphs/Engagement_Performance_Presentation_Slides/2016_07_20_v2"
out.format <- "pdf"

# Breakdown of users into current state, champion  ####
current_state_by_champion_subset <- standard_user_subset

current_state_by_champion_data <- user_facts %>%
  filter(user_id %in% current_state_by_champion_subset) %>%
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
  group_by(champion_name, current_segment) %>%
  summarise(number_of_users = n()) %>% 
  ungroup 

# Plot: user breakdown by champion
plot.name <- "01_pct_users_per_champion"

user_breakdown_by_champion_data <- current_state_by_champion_data %>%
  mutate(total_number_of_users = sum(number_of_users)) %>%
  group_by(champion_name) %>%
  summarise(
    number_of_users_per_champion = sum(number_of_users)
    , total_number_of_users = mean(total_number_of_users)
    , percent_of_users_per_champion = number_of_users_per_champion/total_number_of_users
  ) %>% 
  arrange(desc(percent_of_users_per_champion))

user_breakdown_by_champion_data %>%
  plot_ly(
    type = "bar"
    , x = champion_name
    , y = percent_of_users_per_champion
    , text = paste(as.character(number_of_users_per_champion), " users", sep = "")
    , mode = "text"
    , textposition = "top middle"
  ) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users per Champion ("
        , user_breakdown_by_champion_data$total_number_of_users[1]
        , " Total Users)"
        , sep = ""
      )  
    , yaxisformat = "%"
    , bottommargin = 450
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )




# Plot: user breakdown by state
plot.name <- "02_pct_users_per_state"

user_breakdown_by_current_state_data <- current_state_by_champion_data %>%
  group_by(current_segment) %>%
  summarise(number_of_users = sum(number_of_users)) %>%
  mutate(
    total_number_of_users = sum(number_of_users)
    , percent_of_users = number_of_users/total_number_of_users
  ) %>% 
  arrange(desc(percent_of_users))

user_breakdown_by_current_state_data %>%
  plot_ly(
    type = "bar"
    , x = current_segment
    , y = percent_of_users
    , text = paste(as.character(number_of_users), " users", sep = "")
    , mode = "text"
    , textposition = "top middle"
  ) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Percent of Users in Each State ("
        , user_breakdown_by_current_state_data$total_number_of_users[1]
        , " Total Users)"
        , sep = ""
      )  
    , yaxisformat = "%"
    , bottommargin = 200
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )
  

# Plots: user state breakdown by champion
statelist <- c("Core", "Casual", "Marginal", "Dormant", "Reactivated", "New", "One Session Only")
for(i in 1:length(statelist)){
  state <- statelist[i]
  plot.name <- 
    paste(
      "0"
      , as.character(i+2)
      , "_pct_"
      , gsub(" ", "_", state)
      , "_users_per_champion"
      , sep = ""
    )
  
  plot_data <- current_state_by_champion_data %>%
    filter(current_segment == state) %>%
    mutate(total_number_of_users = sum(number_of_users)) %>%
    group_by(champion_name) %>%
    summarise(
      number_of_users_per_champion = sum(number_of_users)
      , total_number_of_users = mean(total_number_of_users)
      , percent_of_users_per_champion = number_of_users_per_champion/total_number_of_users
    ) %>% 
    arrange(desc(percent_of_users_per_champion))
  
  assign(
    paste("user_breakdown_by_current_state_data", state, sep = "_")
    , plot_data
  )
  
  get(paste("user_breakdown_by_current_state_data", state, sep = "_")) %>%
    plot_ly(
      type = "bar"
      , x = champion_name
      , y = percent_of_users_per_champion
      , text = paste(as.character(number_of_users_per_champion), " '", state, "' users", sep = "")
      , mode = "text"
      , textposition = "top middle"
    ) %>%
    bar_chart_layout(
      charttitle = paste("Share of '", state,"' Users per Champion (", as.character(plot_data$total_number_of_users[1]), " Total Users)", sep = "")
      , yaxisformat = "%"
      , bottommargin = 450
    ) %>%
    save_or_print(
      save_plots = save.plots
      , outloc = out.loc
      , plot_name = plot.name
      , outformat = out.format
      , v.width = 1500
      , v.height = 1100
    )
}

# Streaks ####

# top 10s 
streak_subset <- standard_user_subset_truefirst

streak_stats <- date_user_table %>%
  select(user_id, streaks) %>%
  filter(
    streaks > 0
    , user_id %in% streak_subset
  ) %>%
  merge(
    select(user_facts, user_id, champion_id)
  ) %>%
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
    return(out)
  } %>%
  select(-champion_id, -user_id) %>%
  group_by(champion_name) %>%
  summarise(
    mean_streak = mean(streaks)
    , max_streak = max(streaks)
  )

top_champ_by_mean_streak_data <-  
  streak_stats %>%
  select(champion_name, mean_streak) %>%
  arrange(desc(mean_streak))  %>%
  slice(1:10)

mean_streak_winner <- top_champ_by_mean_streak_data %>%
  slice(1) %>%
  {.$champion_name}

mean_streak_winner_users <- user_facts %>%
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
} %>%
  filter(champion_name == mean_streak_winner) %>%
  {.$user_id}

top_champ_by_max_streak_data <-
  streak_stats %>%
  select(champion_name, max_streak) %>%
  arrange(desc(max_streak))  %>%
  slice(1:10)

max_streak_winner <- top_champ_by_max_streak_data %>%
  slice(1) %>%
  {.$champion_name} 

max_streak_winner_users <- user_facts %>%
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
} %>%
  filter(champion_name == max_streak_winner) %>%
  {.$user_id}

plot.name <- "13_top10_by_mean_streak"
top_champ_by_mean_streak_data %>%
  plot_ly(type = "bar", x = champion_name, y = mean_streak) %>%
  bar_chart_layout(
    charttitle = "Top 10 Champions by Average Streak Length"
    , yaxisformat = "n"
    , bottommargin = 300
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

plot.name <- "11_top10_by_max_streak"
top_champ_by_max_streak_data %>%
  plot_ly(type = "bar", x = champion_name, y = max_streak) %>%
  bar_chart_layout(
    charttitle = "Top 10 Champions by Maximum Streak Length"
    , yaxisformat = "n"
    , bottommargin = 300
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

# Histograms

overall_histogram_data <- date_user_table %>%
  filter(
    streaks > 0
    , user_id %in% streak_subset
  ) %>%
  group_by(streaks) %>%
  summarise(count = n())

max_streak_winnner_histogram_data <- date_user_table %>%
  filter(
    streaks > 0
    , user_id %in% intersect(streak_subset, max_streak_winner_users)
  )%>%
  group_by(streaks) %>%
  summarise(count = n())

mean_streak_winner_histogram_data <- date_user_table %>%
  filter(
    streaks > 0
    , user_id %in% intersect(streak_subset, mean_streak_winner_users)
  )%>%
  group_by(streaks) %>%
  summarise(count = n())

plot.name <- "10_histogram_of_user_active_streaks"
overall_histogram_data %>%
  plot_ly(type = "bar", x = streaks, y = count) %>%
  bar_chart_layout(
    charttitle = "Histogram of User Active Streaks"
    , yaxisformat = NULL
    , xaxistitle = "Streak Length"
    , yaxistitle = "Number of Streaks"
    , xaxisrange = c(.25, max(overall_histogram_data$streaks))
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

plot.name <- "12_histogram_of_user_active_streaks_max_winner"
max_streak_winnner_histogram_data %>%
  plot_ly(type = "bar", x = streaks, y = count) %>%
  bar_chart_layout(
    charttitle = 
      paste(
        "Histogram of User Active Streaks -"
        , max_streak_winner
        , sep = " "
      )
    , yaxisformat = NULL
    , xaxistitle = "Streak Length"
    , yaxistitle = "Number of Streaks"
    , xaxisrange = c(.25, max(overall_histogram_data$streaks))
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

if(mean_streak_winner != max_streak_winner){
  plot.name <- "14_histogram_of_user_active_streaks_mean_winner"
  mean_streak_winner_histogram_data %>%
    plot_ly(type = "bar", x = streaks, y = count) %>%
    bar_chart_layout(
      charttitle = 
        paste(
          "Histogram of User Active Streaks -"
          , mean_streak_winner
          , sep = " "
        )
      , yaxisformat = NULL
      , xaxistitle = "Streak Length"
      , yaxistitle = "Number of Streaks"
      , xaxisrange = c(.25, max(overall_histogram_data$streaks))
    ) %>%
    save_or_print(
      save_plots = save.plots
      , outloc = out.loc
      , plot_name = plot.name
      , outformat = out.format
      , v.width = 1500
      , v.height = 1100
    )
}

# MAU line chart ####
MAU_line_subset <- standard_user_subset

MAU_line_data <- date_user_table %>%
  select(user_id, date, DAU, WAU, MAU) %>%
  filter(
    user_id %in% MAU_line_subset
    , date >= "2016-01-01") %>%
  group_by(date) %>%
  summarise(Total_MAUs = sum(MAU), Total_WAUs = sum(WAU), Total_DAUs = sum(DAU)) %>%
  melt(
    id.vars = "date"
    , variable.name = "User_Class"
    , value.name = "Number_of_Users"
  ) 

plot.name <- "15_total_AU_line_chart"
MAU_line_data %>%
  plot_ly(type = "scatter", x = date, y = Number_of_Users, group = User_Class) %>%
  bar_chart_layout(
    charttitle = "Total Number of Active Users"
    , yaxisrange = NULL
    , yaxisformat = "n"
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

# DAU/MAU ratio, through time ####

DAU_to_MAU_subset <- standard_user_subset


DAU_to_MAU_daily_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_subset) %>%
  filter(
    date >= as.Date(format(Sys.Date(), "%Y-01-01"))
    , date < as.Date(format(Sys.Date(), "%Y-%m-01"))
  ) %>%
  select(user_id, date, DAU, MAU) %>% 
  group_by(date) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup 

DAU_to_MAU_monthly_data <- DAU_to_MAU_daily_data %>%
  mutate(
    month = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  group_by(month) %>%
  summarise(DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio))

DAU_to_MAU_overall <- DAU_to_MAU_daily_data %>%
{.$DAU_to_MAU_ratio} %>%
  mean %>%
  round(3) %>%
  percent

plot.name <- "16_DAUtoMAU_ratio"
DAU_to_MAU_monthly_data %>%
  plot_ly(type = "bar", x = month, y = DAU_to_MAU_ratio) %>%
  bar_chart_layout(
    yaxisformat = "%"
    , charttitle = paste("Ratio of Daily to Monthly Active Users (Overall ratio for 2016 is ", as.character(DAU_to_MAU_overall), ")", sep = "")
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

# Current DAU/MAU ratio, by champion (data done, still needs plotting)####
DAU_to_MAU_by_champion_subset <- standard_user_subset



DAU_to_MAU_by_champion_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_by_champion_subset) %>%
  filter(
    date > max(date) - 7
    , 
    date <= max(date)
  ) %>%
  merge(
    select(user_facts, user_id, champion_id)
  ) %>%
  {
    if(combine_Cru){
      out <- 
        merge(.,
              select(champion_facts, champion_id, champion_organization)  
        )
      rename(out, champion_name = champion_organization)
    } else{
      out <-
        merge(.,
              select(champion_facts, champion_id, champion_name)  
        )
    }
  } %>%
  group_by(date, champion_name) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup %>%
  group_by(champion_name) %>%
  summarise(overall_DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio, na.rm = T)) %>%
  ungroup %>%
  arrange(desc(overall_DAU_to_MAU_ratio))

plot.name <- "17_DAUtoMAU_ratio_by_champion"
DAU_to_MAU_by_champion_data %>%
  plot_ly(type = "bar", x = champion_name, y = overall_DAU_to_MAU_ratio) %>%
  bar_chart_layout(
    charttitle = "Current DAU/MAU Ratio, by Champion"
    , bottommargin = 450
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )
# Year-to-date DAU/MAU ratio, by champion ####
DAU_to_MAU_by_champion_subset <- standard_user_subset

DAU_to_MAU_by_champion_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_by_champion_subset) %>%
  filter(
    date > as.Date(format(max(date), "%Y-01-01"))
    , 
    date <= max(date)
  ) %>%
  merge(
    select(user_facts, user_id, champion_id)
  ) %>%
  {
    if(combine_Cru){
      out <- 
        merge(.,
              select(champion_facts, champion_id, champion_organization)  
        )
      rename(out, champion_name = champion_organization)
    } else{
      out <-
        merge(.,
              select(champion_facts, champion_id, champion_name)  
        )
    }
  } %>%
  group_by(date, champion_name) %>%
  summarise(DAU_to_MAU_ratio = sum(DAU)/sum(MAU)) %>%
  ungroup %>%
  group_by(champion_name) %>%
  summarise(overall_DAU_to_MAU_ratio = mean(DAU_to_MAU_ratio, na.rm = T)) %>%
  ungroup %>%
  arrange(desc(overall_DAU_to_MAU_ratio))

plot.name <- "17_1_DAUtoMAU_ratio_by_champion_ytd"
DAU_to_MAU_by_champion_data %>%
  plot_ly(type = "bar", x = champion_name, y = overall_DAU_to_MAU_ratio) %>%
  bar_chart_layout(
    charttitle = paste("Year-to-Date DAU/MAU Ratio, by Champion (", format(max(date_user_table$date), "%Y"), ")", sep = "")
    , bottommargin = 450
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

# MAU bar charts #### 
MAU_bar_subset <- standard_user_subset

MAU_bar_data_absolute <- date_user_table %>%
  select(user_id, date, MAU, segment) %>%
  filter(
    user_id %in% MAU_bar_subset
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
    total = Casual + Core + Marginal + New + Reactivated
    , Casual = Casual/total
    , Core = Core/total
    , Marginal = Marginal/total
    , New = New/total
    , Reactivated = Reactivated/total
  ) %>%
  select(-total)

core_to_new_data <- MAU_bar_data_absolute %>%
  mutate(core_to_new_user_ratio = Core/New)

MAU_bar_chart_list <- plot_MAU_bar_chart(MAU_bar_data_relative, MAU_bar_data_absolute)

plot.name <- "18_MAUs_by_user_state_through_time_absolute"
MAU_bar_chart_list[[1]] %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

plot.name <- "19_MAUs_by_user_state_through_time_relative"
MAU_bar_chart_list[[2]] %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

plot.name <- "Core_to_New_User_Ratio"
core_to_new_data %>%
  plot_ly(
    x = date
    , y = core_to_new_user_ratio
    , type = "scatter"
    , text = percent(round(core_to_new_user_ratio, 3))
    , hoverinfo = "text"
  ) %>%
  bar_chart_layout(
    charttitle = "Ratio of Core Users to New Users"
    , yaxisrange = NULL
    , yaxisformat = "%"
  ) %>%
  save_or_print(
    save_plots = save.plots
    , outloc = out.loc
    , plot_name = plot.name
    , outformat = out.format
    , v.width = 1500
    , v.height = 1100
  )

endtime <- Sys.time()
endtime-starttime


