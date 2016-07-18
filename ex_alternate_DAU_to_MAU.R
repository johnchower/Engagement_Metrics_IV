# Exploration: Alternate calculateions of overall DAU/MAU ratio
library(MASS)


DAU_to_MAU_subset <- standard_user_subset


DAU_to_MAU_daily_data <- date_user_table %>%
  filter(user_id %in% DAU_to_MAU_subset) %>%
  filter(
    date >= as.Date(format(Sys.Date(), "%Y-01-01"))
    , date < as.Date(format(Sys.Date(), "%Y-%m-01"))
  ) %>%
  select(user_id, date, DAU, MAU) %>% 
  group_by(date) %>%
  summarise(
    number_DAUs = sum(DAU)
    , number_MAUs = sum(MAU)
    , DAU_to_MAU_ratio = number_DAUs/number_MAUs
  ) %>%
  ungroup 

# Playing with some basic models. Started down this road b/c I want to  determine the
# correlation between the two calculation methods.
DAU_to_MAU_daily_data %>%
  {.$DAU_to_MAU_ratio} %>%
  fitdistr("beta", list(shape1=2, shape2 =5))
  
  
  mutate(logratio = log(DAU_to_MAU_ratio)) %>%
  {plot(x = .$date, y = .$DAU_to_MAU_ratio, type = "b")}


  
  log %>%
  
  {c(mean(.), sd(.), shapiro.test(.))}
  hist

DAU_to_MAU_monthly_data <- DAU_to_MAU_daily_data %>%
  mutate(
    month = as.Date(format(date, "%Y-%m-01"))
  ) %>%
  group_by(month) %>%
  summarise(
    DAU_to_MAU_ratio_mean = mean(DAU_to_MAU_ratio)
    ,DAU_to_MAU_ratio_overall = sum(number_DAUs)/sum(number_MAUs)
    )

DAU_to_MAU_overall <- DAU_to_MAU_daily_data %>%
{.$DAU_to_MAU_ratio} %>%
  mean %>%
  round(3) %>%
  percent

plot_name <- "16_DAUtoMAU_ratio"
DAU_to_MAU_monthly_data %>%
  plot_ly(type = "bar", x = month, y = DAU_to_MAU_ratio) %>%
  bar_chart_layout(
    yaxisformat = "%"
    , charttitle = paste("Ratio of Daily to Monthly Active Users (Overall ratio for 2016 is ", as.character(DAU_to_MAU_overall), ")", sep = "")
  ) %>%
  {
    if(save_plots){
      setwd(outloc)
      saveWidget(as.widget(.)
                 , paste(plot_name, ".html", sep = "")
      )  
      setwd("..")
    } else{print(.)}   
  }

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

plot_name <- "17_DAUtoMAU_ratio_by_champion"
DAU_to_MAU_by_champion_data %>%
  plot_ly(type = "bar", x = champion_name, y = overall_DAU_to_MAU_ratio) %>%
  bar_chart_layout(
    charttitle = "Current DAU/MAU Ratio, by Champion"
    , bottommargin = 350
    , yaxisformat = "%"
  ) %>%
  {
    if(save_plots){
      setwd(outloc)
      saveWidget(as.widget(.)
                 , paste(plot_name, ".html", sep = "")
      )  
      setwd("..")
    } else{print(.)}   
  }