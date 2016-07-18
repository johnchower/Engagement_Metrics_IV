

top_user_facts %>%
  select(number_of_actions_past_month, number_of_champion_connections) %>%
  mutate(
    yvar = number_of_champion_connections
  ) %>%
  {lm(number_of_actions_past_month ~ yvar, .)} %>%
  summary

  plot_ly(x = number_of_actions_past_month, y = number_of_champion_connections, mode = "markers")
  chisquare_analysis
  
top_user_facts %>% {.$number_of_actions_past_month} %>% {.[!is.na(.)]} %>% min

top_user_facts %>%
  mutate(xvar = gave_age, yvar = in_top_500) %>%
  {lm(yvar ~ xvar, .)} %>%
  summary

#activitylevel vs cohortmembership 
for(n in user_cutoffs){
  topvar <- paste("in_top", n, sep = "_")
  
userid_cohortid %>%
    group_by(user_id) %>%
    summarise(
      no_cohort = any(is.na(cohort_id))
    ) %>%
    merge(top_user_facts) %>%
    select_(topvar, "no_cohort") %>%
    group_by_(topvar, "no_cohort") %>%
    summarise(count = n()) %>%
    ungroup %>%
    chisquare_analysis %>% 
    {
      print(paste("Tier: Top", n, sep = " "))
      print(paste("Overall p-value:", .$pvalue, sep = " "))
      select_(.$results, .dots = list(quote(no_cohort), topvar, quote(zscore))) %>%
        {
          colnames(.)[2] <- "in_top"
          return(.)
        } %>%
        dcast(no_cohort ~ in_top, value.var = "zscore") %>%
        print
    }
}

# Activitylevel vs gave-age

for(n in user_cutoffs){
  topvar <- paste("in_top", n, sep = "_")
  
  user_demographics %>%
    mutate(gave_age = !is.na(age)) %>%
    select(user_id, gave_age) %>%
    merge(top_user_facts) %>%
    select_(topvar, "gave_age") %>%
    group_by_(topvar, "gave_age") %>%
    summarise(count = n()) %>%
    ungroup %>%
    chisquare_analysis %>% 
    {
      print(paste("Tier: Top", n, sep = " "))
      print(paste("Overall p-value:", .$pvalue, sep = " "))
      select_(.$results, .dots = list(quote(gave_age), topvar, quote(zscore))) %>%
      {
        colnames(.)[2] <- "in_top"
        return(.)
      } %>%
        dcast(gave_age ~ in_top, value.var = "zscore") %>%
        print
    }
}

# Signup date vs. activity level
# View 

# Some users don't have an Account Created date in the database, for some strange
# reason. We're going to use their earliest platform action date as a proxy.

user_facts_proxy_signup_date <- user_facts %>%
  filter(is.na(date)) %>%
  {.$user_id} %>%
  {filter(user_platformaction_datetime, user_id %in% .)} %>%
  group_by(user_id) %>%
  summarise(proxy_signup_date = as.Date(min(date))) %>% 
  merge(user_facts, all = T) %>% 
  mutate(
    date = 
      ifelse(
        is.na(date)
        , proxy_signup_date
        , date
      )
  ) %>% 
  mutate(date = as.Date(date, origin = "1970-01-01"))
  

#

user_facts_proxy_signup_date %>% 
  select(user_id, date, account_type) %>% 
  merge(
    select(
      top_user_facts
      , user_id
      , grep("in_top", colnames(top_user_facts))
    )
  ) %>% filter(account_type != "End User") %>% View
  group_by(date) %>%
  {
    dots <- as.list(paste("sum(", grep("in_top", colnames(top_user_facts), value = T), ")", sep =""))
    names <- grep("in_top", colnames(top_user_facts), value = T)
    dots <- c("n()", dots)
    names <- c("all_users", names)
    summarise_(.,
       .dots = setNames(dots,names)
    )
  } %>% 
  {
    dateframe <- data.frame(date = seq.Date(from = min(.$date), to = Sys.Date(), by = 1))
    merge(., dateframe, all = T)
  } %>% 
  {
    .[is.na(.)] <- 0
    return(.)
  } %>% 
  melt(
    id.vars = "date"
    , variable.name = "user_class"
    , value.name = "number_of_signups"
  ) %>% 
  group_by(user_class) %>%
  mutate(total_signups_in_class = sum(number_of_signups)) %>%
  ungroup %>%
  mutate(percent_of_signups = number_of_signups/total_signups_in_class) %>% {.$total_signups_in_class} %>% unique