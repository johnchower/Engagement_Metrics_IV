

source('fn_chisquare_analysis.r')

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

date_percentofsignups <- user_facts_proxy_signup_date %>% 
  filter(user_id %in% standard_user_subset) %>%  
  select(user_id, date, account_type) %>% 
  merge(
    select(
      top_user_facts
      , user_id
      , grep("in_top", colnames(top_user_facts))
    )
  ) %>%  
  group_by(date) %>%
  {
    dots <- as.list(paste("sum(", grep("in_top", colnames(top_user_facts), value = T), ")", sep =""))
    names <- grep("in_top", colnames(top_user_facts), value = T)
    dots <- c("n()", dots)
    names <- c("all_users", names)
    summarise_(.,
       .dots = setNames(dots,names)
    )
  } 

top_user_signup_date_analysis <- date_percentofsignups %>%
  rename(in_top_5000 = all_users) %>%
  mutate(yearmonth = as.Date(format(date, "%Y-%m-01"), origin = "1970-01-01")) %>%
  select(-date) %>%
  melt(id.vars = c("yearmonth"), variable.name = "user_class", value.name = "number_of_users") %>% 
  group_by(yearmonth, user_class) %>%
  summarise(number_of_users = sum(number_of_users)) %>% 
  ungroup %>% 
  arrange(yearmonth, number_of_users, user_class) %>% 
  group_by(yearmonth) %>%
  mutate(
    number_of_users_diff = c(0,diff(number_of_users, differences = 1))
  ) %>% 
  mutate(
    number_of_users_diff =
      ifelse(
        user_class == "in_top_10"
        , number_of_users
        , number_of_users_diff
      )
  ) %>%
#  filter(yearmonth == "2016-04-01") %>% 
  select(yearmonth, user_class, number_of_users_diff) %>%
  ungroup %>%
  chisquare_analysis



%>% 
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
  mutate(percent_of_signups = number_of_signups/total_signups_in_class) 

