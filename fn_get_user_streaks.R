# Function: get user streaks

# Input : v : a vector of 0s and 1s (always the activitytody vector from date_user_table)

# v <- sample(0:1, 50, replace=T) %>%
#   replace(1,1) #Use this to test results

get_user_streaks <- function(v){
  v %>%
    {. - lag(.,1)} %>%
    replace(1,1)%>% 
    abs %>% 
    cumsum %>% 
    {split(v,.)} %>%
    {.[sapply(.,sum) > 0]} %>%
    sapply(length) %>%
    as.numeric %>%
    return
}