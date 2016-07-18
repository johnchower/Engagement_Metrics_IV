# Function: compare_user_sets

compare_user_sets <- function(df1, df2){
  x <- df1 %>%
    {.$user_id} %>%
    unique   
  y <- df2 %>%
    {.$user_id} %>%
    unique
  
  a <- setdiff(x,y) %>% length 
  b <- setdiff(y,x) %>% length 
  
  return(list(y_minus_x = b, xw_minus_y = a))  
}



