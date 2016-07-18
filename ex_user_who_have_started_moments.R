path <- "/Users/johnhower/Downloads/user_date_time_platformaction 2016-06-29T1549.csv"

path %>%
  read.table(
    header = T
    , sep = ','
    , stringsAsFactors = F
  ) %>%
  filter(User.Dimensions.ID %in% standard_user_subset) %>% View