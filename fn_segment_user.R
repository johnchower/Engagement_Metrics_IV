# Function: segment_user

# Inputs:
#   past28 - user's # of active days in the past 28 days, by date
#   reactivated01 - 1 on days the user reactivates, 0 otherwise
#   activity01 - user's active days

# Outputs:
#     A character vector, with the following values:
#     "New" - The account has been created within the past 28 days
#     "Reactivated" - The account has switched from dormant to active
#       within the past 28 days.
#     "Dormant"   - The user is neither new or reactivated, and past28=0
#     "Marginal"  - The user is neither new or reactivated, and past28 %in% (0,4]
#     "Casual"    - The user is neither new or reactivated, and past28 %in% (4,12]
#     "Core"      - The user is neither new or reactivated, and past28 %in% (12,28]

# Warning: when you run this, make sure that the inputs are arranged by
#   ascending date.

segment_user <-
  function(reactivated01,past28,active01){
    category_breaks <- c(0,4,12,28)
    category_names <- c('Dormant','Marginal','Casual','Core')
    
    out <- NULL
    for (i in (1:length(reactivated01))){
      if (i <= 28){
        nextentry <- "New"
      } else if (sum(active01[1:i])==1){
        nextentry <- "One Session Only"
      } else if (sum(reactivated01[(i-27):i])>0){
        nextentry <- "Reactivated"
      } else {
        index <- (1:(length(category_breaks)))[
          category_breaks==
            min(
              category_breaks[past28[i]<=category_breaks]
            )
          ]
        nextentry <- category_names[index]
      }
      out <- c(out,nextentry)
    }
    return(out)
  }




