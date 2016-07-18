# Function: get_reactivated01

# Input: v - a numeric vector (always the "past28" vector for a given user.)
# Output: A vector of 0s and 1s, of the same length as v. There's a "1" everywhere
#   that v switches from 0 to a positive value.

get_reactivated01 <-
  function(v){
    out <- 0
    if(length(v)==1){
      return(out)
    } else{
      for(i in 2:length(v)){
        out[i] <- as.numeric((v[i-1] == 0) & (v[i] > 0))
      }
      return(out)
    }
  }
