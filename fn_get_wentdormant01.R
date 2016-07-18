# Function: get_wentdormant01

# Input: v - a numeric vector (always the "past28" vector for a given user.)
# Output: A vector of 0s and 1s, of the same length as v. There's a "1" everywhere
#   that v switches from a positive value to 0.

get_wentdormant01 <-
  function(v){
    out <- 0
    if(length(v)==1){
      return(out)
    } else{
      for(i in 2:length(v)){
        out[i] <- as.numeric((v[i-1] > 0) & (v[i] == 0))
      }
      return(out)
    }
  }
