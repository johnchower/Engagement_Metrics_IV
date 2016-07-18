# Function: moving_sum
# Returns a moving sum of a given vector with a given window size

# Input: 
#   x - a numeric vector
#   n - an integer (the window size)

moving_sum <- function(x,n){
  if (n >= length(x)){
    return(cumsum(x))
  } else {
    x.past <- cumsum(x[1:n])
    for(i in (n+1):length(x)){
      nextvalue <- x.past[i-1] + x[i] - x[i-n]
      #nextvalue <- sum(x[(i-(n-1)):i])
      x.past <- c(x.past,nextvalue)
    }
    return(x.past)
  }
}



