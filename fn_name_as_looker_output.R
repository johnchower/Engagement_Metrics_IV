# Function: name_as_looker_output

# Would like to modify this function check to see if the input is already in the required
# format before pasting together.

name_as_looker_output <- function(str){
  paste("(", str, " )\\d{4}(-)\\d{2}(-)\\d{2}(T)\\d{4}(.csv)", sep = "")
}