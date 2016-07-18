# Function: clear history

clearhistory <- function() {
  +     write("", file=".blank")
  +     loadhistory(".blank")
  +     unlink(".blank")
  + }