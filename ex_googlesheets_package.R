# Exploration: googlesheets package

library(googlesheets)
library(dplyr)
gs_auth()
gs_user()
View(gs_ls())


x <- gs_title("core_user_metrics") %>% 
  gs_read

x <- gs_copy(gs_key("1j2aOKj7pxkvkwHgNzq72mFBBF9AgMz5nP4Tny6LzrfM"), to = "x")
x <- gs_add_row(x, ws = gs_key("1j2aOKj7pxkvkwHgNzq72mFBBF9AgMz5nP4Tny6LzrfM"), input = core_user_by_actual_actions)

gs_add_row(
  gs_key("1j2aOKj7pxkvkwHgNzq72mFBBF9AgMz5nP4Tny6LzrfM")
  , input = core_user_by_actual_actions
) 

View(gs_ls())
