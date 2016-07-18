# Exploration: LookR

# install.packages("rJava", type = "source)
# library(devtools)
# install_github("looker/lookr")

library(RJDBC)
library(dplyr) 
library(rjson) 
library(rJava)
library(LookR)

looker_setup(id = "JF5nCp46zkTf29ftJ89S",
             secret = "g4Bbp4KHSp45YM55SBskpwkX",
             api_path = "https://api-looker.gloo.us:443/api/3.0"
)
