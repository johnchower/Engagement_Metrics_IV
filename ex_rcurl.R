# Exploration: RCurl

library(rcurl)

req <- curlGetHeaders("https://looker.gloo.us")

x <-
  GET(
    "https://looker.gloo.us/explore/gloo/user_platform_action_facts.csv?fields=user_platform_action_facts.platform_action&f[user_dimensions.account_type]=End%20User%2CChampion%20User&sorts=user_platform_action_facts.platform_action&limit=500&query_timezone=America%2FDenver&path_prefix=%2Fexplore&generate_drill_links=true&force_production=false&dynamic_fields=%5B%5D&look_id=1752&apply_formatting=false&title=platform_action_list%202016-06-30T0951.csv"
    , query = list(login = "jhower@tangogroup.com", password = "^&Kes999727")
  )

str(x)

x$request

