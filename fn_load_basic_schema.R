# Function: load_basic_schema

load_basic_schema <- 
  function(
    path_user_platformaction_date_time = "input_csvs/user_date_time_platformaction"
    , 
    path_user_facts = "input_csvs/user_signupdate_champid_accounttype_email"
    , 
    path_to_allowed_actions = "input_csvs/platformaction_enduserallowed.csv"
    , 
    path_to_champid_champname = "input_csvs/champid_champname"
    , 
    path_platformaction_facts = "input_csvs/platformaction_facts.csv"
    , 
    path_champid_isfirst = "input_csvs/user_date_time_platformaction_champid_isfirst"
    , 
    path_program_starts = "input_csvs/program_starts"
    , 
    path_programid_champid = "input_csvs/programid_champid"
    , 
    path_assessment_starts = "input_csvs/assessment_engagement_metrics_IV"
    , 
    path_assessmentid_champid = "input_csvs/assessmentid_champid"
    ,
    path_userid_cohortid = "input_csvs/userid_cohortid"
    , 
    path_cohortid_cohortname = "input_csvs/cohortid_cohortname"
    ,
    path_cohortid_champid = "input_csvs/cohortid_champid"
  ){
    
    user_date_time_platformaction_champid_isfirst <-
      path_champid_isfirst %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.table(
        header = T
        , sep = ','
        , stringsAsFactors = F
      ) %>%
      rename(
        user_id = User.Dimensions.ID
        , platform_action = User.Platform.Action.Facts.Platform.Action
        , date = Date.Dimensions.Platform.Action.Date
        , time = Date.Dimensions.Platform.Action.Time.of.Day
        , champion_id = User.Connected.to.Champion.Dimensions.ID
        , isfirst = User.Connected.to.Champion.Dimensions.Is.First.Champion..Yes...No.
      ) %>%
      mutate(time = paste(time,":00",sep="")) %>%
      mutate(datetime = chron(date, time, format = c('y-m-d','h:m:s'))) %>%
      mutate(date = as.Date(date,format="%Y-%m-%d")) %>%
      mutate(isfirst = (isfirst == "Yes"))
    
    user_platformaction_date_time <-
      path_user_platformaction_date_time %>% 
      name_as_looker_output %>% 
      grep(dir(recursive = T), value = T) %>%
      read.table(
        header = T
        , sep = ','
        , stringsAsFactors = F
      ) %>%
      rename(
        user_id = User.Dimensions.ID
        , platform_action = User.Platform.Action.Facts.Platform.Action
        , date = Date.Dimensions.Platform.Action.Date
        , time = Date.Dimensions.Platform.Action.Time.of.Day
      ) %>%
      mutate(time = paste(time,":00",sep="")) %>%
      mutate(datetime = chron(date, time, format = c('y-m-d','h:m:s'))) %>%
      mutate(date = as.Date(date,format="%Y-%m-%d")) %>%
      merge(
        user_date_time_platformaction_champid_isfirst
        , all.x = T
      ) %>%
      mutate(
        isfirst = ifelse(is.na(isfirst), F, isfirst)
        , platform_action = ifelse(isfirst, paste(platform_action, "(first)", sep = " "), platform_action)
      )
    
    cru_champions <- c(45,5,95,69,34,93,29,83,82)
    
    champid_champname <- path_to_champid_champname %>%
      name_as_looker_output %>% 
      grep(dir(recursive = T), value = T) %>%
      read.table( header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
        , quote = ""
      ) %>%
      rename(
        champion_id = User.Connected.to.Champion.Dimensions.ID
        , champion_name = User.Connected.to.Champion.Dimensions.Name
      ) %>%
      {
        rbind(.,
          data.frame(
            # X = c(0, -1) ,
            champion_id = c(0, -1)
            , champion_name = c("Ambiguous", "Ambiguous Internal")
          )  
        )
      } %>%
      mutate(
        champion_organization =
          ifelse(
            champion_id %in% cru_champions
            , "Cru"
            , champion_name
          )
      ) %>%
      mutate(
        champion_organization =
          ifelse(
            grepl("Ambiguous", champion_name)
            , "Ambiguous"
            , champion_name
          )
      )
    
    fake_champions <-  c(12,16,17,18,33,40,41,47,49,50,52,54,59,60,65,68,77,78,81,85,96)
    fake_champions_2 <- champid_champname %>%
      filter(grepl("Gloo", champion_name)) %>%
      {.$champion_id}
    
    fake_champions <- c(fake_champions,fake_champions_2) %>%
      unique %>%
      {.[. != 1]}
    
    champid_champname %<>%
      mutate(
        dont.exclude = !(champion_id %in% fake_champions)
      )
    
    user_signupdate_champid_accounttype_email <- path_user_facts  %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.table(header=T, sep=",", stringsAsFactors=F) %>%
      rename(
        user_id = User.Dimensions.ID
        , date = Date.Dimensions.Platform.Action.Date
        , champion_id = User.Connected.to.Champion.Dimensions.ID
        , account_type = User.Dimensions.Account.Type
        , user_email = User.Dimensions.Email
      ) %>%
      arrange(user_id) %>%
      mutate(date = as.Date(date,format="%Y-%m-%d"))
    
    platformaction_group_mode_enduserallowed <- path_platformaction_facts  %>%
      read.table(header=T, sep=",", stringsAsFactors=F) 
    
    # Get fake end users, merge a column into the user table that points them out.
    user_fakeenduser <-
      merge(
        user_platformaction_date_time
        , select(platformaction_group_mode_enduserallowed
            , platform_action
            , end_user_allowed
          )
      ) %>%
      group_by(user_id) %>%
      summarise(fake_end_user = (sum(!end_user_allowed) > 0))
    
    user_signupdate_champid_accounttype_email_fakeenduser <-
      user_signupdate_champid_accounttype_email %>%
      merge(user_fakeenduser)
    
    # Program & Assessment starts
    
    program_starts <- path_program_starts %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        user_id = User.Dimensions.ID
        , program_id = Program.Dimensions.ID
        , date = Date.Dimensions.Content.Progress.Date
        , time = Date.Dimensions.Content.Progress.Time
      ) %>%
      mutate(time = substr(time, 12, 20)) %>%
      mutate(datetime = chron(date, time, format = c('y-m-d','h:m:s'))) %>%
      mutate(date = as.Date(date,format="%Y-%m-%d")) 
      
    
    assessment_starts <- path_assessment_starts %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        user_id = User.Dimensions.ID
        , date = Date.Dimensions.Assessment.Date
        , assessment_id = Assessment.Dimensions.ID
        , time = Date.Dimensions.Assessment.Time.of.Day
      ) %>%
      mutate(time = paste(time,":00",sep="")) %>%
      mutate(datetime = chron(date, time, format = c('y-m-d','h:m:s'))) %>%
      mutate(date = as.Date(date,format="%Y-%m-%d")) 
    
    # Program & assessment facts
    
    programid_champid <- path_programid_champid %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        program_id = Program.Dimensions.ID
        , champion_id = Program.Champion.Dimensions.ID
      )
    
    assessmentid_champid <- path_assessmentid_champid %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        assessment_id = Assessment.Dimensions.ID
        , champion_id = Assessment.to.Champion.Dimensions.ID
      )
    
    cohortid_cohortname <- path_cohortid_cohortname %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        cohort_id = User.to.Cohort.Dimensions.ID
        , cohort_name = User.to.Cohort.Dimensions.Name
      )
    
    userid_cohortid <- path_userid_cohortid %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>%
      rename(
        user_id = User.Dimensions.ID
        , cohort_id = User.to.Cohort.Dimensions.ID
      )
    
    cohortid_champid <- path_cohortid_champid %>%
      name_as_looker_output %>%
      grep(dir(recursive = T), value = T) %>%
      read.csv(
        header=TRUE
        , sep=','
        , stringsAsFactors = FALSE
      ) %>% 
      rename(
        cohort_id = User.to.Cohort.Dimensions.ID
        , champion_id = Cohort.to.Champion.Dimensions.ID
      )
    
  return(list(
    user_platformaction_date_time
    , user_signupdate_champid_accounttype_email_fakeenduser
    , champid_champname
    , platformaction_group_mode_enduserallowed
    , program_starts
    , assessment_starts
    , programid_champid
    , assessmentid_champid
    , cohortid_cohortname
    , userid_cohortid
    , cohortid_champid
  ))
}


  