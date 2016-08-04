# function: plot_triangle_progress

plot_triangle_progress <-
  function(
    mpdbd = mode_pct_data_by_date
    , user_subset = unique(mpdbd$user_id)
    , subsetname = "All Users"
    , days_since_signup = c(1, 7, 28)
    , save_plots = T
    , ...
  ){
    out <- list()
    for(i in days_since_signup){
      mpdbd %>%
        prepare_modepctdata_for_createconvexdata(user_subset, number_of_days = i, maxdate = Sys.Date()) %>%
        create_convex_data_v2 %>%
        plot_triangle_diagram_v2 %>%
        {
          title <- 
            paste("Day", i, sep = " ")
          return(. + ggtitle(paste(subsetname, "-", title, sep = " ")))
        } %>% 
        {
          if(save_plots){ggsave(plot = ., ...)} else {print(.)}
        }
    }
  }