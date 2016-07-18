# Function: save or print

save_or_print <- 
  function(
    p
    , save_plots = T
    , outloc = getwd()
    , plot_name = 
        paste("myplot", gsub(" ", "_",Sys.time()), sep = "_")
    , outformat = "html"
  ){
    current_wd <- getwd()
    if(save_plots){
      setwd(outloc)
      if(outformat == "html"){
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        ) 
      } else{
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        )
        webshot(
          paste(plot_name, ".html", sep = "")
          , file = paste(plot_name, ".png", sep = "")
        )
      }
      setwd(current_wd)
    } else{print(p)}
  }