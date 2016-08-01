# Function: save or print

save_or_print <- 
  function(
    p
    , save_plots = T
    , outloc = getwd()
    , plot_name = 
        paste("myplot", gsub(" ", "_",Sys.time()), sep = "_")
    , outformat = "html"
    , v.width = 992
    , v.height = 744
  ){
    current_wd <- getwd()
    if(save_plots){
      setwd(outloc)
      if(outformat == "html"){
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        ) 
      } else if(outformat == "png"){
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        )
        webshot(
          paste(plot_name, ".html", sep = "")
          , file = paste(plot_name, ".png", sep = "")
          , vwidth = v.width
          , vheight = v.height
        )
      } else if(outformat == "pdf"){
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        )
        webshot(
          paste(plot_name, ".html", sep = "")
          , file = paste(plot_name, ".pdf", sep = "")
          , vwidth = v.width
          , vheight = v.height
        )
      } else if(outformat == "jpg"){
        saveWidget(as.widget(p)
                   , paste(plot_name, ".html", sep = "")
        )
        webshot(
          paste(plot_name, ".html", sep = "")
          , file = paste(plot_name, ".jpg", sep = "")
          , vwidth = v.width
          , vheight = v.height
        )
      }
      setwd(current_wd)
    } else{print(p)}
  }