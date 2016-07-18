# Function: line_chart_layout

line_chart_layout <-
  function(
    p
    , charttitle = ""
    , yaxisformat = "n"
  ){
    p %>%
    plotly::layout(
      title = charttitle
      , font = 
        list(
          size = 20
        )
      , xaxis = 
        list(
          title = ""
        )
      , yaxis = 
        list(
          title = ""
          , tickformat = yaxisformat
        )
      , margin = 
        list(
          r = 100
          , t = 100
        )
    )
  }