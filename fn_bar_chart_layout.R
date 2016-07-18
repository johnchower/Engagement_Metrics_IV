# function: bar chart layout

bar_chart_layout <-
  function(
    p
    , charttitle = ""
    , yaxisformat = NULL
    , bottommargin = 100
    , leftmargin = 100
    , xaxisrange = NULL
    , yaxisrange = NULL
    , bar_mode = NULL
    , yaxistitle = ""
    , xaxistitle = ""
    , chartwidth = ""
  ){
    layout(
      title = charttitle
      , width = chartwidth
      , barmode = bar_mode
      , font = 
        list(
          size = 20
          , color = "black"
        )
      , margin = 
        list(
          r = 200
          , t = 100
          , b = bottommargin
          , pad = 20
          , l = leftmargin
        )
      , yaxis = 
        list(
          title = yaxistitle
          , linewidth = 0
          , tickformat = yaxisformat
          , range = yaxisrange
        )
      , xaxis = 
        list(
          title = xaxistitle
          , linewidth = 0
          , range = xaxisrange
        )
    )
  }