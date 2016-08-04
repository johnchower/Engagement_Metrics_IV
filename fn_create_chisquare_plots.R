# Function: create_chisquare_plots

# Generates the chisquare plots from data created by fn_produce_chisquare_plot_data.r

create_chisquare_plots <- function(
  plot.data
  , thing_being_counted = "Platform Actions" # What quantity are the bars measuring?
  , thing_being_sliced = "Users" # What does each graph's subset consist of?
  , err_bar_width = 5
  , ... # Pass the bar chart layout arguments 
){
  plot.data %>%
    llply(
      .fun = function(plot_data){
        out_plot <-  
          plot_ly(
            plot_data
            , x = plot_data[,2]
            , y = percent_expected
            , text = 
              paste("(", prettyNum(round(expected), big.mark = ","), " ", thing_being_counted, " expected)", sep = "")
            , type = "bar"
            , name = paste("Expected Percent of", thing_being_counted, sep = " ")
            , error_y = list(array = err, width = err_bar_width)
          ) %>%
            add_trace(
              x = plot_data[,2]
              , y = plot_data$percent_observed
              , text = 
                paste("(", prettyNum(round(plot_data$observed), big.mark = ","), " ", thing_being_counted, " observed)", sep = "")
              , type = "bar"
              , name = paste("Observed Percent of", thing_being_counted, sep = " ")
            ) %>%
            bar_chart_layout(
              charttitle = 
                paste(
                  "Platform Action Distribution for '"
                  , plot_data[1,1]
                  , paste("'", thing_being_sliced, "(", sep = " ")
                  , prettyNum(plot_data$total_actions[1], big.mark = ",")
                  , " Total Actions)"
                  , sep = ""
                )
              , ...
            )
      }
    )
}