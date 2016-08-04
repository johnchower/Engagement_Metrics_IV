# Function: chisquare_plot

# Takes the result of a chisquare_analysis call and plots the bar charts that
# compare the expected to the actual counts in each category.

produce_chisquare_plot_data <- 
  function(
    results # Take this from a chisquare analysis. You'll have to apply your own filters before calling the function
    , slice_variable = colnames(results)[1] # Produce one graph for each unique value of slice_variable
    ){
    
    plotdata <- results %>%
      dlply(
        .variables = slice_variable
        , .fun = function(df){
          # dots <- c(colnames(df)[2], "observed", "expected", "err", "pvalue")
          
          df %>%
            arrange(desc(marginal2)) %>%
            {
              colnames(.)[3] <- "observed"
              return(.)
            } %>%
            # select_(.dots = dots) %>%
            mutate(
              total_actions = sum(observed)
              , percent_observed = observed/sum(observed)
              , percent_expected = expected/sum(expected)
              , err = err/total_actions
            ) %>%
            return
        }
      ) 
    
  
}