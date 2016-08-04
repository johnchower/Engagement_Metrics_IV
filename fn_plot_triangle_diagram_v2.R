# Function: plot triangle diagram v2

# A simplified version of plot_triangle_diagram

plot_triangle_diagram_v2 <- 
  function(
    cd = convex_data
  ){
    out <- cd %>%
      mutate(actions_per_day = actions_per_day + .1) %>%
      ggplot(
        aes(y = yval, x = xval)
      ) +
      geom_point(
        aes(
          colour = mode
          , size = actions_per_day 
        )
        , alpha = 1/4
      ) + 
      scale_colour_manual(values = c("red","blue","green"), drop = F) +
      geom_segment(x = 0, y = 0, xend = 1, yend = 0) +
      geom_segment(x = 0, y = 0, xend = 0.5, yend = sqrt(3)/2) +
      geom_segment(x = 0.5, y = sqrt(3)/2, xend = 1, yend = 0) +
      geom_segment(x = 0.5, y = 0.5*tan(pi/6), xend = 0.5, yend = 0) +
      geom_segment(x = 0.5, y = 0.5*tan(pi/6), xend = 0.25, yend = sqrt(3)/4) +
      geom_segment(x = 0.5, y = 0.5*tan(pi/6), xend = 0.75, yend =  sqrt(3)/4) +
      xlim(-.125,1.15) +
      ylim(-.125,1) +
      annotate("text", label = "Receive Value", x = 0.5, y = sqrt(3)/2 + .1, size = 10) +
      annotate("text", label = "Champion Others", x = .1, y = -.1, size = 10) +
      annotate("text", label = "Invest for Self/Us", x = .9, y = -.1, size = 10) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank()
        , panel.grid.minor = element_blank()
        , legend.text = element_text(size = 20)
        , legend.title = element_text(size = 20)
        , plot.title = element_text(size = 16, face = "bold")
        , axis.ticks = element_blank()
        , axis.text = element_blank()
      ) +
      xlab("") +
      ylab("") +
      scale_size_continuous(limits = c(0,50), breaks = c(1,3,6,10)) +
      ggtitle("") 
    
    return(out)
  }