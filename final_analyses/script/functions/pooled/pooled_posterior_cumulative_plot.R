pooled_posterior_cumulative_plot = function(
  
  data, # Data object
  xlabel, # X axis label (within quotes)
  xlim_inf, # X axis inferior limit
  xlim_sup, # X axis superior limit
  ticks_spacing, # Spacing between ticks in X axis
  outcome # Outcome (within quotes)
  
) {
  data %>%
    summarise(
      posterior = (list(rnorm(n,
                              mean = post.mean,
                              sd = post.sd
      ))
      )
    ) %>%
    unnest(posterior) %>%
    
    # Use exp() to transform log-odds ratio into Odds Ratio
    mutate(posterior = exp(posterior)) %>%
    
    ggplot(aes(posterior)) +
    
    # To invert or not the y axis
    (
      if (outcome == "mortality") {
        # from 0 to 1
        stat_ecdf(geom = "step")
      } else {
        # from 1 to 0
        geom_line(aes(y = 1 - ..y..), stat='ecdf')
      }
    ) +
    
    labs(
      x = xlabel,
      y = 
        # To put lower/greater or equal unicode
        (
          if (outcome == "mortality") {
            # lower or equal
            "Probability OR \u2264 X"
          } else {
            # greater or equal
            "Probability OR \u2265 X"
          }
        )
    ) +
    scale_x_continuous(
      breaks = seq(from = xlim_inf, to = xlim_sup, ticks_spacing),
      limits = c(xlim_inf, xlim_sup)
    ) +
    scale_y_continuous(
      breaks = seq(from = 0, to = 1, .1),
      expand = c(0, .03)
    ) +
    theme(
      axis.ticks.x = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      panel.grid.major.y = element_line(color = "gray80", size = 0.3),
      plot.title.position = "plot",
      legend.position = "none",
    )
}
