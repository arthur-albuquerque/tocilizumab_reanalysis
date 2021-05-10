seed = 123

posterior_difference_plot = function(data, # Data object
                                      ## start using quotes
                                      color_code, # Fill color code
                                      xlabel, # X axis label
                                      ### stop using quotes
                                      xlim_inf, # X axis inferior limit
                                      xlim_sup, # X axis superior limit
                                      ticks_spacing) # Spacing between ticks in X axis
  
{
  data %>%
    summarise(
      posterior = (list(rnorm(10e4,
        mean = post.mean,
        sd = post.sd
      ))
      )
    ) %>%
    unnest(posterior) %>%
    mutate(posterior = 100 * posterior) %>%
    # Plot!
    ggplot(aes(posterior)) +

    # https://mjskay.github.io/ggdist/articles/slabinterval.html
    stat_halfeye(
      .width = 0.95,
      fill = color_code
    ) +
    labs(x = xlabel, y = "Density") +
    scale_x_continuous(breaks = seq(from = xlim_inf, to = xlim_sup, ticks_spacing)) +
    scale_y_discrete(
      breaks = NULL,
      expand = c(0, .03)
    ) +
    theme(
      axis.ticks.x = element_blank(),
      # axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      legend.position = "none"
    ) +
    coord_cartesian(xlim = c(xlim_inf, xlim_sup))
}