risk_comparison_plot = function(data, # Data object
                                 ### start using quotes
                                 control_color, # Color code for control group
                                 intervention_color, # Color code for other group
                                 xlabel, # X axis label
                                 title_label, # Title
                                 subtitle_label, # Subtitle
                                 ### stop using quotes
                                 xlim_inf, # X axis inferior limit
                                 xlim_sup, # X axis superior limit
                                 ticks_spacing) # X axis spacing for ticks
  {
  data %>%
    # make it tidy for ggplot
    pivot_longer(
      cols = c(control, toci),
      names_to = "group", values_to = "samples"
    ) %>%
    ggplot(aes(x = 100 * samples, y = fct_rev(group))) +

    # https://mjskay.github.io/ggdist/articles/slabinterval.html
    stat_halfeye(
      .width = 0.95,
      aes(
        fill = group,
        fill_ramp = stat(cut_cdf_qi(
          cdf,
          .width = c(.5, 1),
          labels = scales::percent_format()
        ))
      ),
      position = "dodge",
    ) +
    # a range from 1 down to 0.2 ensures the fill goes dark to light inside-out
    # and doesn't get all the way down to white (0) on the lightest color
    scale_fill_ramp_discrete(range = c(1, 0.4), na.translate = FALSE) +
    scale_fill_manual(values = c(control_color, intervention_color)) +
    labs(
      x = xlabel, y = "",
      title = title_label,
      subtitle = subtitle_label
    ) +
    scale_x_continuous(breaks = seq(
      from = xlim_inf, to = xlim_sup, ticks_spacing
    )) +
    scale_y_discrete(
      labels = c(
        "control" = "Control ",
        "toci" = "Tocilizumab "
      ),
      expand = c(0, 0.3)
    ) +
    theme(
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = "gray80", size = 0.3),
      plot.title.position = "plot",
      legend.position = "none",
      plot.margin = margin(20, 25, 15, 20)
    )
}