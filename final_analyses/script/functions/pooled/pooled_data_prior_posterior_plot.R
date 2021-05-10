# To plot distributions of the prior, data and posterior side-by-side

set.seed = 123 # set seed for reproducibility (rnorm() function)
n = 10e4 # sampling size

pooled_data_prior_posterior_plot =
  function(data_object, # Output from the normal_approximation()
           # start using quotes
           prior_color, # Color for the prior
           data_color, # Color for RECOVERY
           posterior_color, # Color for the posterior
           xlabel, # X axis label
           title_text, # Title
           subtitle_text, # Subtitle
           # stop using quotes
           xlim_inf, # X axis inferior limit
           xlim_sup, # X axis superior limit
           ticks_spacing # Spacing between ticks in X axis
  ) {
    
    # Generate random samples using rnorm()
    distributions = 
      data_object %>%
      summarise(
        RECOVERY = list(rnorm(n,
                              mean = data.mean,
                              sd = data.sd
        )),
        Prior = list(rnorm(n,
                           mean = prior.mean,
                           sd = prior.sd
        )),
        Posterior = list(rnorm(n,
                               mean = post.mean,
                               sd = post.sd
        ))
      ) %>%
      unnest(RECOVERY:Posterior)
    
    # https://stackoverflow.com/questions/12774210/
    # how-do-you-specifically-order-ggplot2-x-axis-instead-of-alphabetical-order
    
    level_order = c("Posterior", "RECOVERY", "Prior")
    
    # Plot!
    
    distributions %>%
      
      # make it tidy for ggplot
      pivot_longer(
        cols = c(RECOVERY:Posterior),
        names_to = "dist", values_to = "samples"
      ) %>%
      
      # Use exp() to transform log-odds ratio into Odds Ratio
      
      ggplot(aes(x = samples, y = factor(dist, level = level_order))) +
      
      # https://mjskay.github.io/ggdist/articles/slabinterval.html
      stat_halfeye(
        .width = 0.95,
        aes(
          fill = dist,
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
      scale_fill_manual(values = c(
        posterior_color,
        prior_color,
        data_color
      )) +
      labs(
        x = xlabel, y = "",
        title = title_text,
        subtitle = subtitle_text
      ) +
      scale_x_continuous(breaks = seq(from = xlim_inf, to = xlim_sup, ticks_spacing)) +
      scale_y_discrete(expand = c(0, 0.1)) +
      theme(
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = "gray80", size = 0.3),
        plot.title.position = "plot",
        legend.position = "none",
        plot.margin = margin(20, 25, 15, 20)
      ) +
      coord_cartesian(xlim = c(xlim_inf, xlim_sup))
  }
