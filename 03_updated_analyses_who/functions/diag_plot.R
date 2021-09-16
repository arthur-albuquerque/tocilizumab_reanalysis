# https://benyandrew.netlify.app/blog/bayesian_rct/#

pacman::p_load(cowplot, bayesplot)

base_theme <-
  theme_classic()


diag_plot <- function(model, pars_list, ncol_trace){
  
  color_scheme_set("purple")
  plt_1 <- 
    mcmc_trace(as.array(model), 
               pars = pars_list,
               facet_args = list(ncol = ncol_trace, 
                                 strip.position = "left")) + 
    base_theme + 
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_blank(),
      legend.position = "none"
    )
  
  
  plt_2 <- 
    neff_ratio(model, 
               pars = pars_list) %>%
    mcmc_neff() +
    base_theme + 
    theme(
      legend.position = "none"
    )
  
  
  plt_3 <-
    rhat(model, pars = pars_list) %>%
    mcmc_rhat() + 
    base_theme + 
    theme(
      legend.position = "none"
    )
  
  grid <-
    plot_grid(plt_2, plt_3, ncol = 2, align = "hv", axis = "b",
              rel_widths = c(1, 0.5))
  
  plot_grid(plt_1, grid, ncol = 1, align = "hv", axis = "l",
            rel_heights = c(0.5, 1))
  
}
