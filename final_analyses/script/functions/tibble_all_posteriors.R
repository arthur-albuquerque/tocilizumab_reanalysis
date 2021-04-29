tibble_all_posteriors = function(
  noninformative_object, # Output from noninformative_posterior()
  multiple_priors_object # Output from normal_approximation_multiple_priors()
) {
  set.seed = 123 # set seed for reproducibility (rnorm())
  n = 10e4 # sampling size

  tibble(

    # This is an output from rbeta() in noninformative_posterior()
    "Non-informative" = noninformative_object %>%
      summarise(delta = (toci - control)) %>% pull(),
    
    "Evidence-based" = multiple_priors_object %>%
      filter(type == "evidence-based") %>%
      summarise(a = rnorm(n,
        mean = post.mean,
        sd = post.sd
      )) %>% pull(),
    
    "Skeptical" = multiple_priors_object %>%
      filter(type == "skeptical") %>%
      summarise(a = rnorm(n,
        mean = post.mean,
        sd = post.sd
      )) %>% pull(),
    
    "Optimistic" = multiple_priors_object %>%
      filter(type == "optimistic") %>%
      summarise(a = rnorm(n,
        mean = post.mean,
        sd = post.sd
      )) %>% pull(),
    
    "Pessimistic" = multiple_priors_object %>%
      filter(type == "pessimistic") %>%
      summarise(a = rnorm(n,
        mean = post.mean,
        sd = post.sd
      )) %>% pull(),
  )
}