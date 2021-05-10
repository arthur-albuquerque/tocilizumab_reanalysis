pooled_tibble_all_posteriors = function(
  recovery_rma, # Output from rma() with only RECOVER&
  multiple_priors_object # Output from normal_approximation_multiple_priors()
) {
  set.seed = 123 # set seed for reproducibility (rnorm())
  n = 10e4 # sampling size
  
  tibble(
    
    # This is an output from rbeta() in noninformative_posterior()
    "Non-informative" = rnorm(n,
            mean = recovery_rma$beta,
            sd = recovery_rma$se
      ),
    
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
