### To generate normal distributed posterior probabilities from multiple priors
### using normal approximation from discrete outcomes

logOR_normal_approximation_multiple_priors =
  function(
    # Data object with evidence-based posterior from normal_approximation_logOR()
    tibble_data_prior_post, 
           outcome_type, # Outcome (within quotes)
           data_prior_posterior # Name for the final output with data+prior+posterior
  ) {
    data2 = tibble(
      data.mean = (tibble_data_prior_post %>%
                     pull(data.mean)), # Same data mean for every row
      
      data.var = (tibble_data_prior_post %>%
                    pull(data.sd))^2, # Same data variance for every row
      
      prior.mean = c(
        
        0, # Non-informative
        
        tibble_data_prior_post %>%
          pull(prior.mean), # Evidence-based
        
        0, # Skeptical
        
        # Negative values = benefit regarding mortality
        # Positive values = benefit regarding hospital discharge
        
        # Optimistic prior expects 20% reduction of odds
        # For the mortality outcome, this translates to mean = 0.8
        # For the hospital discharge, greater odds ratio means benefits,
        # thus the mean is 1/0.8 = 1.25
        
        # Since we will use these values to generate log-odds ratio
        # the mean will be actually log(0.80) or log(1.25)
        
        ifelse(outcome_type == "mortality", log(0.80), log(1.25)), # Optimistic
        
        ifelse(outcome_type == "mortality", log(1.25), log(0.80))
      ), # Pessimistic prior is the opposite of optimistic
      
      prior.var = c(10000, # Non-informative 
                    
                    rep(
                      (tibble_data_prior_post %>%
                     pull(prior.sd))^2,
                     4) # repeat 4 times for the evidence-based, skeptical,
                        # optimistic and pessimistic priors
                    )
    )
    
    ## Here is to apply the function post.normal.mean() in the tibble above.
    #  This function uses both mean and variance from the data and prior
    #  to generate a normal distributed posterior distribution
    #  The post.normal.mean() can be found at the following path:
    #  here("final_analyses", "script", "functions", "conjugate_normal_posterior.R")
    
    data_prior_posterior = data2 %>%
      # Applying post.normal.mean() with pmap()
      mutate(posterior = pmap(., post.normal.mean)) %>%
      unnest(posterior) %>%
      # To add a column with the name of each prior
      mutate(
        type = c(
          "non-informative",
          "evidence-based",
          "skeptical",
          "optimistic",
          "pessimistic"
        ),
        data.sd = sqrt(data.var),
        prior.sd = sqrt(prior.var)
      ) %>%
      rename("post.sd" = "sqrt(post.var)") %>%
      # To change column order and to remove variance columns
      select(
        type,
        data.mean, data.sd,
        prior.mean, prior.sd,
        post.mean, post.sd
      )
  }
