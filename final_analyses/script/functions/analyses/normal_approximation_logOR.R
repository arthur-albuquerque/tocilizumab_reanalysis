### To generate a normal distributed posterior probability using
#   normal approximation from discrete outcomes in log-odds ratio

normal_approximation_logOR =
  function(recovery_rma, # Data object with RECOVERY data
           priors_rma, # Object output from the rma() in the previous code chunk
           
           data_prior_posterior # Name for the final output with data+prior+posterior
  ) {
    
    ## Here is to put the normal approximate data (likelihood) with
    #  the prior (from rma()) together
    data2 = tibble(
      data.mean = recovery_rma$beta,
      data.var = (recovery_rma$se)^2,
      prior.mean = priors_rma$beta,
      prior.var = (priors_rma$se)^2
    )
    
    ## Here is to apply the function post.normal.mean() in the tibble above. 
    #  This function uses both mean and variance from the data and prior
    #  to generate a normal distributed posterior distribution
    #  The post.normal.mean() can be found at the following path:
    #  here("final_analyses", "script", "functions", "conjugate_normal_posterior.R")
    posterior = data2 %>%
      nest(data = c(data.mean:prior.var)) %>%
      # Apply post.normal.mean() with pmap()
      mutate(posterior = pmap(data2, post.normal.mean)) %>%
      unnest(posterior) %>%
      select("post.mean", "sqrt(post.var)") %>%
      rename("post.sd" = "sqrt(post.var)")
    
    # Create a single tibble with mean and SD of data, prior, and posterior prob
    # This "data_prior_posterior" will be the final output from this script
    data_prior_posterior = data2 %>%
      summarise(
        data.mean = data.mean,
        data.sd = sqrt(data.var),
        prior.mean = prior.mean,
        prior.sd = sqrt(prior.var)
      ) %>%
      bind_cols(posterior)
  }
