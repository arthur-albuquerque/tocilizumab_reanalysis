### To generate a normal distributed posterior probability using
#   normal approximation from discrete outcomes

normal_approximation =
  function(data_recovery, # Data object with RECOVERY data
           outcome_type, # Outcome (within quotes)
           priors_rma, # Object output from the rma() in the previous code chunk
           
           data_prior_posterior # Name for the final output with data+prior+posterior
  ) {
    
    ## Normal approximate the data (likelihood)
    data1 =
      data_recovery %>%
      filter(outcome == outcome_type) %>%
      summarise(
        # Standard deviation
        # sqrt(var); var = a*b/((a+b)^2 * (a + b + 1))

        # https://modelassist.epixanalytics.com/display/EA/Normal+approximation+to+the+Beta+distribution
        # https://en.wikipedia.org/wiki/Beta_distribution
        sd_control = sqrt(
          (sum_events_control * sum_diff_control) / (
            ((sum_events_control + sum_diff_control)^2) *
              (sum_events_control + sum_diff_control + 1)
          )
        ),
        sd_toci = sqrt(
          (sum_events_toci * sum_diff_toci) / (
            ((sum_events_toci + sum_diff_toci)^2) *
              (sum_events_toci + sum_diff_toci + 1)
          )
        ),
        sd_delta = sqrt(sd_control^2 + sd_toci^2),

        # Mean

        mean_delta = (sum_events_toci / sum_total_toci) - (sum_events_control / sum_total_control),
      )

    ## Here is to put the normal approximate data (likelihood) with
    #  the prior (from rma()) together
    data2 = tibble(
      data.mean = data1 %>% pull(mean_delta),
      data.var = (data1 %>% pull(sd_delta))^2,
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
      as.tibble() %>%
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
