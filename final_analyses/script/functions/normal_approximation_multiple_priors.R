### To generate normal distributed posterior probabilities from multiple priors
### using normal approximation from discrete outcomes

normal_approximation_multiple_priors =
  function(tibble_data_prior_post, # Data object with evidence-based posterior from normal_approximation()
           outcome_type, # Outcome (within quotes)
           data_prior_posterior # Name for the final output with data+prior+posterior
  ) {
    data2 = tibble(
      data.mean = (tibble_data_prior_post %>%
        pull(data.mean)), # Same data mean for every row

      data.var = (tibble_data_prior_post %>%
        pull(data.sd))^2, # Same data variance for every row

      prior.mean = c(
        tibble_data_prior_post %>%
          pull(prior.mean), # Evidence-based

        0, # Skeptical

        # Negative values = benefit regarding mortality
        # Positive values = benefit regarding hospital discharge

        ifelse(outcome_type == "mortality", -0.05, 0.05), # Optimistic

        ifelse(outcome_type == "mortality", 0.05, -0.05)
      ), # Pessimistic

      prior.var = (tibble_data_prior_post %>%
        pull(prior.sd))^2
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
