uniformative_posterior_beta = function(data, outcome_type) {
  data %>%
    filter(outcome == outcome_type) %>%
    # To generate posterior probabilities:

    # prior beta(alpha1, beta1); likelihood beta(alpha2, beta2)
    # In this case, alpha1 and beta1 = 1, because I'm using a
    # non-informative prior: beta(1,1), i.e. an uniform distribution

    # Formula underlying a beta-distributed posterior probability:
    # beta(alpha1 + alpha2, beta1 + beta2)

    # In this case,
    # alpha2 = placebo or TA_death / beta2 = placebo or TA_diff

    # Thus, posterior probability formulas:
    # beta(placebo_death + 1, placebo_diff + 1) / beta(TA_death + 1, TA_diff + 1)

    summarise(
      control = list(rbeta(n, sum_events_control + 1, sum_diff_control + 1)),
      toci = list(rbeta(n, sum_events_toci + 1, sum_diff_toci + 1))
    ) %>%
    unnest(control:toci)
}