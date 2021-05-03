summary_table_discharge = function(
  posteriors_table # Tibble with the full posterior distributions
){
  
  tibble(
    
    "Prior" = c(
      "Non-informative",
      "Evidence-based",
      "Skeptical",
      "Optimistic",
      "Pessimistic"
    ),
    
    
    "Mean RD" = c(
      posteriors_table %>%
        pull("Non-informative") %>%
        mean() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Evidence-based") %>%
        mean() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Skeptical") %>%
        mean() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Optimistic") %>%
        mean() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Pessimistic") %>%
        mean() %>%
        round(.,3)
      
    ),
    
    
    "SD" = c(
      posteriors_table %>%
        pull("Non-informative") %>%
        sd() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Evidence-based") %>%
        sd() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Skeptical") %>%
        sd() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Optimistic") %>%
        sd() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Pessimistic") %>%
        sd() %>%
        round(.,3)
      
    ),
    
    "Pr(< -5%)" = c(
      round(pnorm(-0.05,
                  posteriors_table %>%
                    pull("Non-informative") %>% mean(),
                  posteriors_table %>%
                    pull("Non-informative") %>% sd()), 2) * 100,
      round(pnorm(-0.05,
                  posteriors_table %>%
                    pull("Evidence-based") %>% mean(),
                  posteriors_table %>%
                    pull("Evidence-based") %>% sd()), 2) * 100,
      round(pnorm(-0.05,
                  posteriors_table %>%
                    pull("Skeptical") %>% mean(),
                  posteriors_table %>%
                    pull("Skeptical") %>% sd()), 2) * 100,
      round(pnorm(-0.05,
                  posteriors_table %>%
                    pull("Optimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Optimistic") %>% sd()), 2) * 100,
      round(pnorm(-0.05,
                  posteriors_table %>%
                    pull("Pessimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Pessimistic") %>% sd()), 2) * 100
      
      
    ),
    
    "Pr(< -2%)" = c(
      round(pnorm(-0.02,
                  posteriors_table %>%
                    pull("Non-informative") %>% mean(),
                  posteriors_table %>%
                    pull("Non-informative") %>% sd()), 2) * 100,
      round(pnorm(-0.02,
                  posteriors_table %>%
                    pull("Evidence-based") %>% mean(),
                  posteriors_table %>%
                    pull("Evidence-based") %>% sd()), 2) * 100,
      round(pnorm(-0.02,
                  posteriors_table %>%
                    pull("Skeptical") %>% mean(),
                  posteriors_table %>%
                    pull("Skeptical") %>% sd()), 2) * 100,
      round(pnorm(-0.02,
                  posteriors_table %>%
                    pull("Optimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Optimistic") %>% sd()), 2) * 100,
      round(pnorm(-0.02,
                  posteriors_table %>%
                    pull("Pessimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Pessimistic") %>% sd()), 2) * 100
      
    ),
    
    "Pr(< -1%)" = c(
      round(pnorm(-0.01,
                  posteriors_table %>%
                    pull("Non-informative") %>% mean(),
                  posteriors_table %>%
                    pull("Non-informative") %>% sd()), 2) * 100,
      round(pnorm(-0.01,
                  posteriors_table %>%
                    pull("Evidence-based") %>% mean(),
                  posteriors_table %>%
                    pull("Evidence-based") %>% sd()), 2) * 100,
      round(pnorm(-0.01,
                  posteriors_table %>%
                    pull("Skeptical") %>% mean(),
                  posteriors_table %>%
                    pull("Skeptical") %>% sd()), 2) * 100,
      round(pnorm(-0.01,
                  posteriors_table %>%
                    pull("Optimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Optimistic") %>% sd()), 2) * 100,
      round(pnorm(-0.01,
                  posteriors_table %>%
                    pull("Pessimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Pessimistic") %>% sd()), 2) * 100
    ),
    
    
    "Pr(> 0%)" = c(
      round((1 - pnorm(0,
                  posteriors_table %>%
                    pull("Non-informative") %>% mean(),
                  posteriors_table %>%
                    pull("Non-informative") %>% sd())), 2) * 100,
      round((1- pnorm(0,
                  posteriors_table %>%
                    pull("Evidence-based") %>% mean(),
                  posteriors_table %>%
                    pull("Evidence-based") %>% sd())), 2) * 100,
      round((1 - pnorm(0,
                  posteriors_table %>%
                    pull("Skeptical") %>% mean(),
                  posteriors_table %>%
                    pull("Skeptical") %>% sd())), 2) * 100,
      round((1 - pnorm(0,
                  posteriors_table %>%
                    pull("Optimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Optimistic") %>% sd())), 2) * 100,
      round((1 - pnorm(0,
                  posteriors_table %>%
                    pull("Pessimistic") %>% mean(),
                  posteriors_table %>%
                    pull("Pessimistic") %>% sd())), 2) * 100
      
      
    ),
    
    "Pr(> 1%)" = c(
      round((1 - pnorm(0.01,
                       posteriors_table %>%
                         pull("Non-informative") %>% mean(),
                       posteriors_table %>%
                         pull("Non-informative") %>% sd())), 2) * 100,
      round((1 - pnorm(0.01,
                       posteriors_table %>%
                         pull("Evidence-based") %>% mean(),
                       posteriors_table %>%
                         pull("Evidence-based") %>% sd())), 2) * 100,
      round((1 - pnorm(0.01,
                       posteriors_table %>%
                         pull("Skeptical") %>% mean(),
                       posteriors_table %>%
                         pull("Skeptical") %>% sd())), 2) * 100,
      round((1 - pnorm(0.01,
                       posteriors_table %>%
                         pull("Optimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Optimistic") %>% sd())), 2) * 100,
      round((1 - pnorm(0.01,
                       posteriors_table %>%
                         pull("Pessimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Pessimistic") %>% sd())), 2) * 100
    ),
    
    "Pr(> 2%)" = c(
      round((1 - pnorm(0.02,
                       posteriors_table %>%
                         pull("Non-informative") %>% mean(),
                       posteriors_table %>%
                         pull("Non-informative") %>% sd())), 2) * 100,
      round((1 - pnorm(0.02,
                       posteriors_table %>%
                         pull("Evidence-based") %>% mean(),
                       posteriors_table %>%
                         pull("Evidence-based") %>% sd())), 2) * 100,
      round((1 - pnorm(0.02,
                       posteriors_table %>%
                         pull("Skeptical") %>% mean(),
                       posteriors_table %>%
                         pull("Skeptical") %>% sd())), 2) * 100,
      round((1 - pnorm(0.02,
                       posteriors_table %>%
                         pull("Optimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Optimistic") %>% sd())), 2) * 100,
      round((1 - pnorm(0.02,
                       posteriors_table %>%
                         pull("Pessimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Pessimistic") %>% sd())), 2) * 100
    ),
    
    "Pr(> 5%)" = c(
      round((1 - pnorm(0.05,
                       posteriors_table %>%
                         pull("Non-informative") %>% mean(),
                       posteriors_table %>%
                         pull("Non-informative") %>% sd())), 2) * 100,
      round((1 - pnorm(0.05,
                       posteriors_table %>%
                         pull("Evidence-based") %>% mean(),
                       posteriors_table %>%
                         pull("Evidence-based") %>% sd())), 2) * 100,
      round((1 - pnorm(0.05,
                       posteriors_table %>%
                         pull("Skeptical") %>% mean(),
                       posteriors_table %>%
                         pull("Skeptical") %>% sd())), 2) * 100,
      round((1 - pnorm(0.05,
                       posteriors_table %>%
                         pull("Optimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Optimistic") %>% sd())), 2) * 100,
      round((1 - pnorm(0.05,
                       posteriors_table %>%
                         pull("Pessimistic") %>% mean(),
                       posteriors_table %>%
                         pull("Pessimistic") %>% sd())), 2) * 100
    )
  )
  
}