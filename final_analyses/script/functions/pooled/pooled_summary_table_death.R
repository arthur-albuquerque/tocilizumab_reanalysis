pooled_summary_table_death = function(
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
    
    # exp() to transform log-odds ratio into Odds Ratio
    
    "Median OR" = c(
      posteriors_table %>%
        pull("Non-informative") %>%
        exp() %>% 
        median() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Evidence-based") %>%
        exp() %>% 
        median() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Skeptical") %>%
        exp() %>% 
        median() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Optimistic") %>%
        exp() %>% 
        median() %>%
        round(.,3),
      
      posteriors_table %>%
        pull("Pessimistic") %>%
        exp() %>% 
        median() %>%
        round(.,3)
      
    ),
    
    "Pr(< 0.8)" = c(posteriors_table %>%
                       select("Non-informative") %>% 
                       summarise(percentage = sum(. < log(0.8)) / n() ) %>% 
                       pull() %>%
                      round(.,2),
                    
                     posteriors_table %>%
                       select("Evidence-based") %>% 
                       summarise(percentage = sum(. < log(0.8)) / n() ) %>% 
                       pull() %>%
                      round(.,2),
                    
                     posteriors_table %>%
                       select("Skeptical") %>% 
                       summarise(percentage = sum(. < log(0.8)) / n() ) %>% 
                       pull() %>%
                      round(.,2),
                    
                     posteriors_table %>%
                       select("Optimistic") %>% 
                       summarise(percentage = sum(. < log(0.8)) / n() ) %>% 
                       pull() %>%
                      round(.,2),
                    
                     posteriors_table %>%
                       select("Pessimistic") %>% 
                       summarise(percentage = sum(. < log(0.8)) / n() ) %>% 
                       pull() %>%
                      round(.,2)
    ),
    
    "Pr(< 0.9)" = c(posteriors_table %>%
                      select("Non-informative") %>% 
                      summarise(percentage = sum(. < log(0.9)) / n() ) %>% 
                      pull() %>%
                      round(.,2),
                    
                    posteriors_table %>%
                      select("Evidence-based") %>% 
                      summarise(percentage = sum(. < log(0.9)) / n() ) %>% 
                      pull() %>%
                      round(.,2),
                    
                    posteriors_table %>%
                      select("Skeptical") %>% 
                      summarise(percentage = sum(. < log(0.9)) / n() ) %>% 
                      pull() %>%
                      round(.,2),
                    
                    posteriors_table %>%
                      select("Optimistic") %>% 
                      summarise(percentage = sum(. < log(0.9)) / n() ) %>% 
                      pull() %>%
                      round(.,2),
                    
                    posteriors_table %>%
                      select("Pessimistic") %>% 
                      summarise(percentage = sum(. < log(0.9)) / n() ) %>% 
                      pull() %>%
                      round(.,2)
    ),
    
    
    "Pr(< 1)" = c(posteriors_table %>%
                     select("Non-informative") %>% 
                     summarise(percentage = sum(. < log(1)) / n() ) %>% 
                     pull() %>%
                    round(.,2),
                  
                   posteriors_table %>%
                     select("Evidence-based") %>% 
                     summarise(percentage = sum(. < log(1)) / n() ) %>% 
                     pull() %>%
                    round(.,2),
                  
                   posteriors_table %>%
                     select("Skeptical") %>% 
                     summarise(percentage = sum(. < log(1)) / n() ) %>% 
                     pull() %>%
                    round(.,2),
                  
                   posteriors_table %>%
                     select("Optimistic") %>% 
                     summarise(percentage = sum(. < log(1)) / n() ) %>% 
                     pull() %>%
                    round(.,2),
                  
                   posteriors_table %>%
                     select("Pessimistic") %>% 
                     summarise(percentage = sum(. < log(1)) / n() ) %>% 
                     pull() %>%
                    round(.,2)
    ),
    
    "Pr(> 1.1)" = c(posteriors_table %>%
                    select("Non-informative") %>% 
                    summarise(percentage = sum(. > log(1.1)) / n() ) %>% 
                    pull() %>%
                      round(.,2),
                    
                  posteriors_table %>%
                    select("Evidence-based") %>% 
                    summarise(percentage = sum(. > log(1.1)) / n() ) %>% 
                    pull() %>%
                    round(.,2),
                  
                  posteriors_table %>%
                    select("Skeptical") %>% 
                    summarise(percentage = sum(. > log(1.1)) / n() ) %>% 
                    pull() %>%
                    round(.,2),
                  
                  posteriors_table %>%
                    select("Optimistic") %>% 
                    summarise(percentage = sum(. > log(1.1)) / n() ) %>% 
                    pull() %>%
                    round(.,2),
                  
                  posteriors_table %>%
                    select("Pessimistic") %>% 
                    summarise(percentage = sum(. > log(1.1)) / n() ) %>% 
                    pull() %>%
                    round(.,2)
    )
    
  )
  
}
