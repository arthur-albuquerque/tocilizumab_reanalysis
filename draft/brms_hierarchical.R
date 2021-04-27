library(tidyverse)
library(brms)
library(hrbrthemes)
library(broom.mixed)
library(brmstools)
library(metafor)
library(here)

source(here("draft", "forest.R"))

##### Load data from the RECOVERY trial on tocilizumab

data = tibble(
    
    outcome = "mortality",
    subgroup = c("no", "noninvasive", "invasive"),
    toci_event = c(175, 296, 125),
    toci_total = c(935, 819, 268),
    control_event = c(202, 350, 142),
    control_total = c(933, 867, 294)
    
              )

# Check data

data

##### Calculate risk difference and standard error

effect_sizes = 
    
    escalc(
    
    # control
    ai = data %>% pull(control_event),
    n1i = data %>% pull(control_total), 
    
    # tocilizumab
    ci = data %>% pull(toci_event),
    n2i = data %>% pull(toci_total),
    
    data = data,
    measure = "RD" # risk difference
    
                     ) 

effect_sizes =
    summary(effect_sizes) %>%
    select(subgroup, yi, sei)

#### Running the Bayesian model

mod1 = 
    brm(data = effect_sizes, 
        family = gaussian,
        
        yi | se(sei) ~ 1 + (1 | subgroup),
        
        prior = c(prior(normal(0, 1), class = Intercept),
                  prior(cauchy(0, 0.1), class = sd)),
        
        warmup = 5000, iter = 30000,
        cores = parallel::detectCores(), chains = 4,
        control = list(adapt_delta = 0.99),
        
        save_all_pars = T,
        seed = 15  # setting the seed for reproducibility
        )

plot(mod1)
summary(mod1)
tidy(mod1,parameters = c("^b_", "^sd_", "Rhat"), prob = 0.95)

forest(
    mod1, grouping = "subgroup", theme_forest = FALSE,
    show_data = TRUE, sort = FALSE, av_name = "Overall effect size"
) +
    theme_ipsum(
        base_family = "Helvetica",
        base_size = 10, plot_title_size = 12, axis_text_size = 9
    ) +
    xlab("Risk difference") +
    ylab("") +
    xlim(-0.05, 0.2)

library(tidybayes)
library(ggdist)
# Subgroup-specific effects are deviations + average
out_r = spread_draws(mod1, r_subgroup[subgroup,term], b_Intercept) %>% 
    mutate(b_Intercept = r_subgroup + b_Intercept) 
# Average effect
out_f = spread_draws(mod1, b_Intercept) %>% 
    mutate(subgroup = "Average")
# Combine average and study-specific effects' data frames
out_all = bind_rows(out_r, out_f) %>% 
    ungroup() %>%
    # Ensure that Average effect is on the bottom of the forest plot
    mutate(subgroup = fct_relevel(subgroup, "Average")) %>% 
    # tidybayes garbles names so fix here
    mutate(subgroup = str_replace_all(subgroup, "\\.", " "))
# Data frame of summary numbers
out_all_sum = group_by(out_all, subgroup) %>% 
    mean_qi(b_Intercept)
# Draw plot
out_all %>%   
    ggplot(aes(b_Intercept, subgroup)) +
    # Zero!
    geom_vline(xintercept = 0, size = .25, lty = 2) +
    stat_halfeye(.width = c(.8, .95), fill = "dodgerblue") +
    # Add text labels
    geom_text(
        data = mutate_if(out_all_sum, is.numeric, round, 2),
        aes(label = str_glue("{b_Intercept} [{.lower}, {.upper}]"), x = 0.25),
        hjust = "inward"
    ) +
    # Observed as empty points
    geom_point(
        data = effect_sizes %>% mutate(subgroup = str_replace_all(subgroup, "\\.", " ")), 
        aes(x=yi), position = position_nudge(y = -.2), shape = 1 
    )
        

