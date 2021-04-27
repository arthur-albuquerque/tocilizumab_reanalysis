# model <- bmod1_forest
# 
# grouping = "article"; pars = NA; level = 0.95; av_name = "Average"; 
# sort = FALSE; show_data = TRUE; col_ridge = NA; fill_ridge = "grey75"; 
# density = TRUE; text = TRUE; rel_min_height = 0.01; scale = 0.9;
# digits = 2; theme_forest = FALSE;

forest <- function(
    model, grouping = NA, pars = NA, level = 0.95, av_name = "Average", 
    sort = TRUE, show_data = FALSE, col_ridge = NA, fill_ridge = "grey75", 
    density = TRUE, text = TRUE, rel_min_height = 0.01, scale = 0.9,
    digits = 2, theme_forest = TRUE
    ) {
    
    if (!requireNamespace("ggridges", quietly = TRUE) ) {
        
        stop("ggridges package needed for this function. Please install it.", call. = FALSE)
        
    }
    
    grouping <- brmstools:::get_grouping(model, grouping)
    probs <- c(0.5 - level / 2, 0.5 + level / 2)
    
    lwr <- paste0(probs[1] * 100, "%ile")
    upr <- paste0(probs[2] * 100, "%ile")
    
    samples <- tidycoef(model, pars = pars, grouping = grouping)
    
    samples_sum <- tidycoef(
        model, pars = pars, grouping = grouping, 
        summary = TRUE, level = level
        )
    
    samples[[grouping]] <- ifelse(is.na(samples[[grouping]]), av_name, samples[[grouping]])
    
    samples_sum[[grouping]] <- ifelse(is.na(samples_sum[[grouping]]), 
        av_name, samples_sum[[grouping]])
    
    samples_sum[["Interval"]] <- paste0(round(samples_sum[["Estimate"]], 
        digits), " [", round(samples_sum[[lwr]], digits), ", ", 
        round(samples_sum[[upr]], digits), "]")
    
    if (sort) {
        
        samples_sum <- dplyr::arrange_(samples_sum, "type", "Parameter", "Estimate")
        
    } else {
    
        samples_sum <- dplyr::arrange(samples_sum, type, desc(subgroup) )
        
    }
    
    samples_sum[["order"]] <- forcats::fct_inorder(paste0(samples_sum[["type"]], 
        samples_sum[[grouping]], samples_sum[["Parameter"]]) )
    
    samples <- dplyr::left_join(samples, samples_sum[, c(grouping, 
        "Parameter", "order")], by = c(grouping, "Parameter") )
    
    g <- ggplot(samples_sum, aes_string("Estimate", "order") ) + 
        scale_y_discrete(labels = samples_sum[[grouping]], breaks = samples_sum[["order"]]) +
        geom_vline(
            xintercept = samples_sum %>% filter(type == "b") %>% pull(Estimate),
            linetype = 3
            ) +
        geom_point()
    
    if (density) {
        
        g <- g +
            ggridges::geom_density_ridges(
                data = samples, aes_string(x = "value"),
                rel_min_height = rel_min_height, 
                scale = scale, col = col_ridge, fill = fill_ridge
                ) +
            geom_point()
        
    }
    
    g <- g +
        geom_segment(
            aes_(y = ~order, yend = ~order, x = as.name(lwr), xend = as.name(upr) )
            )
    
    if (text) {
        
        g <- g +
            geom_text(
                data = samples_sum[samples_sum[["type"]] == "b", ],
                aes_string(label = "Interval", x = "Inf"),
                hjust = "inward", fontface = "bold", size = 3
                ) + 
            geom_text(
                data = samples_sum[samples_sum[["type"]] == "r", ],
                aes_string(label = "Interval", x = "Inf"), 
                hjust = "inward", size = 3
                )
        
    }
    
    if (show_data & length(unique(samples_sum[["Parameter"]]) ) == 1) {
        
        tmp <- dplyr::left_join(model$data, samples_sum[, c(grouping, "order")])
        
        g <- g +
            geom_point(
                data = tmp, aes_string(
                    attr(attr(model$data, "terms"), "term.labels")[1], "order"),
                shape = 8
                )
        
    }
    
    # g <- g + facet_wrap("Parameter", scales = "free", strip.position = "bottom")
    
    if (theme_forest) g <- g + theme_forest()
    
    g
    
}
