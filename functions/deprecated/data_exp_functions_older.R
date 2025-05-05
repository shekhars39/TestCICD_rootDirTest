chlorinated_ethene_graph <- function(dataset, analyte_label_dataset, well_selected, analytes_selected, y_scale = "normal"){
  
  df <- dataset %>% 
    filter(analyte %in% analytes_selected,
           well_id == well_selected) %>% 
    left_join(analyte_label_dataset, by = "analyte") %>% 
    mutate(analyte_short = factor(analyte_short, levels = analyte_short_factor_order))
  
  p <- ggplot(df, aes(sample_date, result, colour = analyte_short, shape = detect)) +
    theme_minimal() +
    # pce and tce mcl line and text
    annotate("text", x = min(df$sample_date), y = 5, label = "PCE and\n TCE MCL", size = 3, colour = "grey") +
    geom_hline(yintercept = 5, linetype = "dashed", colour = "grey") +
    geom_point(size = 2) +
    scale_x_date(labels = scales::label_date_short()) +
    labs(
      title = glue("{well_selected} Chlorinated Ethene and Daughter Product"),
      x = "Sample Date",
      # y = expression(paste("Organic Compound Concentration (", mu, "g/L)")),
      y = expression(atop(
        paste("Organic Compound Concentration (", mu, "g/L)"),
        paste("Chloride Concentration (mg/L)")
      )
      ),
      colour = NULL,
      shape = "Detected"
    ) +
    theme(legend.position = "bottom") +
    scale_shape_manual(values = c("N" = 1, "Y" = 19)) +
    scale_colour_manual(values = colour_chlorinated) +
    
    guides(colour = guide_legend(nrow = 3, byrow = TRUE),
           shape = guide_legend(nrow = 2, byrow = TRUE))
  
  if(y_scale == "log10"){
    p + scale_y_log10() +
      labs(subtitle = "Mass Concentrations (Log Scale)")
  } else {
    p
  }
  
} # end function