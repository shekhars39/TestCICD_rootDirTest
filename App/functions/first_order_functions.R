
#' create the table and graphs for the first order model
#'
#' @return a list of the joined_df, table, lm_graph, lm_tidy, lm_glance, exp_table, exp_tidy and exp_glance objects
#' @param data_df the dataset
#' @param well_selected the well
#' @param analyte_selected the chemical
#' @param min_date the start date of the graph
#' @param max_date the end date of the graph
#' @param frac the fraction filter, either 'T' or 'D'
#' @param criteria_df the criteria dataset
#' @param molar_mass_table a lookup table of molar concentrations 
#' @param conf_level the confidence interval level
#' @param is_ggiraph whether to use ggiraph for interactive graph or not
#'
first_order_data <- function(data_df, well_selected, analyte_selected, min_date, max_date, frac,
                             criteria_df, molar_mass_table, conf_level = 0.95, is_ggiraph = FALSE){
  
  # chem data
  df_raw <- data_df |> 
    filter(SYS_LOC_CODE == well_selected,
           CHEMICAL_NAME  == analyte_selected,
           SAMPLE_DATE >= min_date,
           SAMPLE_DATE <= max_date,
           FRACTION == frac) |> 
    convert_mu_symbol(REPORT_RESULT_UNIT)
  
  df <- df_raw |> 
    select(CAS_RN, SAMPLE_DATE, REPORT_RESULT_VALUE)
  
  # molar mass data
  molar_mass_df <- voc_molecular_weights |> 
    filter(CAS_RN == unique(df$CAS_RN)) 
  
  if(nrow(molar_mass_df) == 1){
    
    # the molecular weight
    molar_mass <-  molar_mass_df |> 
      pull(MOLECULAR_WEIGHT)
    
    # then continue
    
    # joined df and mass
    joined_df <- df |> 
      mutate(DAYS = as.vector(SAMPLE_DATE - min(SAMPLE_DATE)),
             CONCENTRATION_umol_L = REPORT_RESULT_VALUE / molar_mass,
             ln_concentration = log(CONCENTRATION_umol_L)) |> 
      arrange(SAMPLE_DATE)
    
    # clean version of joined_df for output
    values_df <- joined_df |> 
      mutate(across(CONCENTRATION_umol_L:ln_concentration, ~signif(.x, 3))) |> 
      select(SAMPLE_DATE,
             REPORT_RESULT_VALUE,
             DAYS, 
             `CONCENTRATION (umol/L)` = CONCENTRATION_umol_L,
             `ln(conentration)` = ln_concentration)
    
    # linear regression
    fit <- lm(ln_concentration ~ DAYS, data = joined_df)
    slope <- fit$coefficients[2][[1]]
    intercept <- fit$coefficients[1][[1]]
    half_life_est <- abs(log(2) / slope)
    lm_tidy <- tidy(fit) |> 
      mutate(across(where(is.numeric), ~signif(.x, 3)))
    lm_glance <- glance(fit) |> 
      mutate(across(everything(), ~signif(.x, 3))) |> 
      pivot_longer(everything())
    
    # values for ribbon for confidence interval
    pred_values <- predict(fit, tibble(DAYS = joined_df$DAYS), interval = "confidence", level = conf_level)
    pred_df <- tibble(DAYS = joined_df$DAYS,
                      ln_concentration = pred_values[,1],
                      lwr = pred_values[,2],
                      upr = pred_values[,3])
    
    # from the criteria - groundwater protection standard
    gps_df <- criteria_df |> 
      filter(CAS_RN == unique(df$CAS_RN)) |> 
      select(CAS_RN, action_level)
    
    if(nrow(gps_df) == 1){
      # get the gps value
      gps <- gps_df |> 
        pull(action_level)
      
      # then continue
      gps_umol_L <- gps / molar_mass
      ln_gps <- log(gps_umol_L)
      
      # calc using intercept from regression calc
      initial_date <- min(joined_df$SAMPLE_DATE)
      initial_concentration <- joined_df |> 
        filter(SAMPLE_DATE == initial_date) |> 
        pull(ln_concentration)
      time_to_gps_from_intercept <- (ln_gps - intercept) / slope
      date_of_predicted_gps_from_intercept <- initial_date + time_to_gps_from_intercept
      
      # from initial concentration
      initial_conc_date <- min(joined_df$SAMPLE_DATE)
      time_to_gps_from_initial_conc <- (ln_gps - initial_concentration) / slope
      date_of_predicted_gps_from_initial_conc <- initial_conc_date + time_to_gps_from_initial_conc
      
      # output as tibble
      table <- tibble(`k estimate (day-1)` = slope,
                      `Half-life estimate (day)` =  half_life_est,
                      Intercept = intercept,
                      `GPS (ug/L)` = gps,
                      `GPS (umol/L)` = gps_umol_L,
                      `ln(GPS)` = ln_gps,
                      `Time to achieve GPS from intercept (day)` = time_to_gps_from_intercept,
                      `Date of predicted achievement of GPS from intercept` = date_of_predicted_gps_from_intercept,
                      `Time to achieve GPS from initial concentration (day)` = time_to_gps_from_initial_conc,
                      `Date of predicted achievement of GPS from initial concentration` = date_of_predicted_gps_from_initial_conc) |> 
        mutate(across(where(is.numeric), ~signif(.x, 6))) |> 
        mutate(across(everything(), as.character)) |> 
        pivot_longer(everything())
      
      # x axis things
      if(time_to_gps_from_intercept > 0){
        # if there is time to the intercept, then go until there
        x_breaks <- seq(0, time_to_gps_from_intercept, 365.25)
        x_labels <- 0:(length(x_breaks)-1)
        x_limits <- c(0, time_to_gps_from_intercept)
      } else {
        # just end it where it would end
        x_breaks <- seq(0, max(joined_df$DAYS), 365.25)
        x_labels <- 0:(length(x_breaks)-1)
        x_limits <- c(0, NA)
      }
      
      # if positive, then add the +, else has a -
      linear_subtitle <- if_else(slope > 0,
                                 glue("y = {signif(intercept, 6)} + {signif(slope, 6)}x"),
                                 glue("y = {signif(intercept, 6)} {signif(slope, 6)}x"))
      
      # linear graph
      lm_initial <- ggplot(joined_df, aes(DAYS, ln_concentration)) +
        theme_bw() +
        geom_ribbon(data = pred_df, aes(ymin = lwr, ymax = upr), colour = "grey", alpha = .15) +
        geom_abline(aes(intercept = intercept,
                        slope = slope,
                        colour = "First-order model"),
                    key_glyph = "path")
      
      if(is_ggiraph == TRUE){
        lm2 <- lm_initial +
          geom_point_interactive(aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                                  CONCENTRATION: {signif(ln_concentration, 3)} (\u03BCmol/L)")),
                                 size = 2)
      } else {
        lm2 <- lm_initial +
          geom_point(size = 2)
      }
      
      lm_g <- lm2 +
        geom_point() +
        geom_hline(aes(yintercept = ln_gps, colour = "GPS line")) +
        scale_x_continuous(breaks = x_breaks,
                           labels = x_labels,
                           limits = x_limits) +
        scale_colour_manual(values = c("First-order model" = "black",
                                       "GPS line" = "red")) +
        theme(legend.position = "bottom") +
        labs(x = glue("Years from {format(initial_conc_date, '%b %d, %Y')}"),
             y = glue("ln({analyte_selected})"),
             title = glue("{well_selected}. {analyte_selected}"),
             subtitle = linear_subtitle,
             caption = glue("Confidence level: {conf_level}. Note: slope calculated in days, but x-axis shown in years."),
             colour = NULL)
      
      # exponential regression
      fit_exp <- lm(log(REPORT_RESULT_VALUE) ~ DAYS, data = joined_df)
      exp_tidy <- tidy(fit_exp) |> 
        mutate(across(where(is.numeric), ~signif(.x, 3))) 
      # no exp_glance as will be the same

      
      # whether to have the zero value
      if(time_to_gps_from_intercept > 0){
        days_exp <- seq(0, time_to_gps_from_intercept, 30)
      } else {
        days_exp <- seq(0, max(joined_df$DAYS), 30)
      }
      
      # data frame for exp line - 95% conf int by default
      values_exp <- exp(predict(fit_exp, tibble(DAYS = days_exp), interval = "confidence", level = conf_level))
      exp_df <- tibble(DAYS = days_exp,
                       REPORT_RESULT_VALUE = values_exp[,1],
                       lwr = values_exp[,2],
                       upr = values_exp[,3])
      
      
      # if positive, then add the +, else has a -
      exp_subtitle <- if_else(fit_exp$coefficients[2][[1]] > 0,
                              glue("y = e^({signif(fit_exp$coefficients[1][[1]], 6)} + {signif(fit_exp$coefficients[2][[1]], 3)}x)"),
                              glue("y = e^({signif(fit_exp$coefficients[1][[1]], 6)} {signif(fit_exp$coefficients[2][[1]], 3)}x)"))
      
      # exponential regression
      exp_initial <- ggplot(joined_df, aes(DAYS, REPORT_RESULT_VALUE)) +
        theme_bw() +
        geom_hline(aes(yintercept = gps, colour = "GPS line")) +
        geom_ribbon(data = exp_df, aes(ymin = lwr, ymax = upr), colour = "grey", alpha = .15) +
        geom_line(data = exp_df, aes(DAYS, REPORT_RESULT_VALUE, colour = "Exponential first-order model"))
      
      if(is_ggiraph == TRUE){
        exp2 <- exp_initial +
          geom_point_interactive(aes(tooltip = glue("SAMPLE_DATE: {SAMPLE_DATE}
                                                  CONCENTRATION: {signif(REPORT_RESULT_VALUE, 3)} (\u03BCg/L)")),
                                 size = 2)
      } else {
        exp2 <- exp_initial +
          geom_point(size = 2)
      }
      
      exp_g <- exp2 +
        scale_colour_manual(values = c("Exponential first-order model" = "black",
                                       "GPS line" = "red")) +
        scale_x_continuous(breaks = x_breaks,
                           labels = x_labels,
                           limits = x_limits) +
        theme(legend.position = "bottom") +
        labs(x = glue("Years from {format(initial_conc_date, '%b %d, %Y')}"),
             y = unique(df_raw$REPORT_RESULT_UNIT),
             title = glue("{well_selected}. {analyte_selected}"),
             subtitle = exp_subtitle,
             caption = glue("Confidence level: {conf_level}. Note: slope calculated in days, but x-axis shown in years."),
             colour = NULL)
      
    } else {
      # if there is no criteria, also produce an error
      table <- tibble(text = "No criteria data")
      lm_g <- blank_graph()
      exp_g <- blank_graph()
    }
    
  } else {
    # some sort of error on not having a molecular weight
    table <- tibble(text = "No molecular mass data")
    lm_g <- blank_graph()
    exp_g <- blank_graph()
  }
  
  # output dependent on whether ggiraph or not
  if(is_ggiraph == TRUE){
    
    lm_graph <- girafe(ggobj = lm_g,
           # no button - there is a manual one
           options = list(opts_toolbar(saveaspng = FALSE)))
    
    exp_graph <- girafe(ggobj = exp_g,
                       # no button - there is a manual one
                       options = list(opts_toolbar(saveaspng = FALSE)))
  } else {
    lm_graph <- lm_g
    exp_graph <- exp_g
  }
  
  return(list("values_df" = values_df,
              "table" = table,
              "lm_tidy" = lm_tidy,
              "lm_glance" = lm_glance,
              "lm_graph" = lm_graph,
              "exp_tidy" = exp_tidy,
              "exp_graph" = exp_graph))
  
} # end function
