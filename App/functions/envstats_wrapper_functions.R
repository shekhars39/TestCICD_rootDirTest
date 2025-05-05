# initial clean ----------------------------------------------------------------

#' initial helper function used in most of the other functions
#' works out the values, the censored values and the distinct detected values
#'
#' @return A list of `vals`, `cens`, and `dist_detected`
#' @param dataset 
#' 
envstats_initial_helper <- function(dataset){
  
  # dataset to use
  df <- dataset %>% 
    mutate(censored = if_else(DETECT_FLAG == "N", 1, 0))
  
  # these to be used
  vals <- df$REPORT_RESULT_VALUE
  cens <- df$censored
  
  # non-missing uncensored distinct
  dist_detected <- tibble(vals, cens) %>% 
    filter(cens == 0) %>% 
    distinct(vals) %>% 
    pull()
  
  # highest ND value
  initial_high_nd <- max(df[df$DETECT_FLAG == "N",]$REPORT_RESULT_VALUE)
  highest_nd_val <- if_else(abs(initial_high_nd) == Inf, NA_real_, initial_high_nd)

  # if there are any censored values, get the highest U qualified value
  if(sum(cens) > 0){

    # highest U qualified value
    u_qual_vals <- df[df$INTERPRETED_QUALIFIERS == "U" & !is.na(df$INTERPRETED_QUALIFIERS),]
    highest_u_qual <- max(u_qual_vals$REPORT_RESULT_VALUE)

    # substituted values
    sub_vals <- df |>
      mutate(REPORT_RESULT_VALUE2 = if_else(DETECT_FLAG == "N",
                                            highest_u_qual,
                                            REPORT_RESULT_VALUE)) |>
      # but also substitute any detected values that are lower than the highest U qualified value
      mutate(REPORT_RESULT_VALUE2 = if_else(DETECT_FLAG == "Y" & REPORT_RESULT_VALUE < highest_u_qual,
                                            highest_u_qual,
                                            REPORT_RESULT_VALUE2)) |> 
      pull(REPORT_RESULT_VALUE2)

  } else{
    sub_vals <- NA_real_
  }

  return(list("vals" = vals,
              "cens" = cens,
              "dist_detected" = dist_detected,
              "sub_vals" = sub_vals,
              "highest_nd_val" = highest_nd_val))
}


# summary stats -----------------------------------------

#' helper function for the summary stats
#' works out the values, the censored values and the distinct detected values
#'
#' @return a dataframe
#' @param est_object the estimate object from the envstats function
#' @param is_censored boolean value
#' 
envstats_summary_helper <- function(est_object, is_censored){
  
  distribution = est_object$distribution
  sample_size = est_object$sample.size
  dist_method = est_object$method
  
  if(is_censored == TRUE){
    max_DL = max(est_object$censoring.levels)
    pct_censored = round(est_object$percent.censored, 1)
    
    t1 <- tibble(distribution, sample_size, dist_method, max_DL, pct_censored)
  } else {
    
    t1 <- tibble(distribution, sample_size, dist_method, pct_censored = 0.0)
  }
  
  
  if(distribution == "Normal"){
    t2 <- tibble(mean = signif(est_object$parameters[1]),
                 sd = signif(est_object$parameters[2]))
  } else if (distribution %in% c("Lognormal", "Gamma")){
    t2 <- tibble(mean = signif(est_object$parameters[1]),
                 cv = signif(est_object$parameters[2]))
  } else if (distribution == "None"){
    t2 <- tibble(mean = signif(est_object$parameters[1]),
                 sd = signif(est_object$parameters[2]),
                 se_mean = signif(est_object$parameters[3]))
  }
  
  if(!is.null(est_object$interval$limits)){
    # pretty much all cases
    t3 <- tibble(interval_method = str_squish(str_remove_all(est_object$interval$method, "\\n")),
                 interval_param = est_object$interval$parameter,
                 LCL = signif(est_object$interval$limits[1]),
                 UCL = signif(est_object$interval$limits[2]),
                 type = est_object$interval$type,
                 conf_level = est_object$interval$conf.level)
    
  } else {
    # in the case where all the detects are the same and no confidence limits can be calculated
    t3 <- tibble(interval_method = str_squish(str_remove_all(est_object$interval$method, "\\n")),
                 interval_param = est_object$interval$parameter,
                 LCL = NA_real_,
                 UCL = NA_real_,
                 type = NA_character_,
                 conf_level = NA_real_)
  }
  
  bind_cols(t1, t2, t3)
  
}

#' function to bootstrap uncensored data and calculate LCL and UCL using boot package
#'
#' @return a list of various objects
#' @param data the values in the dataset as a vector
#' 
boot_ucl_lcl <- function(data){
  
  # ordinary non param bootstrap
  sim_mean <- boot::boot(data,
                         statistic = function(d, i){
                           d = d[i]
                           return(mean(d))},
                         R = 2500,
                         sim = "ordinary")
  
  sim_sd <- boot::boot(data,
                         statistic = function(d, i){
                           d = d[i]
                           return(sd(d))},
                         R = 2500,
                         sim = "ordinary")
  
  # bias corrected accelerated bootstrap
  bca_boot <- boot::boot.ci(boot.out = sim_mean,
                            type = c("perc"))
  
  return(list("distribution" = "None",
              "sample.size" = length(data),
              "percent.censored" = 0,
              "parameters" = c("mean" = sim_mean$t0,
                               "sd" = sim_sd$t0,
                               "se.mean" = sim_sd$t0 / sqrt(length(data))),
              "method" = "Ordinary nonparametric bootstrap",
              "interval" = list("limits" = c("LCL" = bca_boot$percent[4],
                                             "UCL" = bca_boot$percent[5]),
                                "type" = "two-sided",
                                "method" = "BCa bootstrap"))
  )
}

#' function to get summary stats using envstats  for various distributions
#' used for testing, not in the app anymore
#'
#' @return a list of two objects - `text` and `table`
#' @param dataset
#' 
envstats_summary_stats <- function(dataset){
  
  # initial data using helper function
  a <- envstats_initial_helper(dataset)
  vals <- a$vals
  cens <- a$cens
  dist_detected <- a$dist_detected
  
  # check number of censored
  if(any(cens == 1)){
    # if doesn't have two distinct detected values
    if(length(dist_detected) < 2){
      
      text <- "Does not contain at least two distinct detected values"
      table <- tibble(n = nrow(df),
                      pct_censored = round(sum(cens) / length(cens) * 100, 1),
                      note = "Does not contain at least two distinct detected values")
      
    } else {
      # has enough uncensored
      norm_ss <- enormCensored(vals, cens, ci = TRUE)
      lnorm_alt_ss <- elnormAltCensored(vals, cens, ci = TRUE)
      gamma_alt_ss <- egammaAltCensored(vals, cens, ci = TRUE)
      npar_est_ss <- enparCensored(vals, cens, ci = TRUE)
      
      text <- "Contains censored values"
      table <- map_df(list(norm_ss, lnorm_alt_ss, gamma_alt_ss, npar_est_ss), ~envstats_summary_helper(.x, TRUE)) %>% 
        relocate(c(cv, se_mean), .after = sd) 
    }
    
  } else {
    # is all uncensored
    norm_ss <- enorm(vals, ci = TRUE) # normal
    lnorm_alt_ss <- elnormAlt(vals, ci = TRUE) # lognormal backtransformed
    gamma_alt_ss <- egammaAlt(vals, ci = TRUE) # gamma backtransformed
    npar_alt_ss <- boot_ucl_lcl(vals) # bootstrapped using custom function
    
    # create a helper function to get these in to tibbles
    
    text  <- "No censored values"
    table <- map_df(list(norm_ss, lnorm_alt_ss, gamma_alt_ss, npar_alt_ss), ~envstats_summary_helper(.x, FALSE)) %>% 
      relocate(cv, .after = sd)
  }
  return(list("text" = text,
              "table" = table))
}



# prediction and tolerance intervals ----------------

#' helper function for intervals
#'
#' @return a data frame
#' 
#' @param est an estimates object
#' 
envstats_ints_helper <- function(est){
  
  distribution  <-  est$distribution
  int_type <- est$interval$name
  
  # non-vectorised for t1 tibble below
  method_val <- if(est$distribution != "None"){
    str_squish(str_remove_all(est$method, "\\n"))
  } else {
    NA_character_
  }
  
  t1 <- tibble(distribution = est$distribution,
               method = method_val, # from above
               int_type = est$interval$name,
               type = est$interval$type,
               interval_method = str_squish(str_remove_all(est$interval$method, "\\n")),
               conf_level = est$interval$conf.level)
  
  
  if(distribution == "Normal"){
    t2 <- tibble(mean = signif(est$parameters[1]),
                 sd = signif(est$parameters[2]))
  } else if (distribution %in% c("Gamma")){
    t2 <- tibble(mean = signif(est$parameters[1]),
                 cv = signif(est$parameters[2]))
  } else if (distribution %in% c("Lognormal")){
    t2 <- tibble(mean_log = signif(est$parameters[1]),
                 sd_log = signif(est$parameters[2]))
  } else if (distribution == "None"){
    # t2 <- tibble(mean = signif(est$parameters[1]),
    #              sd = signif(est$parameters[2]),
    #              se_mean = signif(est$parameters[3]))
    t2 <- tibble()
  }
  
  if(int_type == "Prediction"){
    
    t3 <- tibble(LPL = signif(est$interval$limits[1]),
                 UPL = signif(est$interval$limits[2]))
  } else {
    t3 <- tibble(LTL = signif(est$interval$limits[1]),
                 UTL = signif(est$interval$limits[2]))
  }
  
  # output object
  if(distribution == "None"){
    bind_cols(t1, t3)
  } else {
      bind_cols(t1, t2, t3)
  }
}

# ### no longer being used as using non-parametric methods
# # # function for various prediction and tolerance intervals using envstats
# envstats_ints <- function(dataset){
#   
#   # initial data using helper function
#   a <- envstats_initial_helper(dataset)
#   vals <- a$vals
#   cens <- a$cens
#   dist_detected <- a$dist_detected
#   
#   # # prediction intervals (UPL)
#   normal_upl <- predIntNorm(vals, pi.type="upper")
#   
#   lognorm_upl <- predIntLnormAlt(vals, pi.type="upper")
#   
#   gamma1_upl <- predIntGammaAlt(vals,
#                                 pi.type="upper",
#                                 normal.approx.transform = "cube.root") # wilson hilfertly UPL
#   
#   gamma2_upl <- predIntGammaAlt(vals,
#                                 pi.type="upper",
#                                 normal.approx.transform = "fourth.root") # hawkins wixley UPL
#   
#   boot_upl <- predIntNpar(vals, pi.type="upper") 
#   
#   # tolerance intervals (UTL)
#   gamma1_utl <- tolIntGammaAlt(vals,
#                                ti.type="upper",
#                                normal.approx.transform = "cube.root") # wilson hilfertly UTL  - very close
#   
#   gamma2_utl <- tolIntGammaAlt(vals,
#                                ti.type="upper",
#                                normal.approx.transform = "fourth.root") # hawkins wixley UTL  - very close
#   
#   lnorm_utl <- tolIntLnormAlt(vals, ti.type="upper")
#   
#   
#   normal_utl <- tolIntNorm(vals, ti.type="upper")
#   
#   boot_utl <- tolIntNpar(vals, conf.level = 0.95, ti.type = "upper")
#   
#   estimates <- list(normal_upl, lognorm_upl, gamma1_upl, gamma2_upl, boot_upl,
#                     normal_utl, lnorm_utl, gamma1_utl, gamma2_utl, boot_utl) 
#   
#   # output
#   out1 <- map_df(estimates, ~envstats_ints_helper(.x)) %>% 
#     relocate(c(cv, mean_log, sd_log), .after = sd)
#   
#   if(sum(cens) > 0 & length(dist_detected) > 2){
#     # only if censored and enough non-missing
#     lnormc_utl <- tolIntLnormCensored(vals, cens, ti.type="upper") 
#     
#     normc_utl <- tolIntNormCensored(vals, cens, ti.type="upper")
#     
#     out2 <- map_df(list(lnormc_utl, normc_utl), ~envstats_ints_helper(.x)) %>% 
#       relocate(c(mean_log, sd_log), .after = sd)
#     
#     table <- bind_rows(out1, out2)
#     
#   } else {
#     # no censored
#     table <- out1
#   }
#   
#   upl_table <- table %>% 
#     filter(int_type == "Prediction") %>% 
#     select(-int_type, -LTL, -UTL) 
#   
#   utl_table <- table %>% 
#     filter(int_type == "Tolerance") %>% 
#     select(-int_type, -LPL, -UPL) 
#   
#   return(list("upl_table" = upl_table,
#               "utl_table" = utl_table))
# }

# upl functions -------------------------------------------------------------------------------
# three helper functions get used to produce the final UPL output
# this is because you need the to get the UPL for the background well
# the default background_wells_vector is in ./data/app_constants.R


#' first helper function that calculates the UPL values for all
#'
#' @return a dataframe
#' @param dataset
#' @param background_wells a vector of background wells
#' @param prog progression if shiny app
#' @param prog_detail the text to appear if running in shiny with progress
#' 
envstats_upl_background_helper1 <- function(dataset, background_wells = background_wells_vector, prog = NULL, prog_detail = NULL){
  
  # check if it is a background well - only one well gets passeed in
  loc_code <- unique(dataset$SYS_LOC_CODE)
  is_background_well <- if_else(loc_code %in% background_wells, TRUE, FALSE)
  
  # initial data using helper function
  a <- envstats_initial_helper(dataset)
  vals <- a$vals
  cens <- a$cens
  dist_detected <- a$dist_detected
  
  if(is_background_well){
    
    # the UPL values if there are enough values
    if(length(dist_detected) >= 2){
      boot_upl <- predIntNpar(vals, pi.type = "upper") # non detects treated as is
      upl_table <- envstats_ints_helper(boot_upl) %>%
        mutate(interval_method = if_else(interval_method == "Exact", "NP prediction limit for next result", interval_method))
    } else {
      upl_table <- tibble(UPL = NA_real_,
                          conf_level = NA_real_,
                          interval_method = NA_character_)
    }
  } else {
    # non background wells are also shown as na
    upl_table <- tibble(UPL = NA_real_,
                        conf_level = NA_real_,
                        interval_method = NA_character_)
  }
  
  # the chem and well details
  n_bg <- if_else(is_background_well, length(vals), NA_real_)
  nd_bg <- if_else(is_background_well, sum(cens), NA_real_)
  
  data_details <- tibble(SYS_LOC_CODE = loc_code,
                         CHEMICAL_NAME = unique(dataset$CHEMICAL_NAME),
                         `N (background)` = n_bg,
                         `ND (background)` = nd_bg)
  
  # the output
  df <- bind_cols(data_details,
            upl_table %>%
              select(`UPL method` = interval_method,
                     `UPL conf` = conf_level,
                     `UPL of background` = UPL) %>%
              mutate(`UPL conf` = signif(`UPL conf`, 3)))
  
  # increase progress
  if(isRunning()){
    incProgress(prog,
                detail = prog_detail) 
  }
  
  return(df)
  
}


#' second helper function that splits off the background well then joins
#'
#' @return a dataframe
#' @param dataset
#' @param background_wells a vector of background wells
#' @param prog progression if shiny app
#' 
envstats_upl_background_helper2 <- function(dataset, background_wells = background_wells_vector, prog = NULL){
  
  bg_wells_df <- dataset %>% 
    filter(SYS_LOC_CODE %in% background_wells) 
  
  # when there is a background well - so with Cell 2 and Cell 4
  if(nrow(bg_wells_df) > 0){
    
    bg_well <- unique(bg_wells_df$SYS_LOC_CODE)
    
    non_bg_wells_df <- dataset %>% 
      filter(!SYS_LOC_CODE %in% background_wells) %>% 
      select(SYS_LOC_CODE, CHEMICAL_NAME) 
    
    joined_df <- left_join(non_bg_wells_df,
                           bg_wells_df %>% 
                             select(-SYS_LOC_CODE),
                           by = "CHEMICAL_NAME")
    
    output_df <- bind_rows(bg_wells_df,
                           joined_df) %>% 
      mutate(`UPL method` = if_else(!SYS_LOC_CODE %in% background_wells & !is.na(`UPL method`),
                                    glue("{`UPL method`} on {bg_well}"),
                                    `UPL method`))
    
  } else {
    
    output_df <- dataset %>% 
      mutate(`UPL method` = "No background wells")
  }
  
  return(output_df)
}


#' third helper that calculates whether the latest value > UPL
#'
#' @return a dataframe
#' @param dataset
#' @param background_wells a vector of background wells
#' @param prog progression if shiny app
#' 
envstats_upl_background_helper3 <- function(dataset, background_wells = background_wells_vector, prog = NULL){
  
  dataset %>% 
    # comparisons
    mutate(`Latest Result > UPL of background` = case_when(`Latest Result` > `UPL of background` & str_detect(`Latest Q`, "U") == FALSE ~ "Yes",
                                                         `Latest Result` > `UPL of background` & str_detect(`Latest Q`, "U") == TRUE ~ "No (ND)",
                                                         `Latest Result` <= `UPL of background` ~ "No")) %>% 
    relocate(`Latest Result > UPL of background`, .after = `UPL of background`) %>% 
    # get rid of the method analyte group col, but check if is RCRA regulated
    mutate(`RCRA regulated chemical` = if_else(CAS_RN %in% rcra_regulated_cas_rn, "Yes", "No")) %>% 
    relocate(`RCRA regulated chemical`, .before = CAS_RN) %>% 
    rename(`Well ID` = SYS_LOC_CODE)
}

# mann kendall ------------------------------------------------------------------------------


#' a function to calculate Mann-Kendall values using EnvStats::kendallTrendTest 
#' and formatting the output so that it is useful
#' one-sided tests only, based on the direction of S. Not running a two-sided test
#'
#' @return a dataframe with Mann-Kendall and trend outputs
#' @param vals the values in the dataset
#' @param conf_level the confidence level
#' 
envstats_kendall2 <- function(vals, conf_level = 0.95){
  
  # upward version
  ktt_up <- kendallTrendTest(vals, conf.level = conf_level, alternative = "greater")
  
  # check value of S
  if(ktt_up$S >= 0){
    # if S is positive, use the up results
    ktt <- ktt_up
  } else {
    # if S is negative, run with HA of downward and use those
    ktt <- kendallTrendTest(vals, conf.level = conf_level, alternative = "less")
  }

  # this calculates the Z score-threshold used for determining if a trend is present - large sample size only
  crit_Z <-  abs(qnorm((1 - conf_level)/2)) 
  
  initial <- tibble(n = ktt$sample.size,
         # intercept = ktt$estimate[3], # use the ones from the robust linear regression package instead
         # slope = ktt$estimate[2],
         p_value = if_else(is.nan(ktt$p.value), NA_real_, ktt$p.value),
         S = ktt$S,
         varS = ktt$var.S,
         alpha = 1-conf_level) %>% 
    mutate(across(where(is.numeric), ~signif(.x, 3)),
           has_ties = any(duplicated(vals)))
  
  # from ProUCL technical guide v5.20 pp. 276-277
  
  # part of equation 10-14 on page 276
  if(initial$has_ties == TRUE){
    num_ties <- tibble(vals) %>% 
      count(vals) %>% 
      filter(n > 1) %>% 
      pull(n)
    
    sum_ties_value <- sum(map_vec(num_ties, ~(.x * (.x - 1) * (2 * .x + 5))))
  } else {
    sum_ties_value <- 0 # ends up being the same as eq10-15
  }
  
  # this is the output
  initial %>% 
    mutate(sd_S = case_when(n < 23 ~ sqrt(varS),
                            # eq 10-14 - normal approx of S with ties
                            n > 23 & has_ties == TRUE ~ sqrt((1/18) * (n * (n-1) * 2*n + 5 - sum_ties_value)), 
                            # eq 10-15 - normal approx without ties
                            n > 23 & has_ties == FALSE ~ sqrt((1/18) * (n * (n-1) * 2*n + 5)))) %>% 
    # eq 10-16 - relevant for large samples
    mutate(Z = case_when(S > 0 ~ (S-1)/sd_S,
                         S == 0 ~ 0,
                         S < 0 ~ (S + 1)/sd_S)) %>% 
    # calculate the trend and direction
    mutate(
      trend = case_when(
        # small sample size
        n == 3 ~ "Not enough data",
        (abs(p_value) < alpha) & n < 23 ~ "Trend",
        (abs(Z) > crit_Z) & n >= 23 ~ "Trend",
        TRUE ~ "No Trend"),
      
      direction = case_when(
        trend == "Trend" & S > 0 ~ "Increasing",
        trend == "Trend" & S < 0 ~ "Decreasing",
        trend == "Trend" & S == 0 ~ "No Trend",
        
        trend == "Trend" & Z > 0 ~ "Increasing",
        trend == "Trend" & Z < 0 ~ "Decreasing",
        trend == "Trend" & Z == 0 ~ "No Trend",
        trend == "No Trend" ~ "No Trend",
        trend == "Not enough data" ~ "Not enough data")
      )

}


# theil sen -------------------------------------------------------

#' a function to calculate theil-sen based on RobustLinearReg::theil_sen_regression
#'
#' @return a dataframe with slope and intercept values
#' @param dataset 
#' 
theil_sen_helper <- function(dataset){
  ts <-  RobustLinearReg::theil_sen_regression(REPORT_RESULT_VALUE ~ SAMPLE_DATE, data = dataset)
  
  tibble(slope = coef(ts)[[2]],
         intercept = coef(ts)[[1]])

}

# combined mk, conf and pred ------------------------------------------------------

#' helper function for grouping and filtering, then splitting in by the chem and well combination
#' used for the mann kendall functions
#'
#' @return a list of split datasets
#' @param dataset 
#' 
mk_group_filter_split_helper <- function(dataset){
  
  dataset %>% 
    group_by(CHEMICAL_NAME, SYS_LOC_CODE) %>% 
    add_count() %>% 
    ungroup() %>%
    arrange(SYS_LOC_CODE, CHEMICAL_NAME, SAMPLE_DATE) %>% 
    mutate(chem_well = paste(CHEMICAL_NAME, SYS_LOC_CODE)) %>% 
    split(.$chem_well)
}


#' function to generate LCL/UCL and UPL values
#'
#' @return a data frame
#' @param dataset 
#' @param substitute_vals either 'AV' (default) or 'SV'
#' @param mk_conf the Mann-Kendall confidence interval
#' @param progress boolean if running in Shiny
#' 
envstats_mk_conf_pred <- function(dataset, substitute_vals = "AV", mk_conf = 0.95, prog = NULL){
  
  # if the length of the dataset is greater than 40 observations, then just use the most recent 40 obs
  if(nrow(dataset) > 40){
    dataset <- dataset %>% 
      # it should already be arranged by date, but make sure
      arrange(SAMPLE_DATE) %>% 
      tail(40)
  } else {
    dataset <- dataset
  }
  
  # initial data using helper function
  a <- envstats_initial_helper(dataset)
  vals <- a$vals
  cens <- a$cens
  dist_detected <- a$dist_detected
  sub_vals <- a$sub_vals # these are the substituted values using the highest U value where ND
  highest_nd_val <- a$highest_nd_val
  
  # data details
  details_table <- tibble(SYS_LOC_CODE = unique(dataset$SYS_LOC_CODE),
                          CAS_RN = unique(dataset$CAS_RN),
                          CHEMICAL_NAME = unique(dataset$CHEMICAL_NAME),
                          REPORT_RESULT_UNIT = unique(dataset$REPORT_RESULT_UNIT),
                          `Dataset start` = min(dataset$SAMPLE_DATE),
                          `Dataset end` = max(dataset$SAMPLE_DATE),
                          `Min detected` = if_else(length(dist_detected) > 0, min(dist_detected), NA_real_),
                          `Max detected` = if_else(length(dist_detected) > 0, max(dist_detected), NA_real_),
                          `Max ND` = highest_nd_val,
                          `Latest Result` = dataset[nrow(dataset),]$REPORT_RESULT_VALUE,
                          `Latest Q` = dataset[nrow(dataset),]$INTERPRETED_QUALIFIERS) 
  
  # mann kendall with theil sen from robustlinearregression package
  # in case all non_detects, fewer than six values or > 50% ND
  if(length(vals) == sum(cens) | length(vals) < 6 | (sum(cens) / length(vals) > 0.5)){
    mk_table <-  tibble(n = length(vals),
           slope = NA_real_,
           intercept = NA_real_,
           p_value = NA_real_,
           `Confidence Level %` = NA_real_,
           S = NA_real_,
           varS = NA_real_,
           alpha = NA_real_,
           has_ties = NA,
           sd_S = NA_real_,
           Z = NA_real_,
           trend = NA_character_,
           direction = NA_character_)
    
  } else if (substitute_vals == "SV"){
    # use the substituted values "SV"
    mk_table <- bind_cols(envstats_kendall2(sub_vals, mk_conf),
                          theil_sen_helper(dataset)) %>% 
      mutate(`Confidence Level %` = round((1-p_value) * 100, 1)) %>% 
      relocate(c(slope, intercept), .after = n) %>% 
      relocate(`Confidence Level %`, .after = p_value)
    
  } else {
    # use actual values "AV" - default!!!
    mk_table <- bind_cols(envstats_kendall2(vals, mk_conf),
                          theil_sen_helper(dataset)) %>% 
      mutate(`Confidence Level %` = round((1-p_value) * 100, 1)) %>% 
      relocate(c(slope, intercept), .after = n) %>% 
      relocate(`Confidence Level %`, .after = p_value)
  }
  
  # mean, sd, LCL and UCL
  # check number of censored
  if(any(cens == 1)){
    # if doesn't have two distinct detected values
    if(length(dist_detected) < 2){
      sum_stats_table <- tibble(distribution = "None",
                                dist_method = "Does not take in to account ND status",
                                pct_censored = round(sum(cens) / length(cens) * 100, 1),
                                mean = mean(vals),
                                sd = sd(vals),
                                interval_method = "Not enough distinct detected values to assess",
                                conf_level = NA_real_,
                                LCL = NA_real_,
                                UCL = NA_real_)
      
    } else {
      
      # not doing this!!!!
      # # has enough uncensored and all positive numbers
      # if(min(vals) <= 0){
      #   # replace the zeros with NA
      #   # might need to do something about the negative numbers too, but leave for now
      #   vals <- na_if(vals, 0)
      # }
      
      npar_est_ss <- tryCatch(enparCensored(vals, cens, ci = TRUE, n.bootstraps = 2500, ci.method = "bootstrap"),
                              # just in case the bootstrap doesn't work
                              error = function(e) enparCensored(vals, cens, ci = TRUE, ci.method = "normal.approx"))
        
      sum_stats_table <- envstats_summary_helper(npar_est_ss, TRUE) |>  
        mutate(dist_method = if_else(dist_method == "Kaplan-Meier", "NP Kaplan-Meier", dist_method))
    }
    
  } else {
    # is all uncensored
    npar_alt_ss <- boot_ucl_lcl(vals) # bootstrapped using custom function
    sum_stats_table <- envstats_summary_helper(npar_alt_ss, FALSE) %>% 
      mutate(conf_level = 0.95)
  }
  
  # GSI toolkit trend info - same as we are only using the last 40 obs
  gsi_COV <- sd(vals, na.rm = TRUE) / mean(vals, na.rm = TRUE)
  gsi_CF <- (1 - mk_table$p_value) # CF as (1 - p)%.
  gsi_concentration_trend <- case_when(mk_table$S > 0 & gsi_CF > .95 ~ "Increasing",
                                       mk_table$S > 0 & between(gsi_CF, 0.9, 0.95) ~ "Probably Increasing",
                                       mk_table$S > 0 & gsi_CF < 0.9 ~ "No trend",
                                       mk_table$S <= 0 & gsi_CF < 0.9 & gsi_COV >= 1 ~ "No Trend",
                                       mk_table$S <= 0 & gsi_CF < 0.9 & gsi_COV < 1 ~ "Stable",
                                       mk_table$S < 0 & between(gsi_CF, 0.9, 0.95) ~ "Probably Decreasing",
                                       mk_table$S < 0 & gsi_CF > .95 ~ "Decreasing",
                                       TRUE ~ NA_character_)

  # output
  df <- bind_cols(details_table,
                  sum_stats_table %>%
                    select(`ND %` = pct_censored,
                           `Parameter est. method` = dist_method,
                           mean,
                           sd,
                           `CL method` = interval_method,
                           `CL conf` = conf_level,
                           LCL,
                           UCL) %>% 
                    mutate(LCL = signif(LCL, 3),
                           UCL = signif(UCL, 3)),
                  mk_table) %>% 
    relocate(`ND %`, .after = n) %>% 
    rename(`sd(S)` = sd_S,
           `LCL of the mean` = LCL,
           `UCL of the mean` = UCL) %>% 
    select(-varS, -trend, -has_ties, -alpha) %>% 
    relocate(`sd(S)`, .after = S) %>% 
    mutate(across(where(is.numeric), ~signif(.x, 3)),
           # add the GSI trend to the end
           COV = signif(gsi_COV, 3),
           `GSI Toolkit Trend Direction` = gsi_concentration_trend) 
  
  # increase progress
  if(isRunning()){
    incProgress(prog) 
  }
  
  return(df)
}

#' helper function at the end for the App Stats output
#'
#' @return a data frame
#' @param dataset 
#' @param bg_upf_df a data frame if there are background wells
#' @param criteria_units_table the criteria table to use
#' 
mk_conf_pred_end_helper <- function(dataset,
                                    bg_upl_df = NULL,
                                    criteria_units_table){
  
  initial <- dataset %>% 
    arrange(SYS_LOC_CODE, CHEMICAL_NAME)  %>% 
    left_join(criteria_units_table %>%
                select(-CHEMICAL_NAME, -MATRIX_CODE),
              by = "CAS_RN") %>%
    # adjust the action level - slightly diff to the ones in functions already, keep separate
    mutate(same_units = REPORT_RESULT_UNIT == CRITERIA_UNIT,
           unit_factor = case_when(same_units == FALSE &
                                     (grepl(pattern = "ug", x = REPORT_RESULT_UNIT, ignore.case = FALSE)
                                      & grepl(pattern = "mg", x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 0.001,
                                   same_units == FALSE & (grepl(pattern = "mg",  x = REPORT_RESULT_UNIT, ignore.case = FALSE) &
                                                            grepl(pattern = "ug",  x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 1000, TRUE ~ 1),
           Standard = case_when(same_units ==  FALSE ~ action_level / unit_factor, TRUE ~ action_level)) %>% 
    relocate(Standard, .before = `Latest Result`) %>% 
    select(-same_units, -unit_factor, -action_level, -CRITERIA_UNIT, -CRITERIA_TEXT) %>% # no matrix code
    mutate(`UCL > Standard` = if_else(`UCL of the mean` > Standard, "Yes", "No"),
           `LCL > Standard` = if_else(`LCL of the mean` > Standard, "Yes", "No"),
           `Latest Result > mean` = if_else(`Latest Result` > mean, "Yes", "No"),
           `Latest Result > Standard` = if_else(`Latest Result` > Standard, "Yes", "No")) %>% 
    convert_mu_symbol(REPORT_RESULT_UNIT) %>% 
    rename(Unit = REPORT_RESULT_UNIT) %>% 
    rename(`TS Intercept` = intercept,
           `TS Slope` = slope,
           `p-value` = p_value,
           `ProUCL Method Trend Direction` = direction,
           `Standard source` = action_level_code) %>% 
    relocate(`Standard source`, .before = Standard) %>% 
    rename(`Dataset n` = n,
           `Dataset mean` = mean) %>% 
    mutate(`Latest Q` = if_else(is.na(`Latest Q`), " ", `Latest Q`))
  
  # for the application stats
  if(!is.null(bg_upl_df)){
    
    output <- initial %>% 
      # join to the background UPL table
      left_join(bg_upl_df, by = c("SYS_LOC_CODE", "CHEMICAL_NAME")) %>% 
      relocate(c(`Dataset n`, `ND %`), .after = `Latest Q`) %>% 
      relocate(c(`N (background)`, `ND (background)`, `UPL method`, `UPL conf`, `UPL of background`), .after = `UCL of the mean`) %>% 
      relocate(c(`UCL > Standard`, `LCL > Standard`, `Latest Result > mean`, `Latest Result > Standard`), .after = `UCL of the mean`)
  } else {
    
    # this is the version that is in the single dataset view
    output <- initial %>% 
      # join to the background UPL table
      relocate(c(`Latest Result > mean`, `Latest Result > Standard`, `Dataset n`, `ND %`), .after = `Latest Q`) %>% 
      relocate(c(`UCL > Standard`, `LCL > Standard`), .after = `UCL of the mean`) %>% 
      relocate(COV, .after = sd)
  }
  
  return(output)

}
