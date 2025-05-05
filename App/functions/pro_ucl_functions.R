#' function that gets the closest highest number
#'
#' @return a number
#' @param vec a vector of numbers to check against
#' @param num the number to check
closest_higher <- function(vec, num){
  min(vec[vec > num])
}

#' function that gets the closest lowest number
#'
#' @return a number
#' @param vec a vector of numbers to check against
#' @param num the number to check
closest_lower <- function(vec, num){
  max(vec[vec < num])
}


#' generate data for input in to ProUCL
#'
#' @return a list of data frames
#' @param dataset the input dataset
#' @param analyte_group the analytes group to filter on
#' @param loc_group the location group to filter on
#' @param start_date the start date to filter on
#' @param end_date the end date to filter on
#'
generate_pro_ucl_format <- function(dataset, analyte_group, loc_group, start_date, end_date = max(dataset$SAMPLE_DATE)){
  
  df <- dataset %>% 
    mutate(SAMPLE_DATE = as_date(SAMPLE_DATE)) %>%
    filter(SAMPLE_DATE >= start_date,
           SAMPLE_DATE <= end_date,
           METHOD_ANALYTE_GROUP %in% analyte_group,
           LOC_GROUP_CODE == loc_group,
           !is.na(REPORT_RESULT_VALUE)) %>%
    select(SYS_LOC_CODE, LOC_GROUP_CODE, CHEMICAL_NAME, CAS_RN,
           METHOD_ANALYTE_GROUP, MATRIX_CODE, SAMPLE_DATE,
           DETECT_FLAG, REPORT_RESULT_VALUE) %>% 
    arrange(SAMPLE_DATE, LOC_GROUP_CODE, SYS_LOC_CODE, METHOD_ANALYTE_GROUP) 
  
  vals <- df %>%
    select(-DETECT_FLAG, -CAS_RN) %>%
    pivot_wider(SYS_LOC_CODE:SAMPLE_DATE, names_from = CHEMICAL_NAME, values_from = REPORT_RESULT_VALUE) %>%
    bind_cols()
  
  detects <- df %>%
    select(-CAS_RN) %>% 
    pivot_wider(SYS_LOC_CODE:SAMPLE_DATE, names_from = CHEMICAL_NAME, values_from = DETECT_FLAG) %>%
    mutate(across(-(SYS_LOC_CODE:SAMPLE_DATE), ~if_else(.x == "Y", 1, 0))) %>%
    rename_with(~glue("D_{.x}"), .cols = -(SYS_LOC_CODE:SAMPLE_DATE)) %>%
    select(-(SYS_LOC_CODE:SAMPLE_DATE))
  
  chems <- df %>%
    distinct(CAS_RN, CHEMICAL_NAME)
  
  # combine for pro_ucl
  pro_ucl <- bind_cols(vals, detects) %>%
    arrange(SYS_LOC_CODE, SAMPLE_DATE)
  
  
  # output sheets
  sheets <- list("ProUCL" = pro_ucl,
                 "Chems" = chems)
  
  return(sheets)
  
}

# pro ucl version of mann kendall ----------
# # just edit the original one as it is just half the p-value
# pro_ucl_mann_kendall <- function(dataset){
#   
#   # summary stats -----
#   
#   sum_stats <- tibble(n = length(dataset$REPORT_RESULT_VALUE),
#                       min = min(dataset$REPORT_RESULT_VALUE),
#                       max = max(dataset$REPORT_RESULT_VALUE),
#                       mean = mean(dataset$REPORT_RESULT_VALUE),
#                       geom_mean = exp(mean(log(dataset$REPORT_RESULT_VALUE[dataset$REPORT_RESULT_VALUE > 0 ]))),
#                       median = median(dataset$REPORT_RESULT_VALUE),
#                       sd = sd(dataset$REPORT_RESULT_VALUE),
#                       cv = sd / mean)
#   
#   # one sided mann kendall with envstats ----
#   mk <- EnvStats::kendallTrendTest(REPORT_RESULT_VALUE ~ SAMPLE_DATE,
#                                    data = dataset,
#                                    alternative = "greater")
#   
#   mk_vals <- tibble(`M-K test value (S)` = mk$S,
#                     `SD of S` = sqrt(mk$var.S),
#                     `Approximate p-value` = mk$p.value#,
#                     # slope = mk$estimate[2],
#                     # intercept = mk$estimate[3],
#                     # LCL_slope = mk$interval$limits[1],
#                     # UCL_slope = mk$interval$limits[2]
#                     ) 
#   
#   return(list("sum_stats" = sum_stats,
#               "mk_vals" = mk_vals))
#   
# }
