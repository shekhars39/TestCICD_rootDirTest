#' Function to convert units in the dataset
#'
#' @return A data frame
#' @param dataset 
#' @param cas_rn the CAS_RN numbers to convert
#' @param unit_from the units to convert from 
#' @param unit_to the units to convert to
#' @param divider the value to divide by to convert
#' 
unit_conversion <- function(dataset, cas_rn, unit_from, unit_to, divider){
  
  # data to edit
  data_to_edit <- dataset %>% 
    filter(CAS_RN == cas_rn & REPORT_RESULT_UNIT == unit_from)
  
  # the rest
  data_remainder <- dataset %>% 
    filter(!(CAS_RN == cas_rn & REPORT_RESULT_UNIT == unit_from))
  
  # changes
  edited <- data_to_edit %>% 
    mutate(REPORT_RESULT_VALUE = REPORT_RESULT_VALUE / divider,
           REPORT_RESULT_UNIT = unit_to,
           METHOD_DETECTION_LIMIT = METHOD_DETECTION_LIMIT / divider)
  
  # combine
  bind_rows(edited, data_remainder)
  
}



#' helper function for the unit factors column in the unit conversion functions
#'
#' @return A data frame
#' @param dataset 
#' 
unit_factors_helper <- function(dataset){
  
  dataset %>% 
    mutate(
      # check the units
      same_units = REPORT_RESULT_UNIT == CRITERIA_UNIT,
      # create the unit factors
      unit_factor = case_when(
        # ug to mg
        same_units == FALSE &(grepl(pattern = "ug", x = REPORT_RESULT_UNIT, ignore.case = FALSE)
                              & grepl(pattern = "mg", x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 0.001,
        # mg to ug
        same_units == FALSE & (grepl(pattern = "mg",  x = REPORT_RESULT_UNIT, ignore.case = FALSE) &
                                 grepl(pattern = "ug",  x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 1000,
        # mg to pg
        same_units == FALSE & (grepl(pattern = "mg",  x = REPORT_RESULT_UNIT, ignore.case = FALSE) &
                                 grepl(pattern = "pg",  x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 1e9,
        # ug to pg
        same_units == FALSE & (grepl(pattern = "mg",  x = REPORT_RESULT_UNIT, ignore.case = FALSE) &
                                 grepl(pattern = "pg",  x = CRITERIA_UNIT, ignore.case = FALSE)) ~ 1e6,
        # everything else
        TRUE ~ 1))
}

#' convert values so that they are the same as the criteria
#' dataset needs to be already joined 
#' this converts the criteria action level to match the report units
#'
#' @return A data frame
#' @param dataset 
#' 
criteria_unit_conversion <- function(dataset){
  if(all(c("REPORT_RESULT_VALUE", "REPORT_RESULT_UNIT", "action_level", "CRITERIA_UNIT") %in% colnames(dataset))){
    
    dataset %>% 
      unit_factors_helper() %>% 
      mutate(# invert the above - different to the other function 
        unit_factor = 1 / unit_factor,
        # adjusted criteria level
        adjusted_criteria_value = action_level * unit_factor)
    
  } else {
    
    rlang::abort("dataset needs the columns 'REPORT_RESULT_VALUE', 'REPORT_RESULT_UNIT', 'action_level', 'CRITERIA_UNIT'")
  }
  
}

#' different to criteria_unit_conversion, it adjusts the report value
#' this converts the report_result_value to match the criteria units
#'
#' @return A data frame
#' @param dataset 
#' 
report_unit_conversion <- function(dataset){
  if(all(c("REPORT_RESULT_VALUE", "REPORT_RESULT_UNIT", "action_level", "CRITERIA_UNIT") %in% colnames(dataset))){
    
    dataset %>%
      unit_factors_helper() %>% 
      mutate(adjusted_value = case_when(same_units ==  FALSE ~ REPORT_RESULT_VALUE * unit_factor,
                                        TRUE ~ REPORT_RESULT_VALUE),
             exceeds = case_when(DETECT_FLAG == "N" ~ 0,
                                 !is.na(action_level) & (adjusted_value < action_level |
                                                           adjusted_value > action_level) ~ 1,
                                 adjusted_value > action_level ~ 1,
                                 TRUE ~ 0),
             detects = if_else(DETECT_FLAG == "Y",  1, 0))
    
  } else {
    
    rlang::abort("dataset needs the columns 'REPORT_RESULT_VALUE', 'REPORT_RESULT_UNIT', 'action_level', 'CRITERIA_UNIT'")
  }
  
}
