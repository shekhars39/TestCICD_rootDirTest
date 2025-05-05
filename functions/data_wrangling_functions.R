# data cleaning functions ------------------------------

### not used in this app
#' #' Function to fix the encoding due to UTF-8 values for mu (µ)
#' #' This is as there are issues with the data from the API with encoding
#' #'
#' #' @return A data frame 
#' #' @param dataset 
#' #' 
#' fix_encoding <- function(dataset){
#'   
#'   # add the extra columns
#'   initial <- dataset %>% 
#'     mutate(row_no = row_number(),
#'            encoding = Encoding(REPORT_RESULT_UNIT))
#'   
#'   # get the UTF encoded values
#'   utf8 <- initial %>%  
#'     filter(encoding == "UTF-8") %>% 
#'     mutate(REPORT_RESULT_UNIT = iconv(REPORT_RESULT_UNIT, "UTF-8", "latin1"),
#'            REPORT_RESULT_UNIT = str_replace_all(REPORT_RESULT_UNIT, "µ", "u")) 
#'   
#'   # the rest
#'   other <- initial %>% 
#'     filter(encoding != "UTF-8")
#'   
#'   # join it up
#'   combined <- bind_rows(utf8, other) %>% 
#'     arrange(row_no) %>% 
#'     select(-row_no, -encoding)
#' }

#' Function to convert the a 'u' to the mu symbol (µ)
#' This happens right at the end after all the calculations
#'
#' @return A data frame 
#' @param dataset 
#' @param column_name the column name where the symbol is - usually REPORT_RESULT_UNIT
#' 
convert_mu_symbol <- function(dataset, column_name){
  dataset %>% 
    mutate({{column_name}} := str_replace_all({{column_name}}, "ug\\/L", "\u03BCg\\/L"), # μg/L with μ in unicode
           {{column_name}} := str_replace_all({{column_name}}, "uS\\/cm", "\u03BCS\\/cm"), # μS\cm
           {{column_name}} := str_replace_all({{column_name}}, "umhos\\/cm", "\u03BCmhos\\/cm") # μm
           ) 
}

#' Initial cleaning of the dataset after data upload
#' Other datasets are built off this
#'
#' @return A data frame 
#' @param dataset 
#' @param wanted_cols the columns to look for
#' 
initial_clean <- function(dataset, wanted_cols){
  
  # initial data load and clean
  a1 <- dataset |> 
    # just the columns needed
    select(all_of(wanted_cols)) |> 
    # filter out rows without location codes
    filter(!is.na(SYS_LOC_CODE),
           # r qualifiers removed, needs is.na otherwise NA dropped 
           is.na(INTERPRETED_QUALIFIERS) | INTERPRETED_QUALIFIERS != "R",
           # (CAS_RN != "ORP" & REPORT_RESULT_VALUE > 0) | CAS_RN == "ORP", # ORP values can be below zero, the others shouldn't be
           !is.na(REPORT_RESULT_VALUE)) |> 
    # keep only the N sample types - remove the FD
    filter(SAMPLE_TYPE_CODE == "N",
           # check detect flags
           DETECT_FLAG %in% c("Y", "N")) |>   
    # just date, not datetime
    mutate(SAMPLE_DATE = as_date(SAMPLE_DATE),
           METHOD_DETECTION_LIMIT = as.numeric(METHOD_DETECTION_LIMIT),
           REPORT_RESULT_UNIT = str_replace_all(REPORT_RESULT_UNIT, "l$", "L"),
           REPORT_RESULT_UNIT = str_replace_all(REPORT_RESULT_UNIT, "m3$", "L"), # these should have been removed earlier
           REPORT_RESULT_UNIT = str_replace_all(REPORT_RESULT_UNIT, "^mv", "mV"),
           REPORT_RESULT_UNIT = str_replace_all(REPORT_RESULT_UNIT, "millivolts", "mV"), # convert N to T
           FRACTION = if_else(FRACTION == "N", "T", FRACTION)) 
  
  # chlorinated ethenes but not Chloride
  ce_cas_rn <- cas_rn_graph_groups$`Chlorinated Ethene and Daughter Product`[cas_rn_graph_groups$`Chlorinated Ethene and Daughter Product` != "16887-00-6"]
  
  # methane, manganese
  other_cas_rn <- c("74-82-8", "7439-96-5")
  
  # the CE ones
  b1 <- a1 |> 
    filter(CAS_RN %in% ce_cas_rn)
  
  b2 <- a1 |> 
    filter(CAS_RN %in% other_cas_rn)
  
  # the others
  b3 <- a1 |> 
    filter(!CAS_RN %in% c(ce_cas_rn, other_cas_rn))
  
  # convert to micrograms if it is in milligrams
  c1 <- map_df(ce_cas_rn, function(.x){
    b1 |> 
      filter(CAS_RN == .x) |> 
      unit_conversion(.x, "mg/L", "ug/L", 1/1000)
  }) 
  
  # convert to milligrams if it is in micrograms
  c2 <- map_df(other_cas_rn, function(.x){
    b2 |> 
      filter(CAS_RN == .x) |> 
      unit_conversion(.x, "ug/L", "mg/L", 1000)
  })
  
  # combine it all 
  df <- bind_rows(c1, c2) |> 
    bind_rows(b3)
  
  return(df)
}

#' Picks the highest value based on criteria.
#' For a sample with both N and FD samples:
#' IF only one is detected: use the detected value
#' If both are detected, pick the higher value. If neither are detected, use the higher value.
#' 
#' Other datasets are built off this
#'
#' @return A data frame
#' @param dataset 
#'  
highest_filter <- function(dataset){
  
  dataset %>% 
    # highest filter - this bit goes first - order is important
    mutate(SAMPLE_TYPE_CODE = factor(SAMPLE_TYPE_CODE, levels = c("N", "FD")),
           DETECT_FLAG = factor(DETECT_FLAG, levels = c("Y", "N"))) %>%
    group_by(SAMPLE_DATE, SYS_LOC_CODE, CAS_RN, CHEMICAL_NAME, FRACTION) %>%
    arrange(DETECT_FLAG, desc(REPORT_RESULT_VALUE), SAMPLE_TYPE_CODE) %>%
    slice(1) %>%
    ungroup() %>%
    # deal with duplicates - moved from bannock clean
    group_by(SAMPLE_DATE, CAS_RN, SYS_LOC_CODE, REPORT_RESULT_UNIT, FRACTION) %>%
    mutate(rank = rank(-REPORT_RESULT_VALUE, ties.method = "random")) %>% # highest to lowest for duplicates
    ungroup() %>%
    filter(rank == 1) %>% # takes max for duplicates
    select(-rank)
}



#' create the vector of chemical names for the inorganics datasets
#'
#' @return a vector of CHEMICAL_NAMEs
#' @param dataset
#' @param cas_rn_vec a vector of CAS_RN numbers 
#' 
create_inorg_vector <- function(dataset, cas_rn_vec){
  
  dataset %>% 
    distinct(CAS_RN, CHEMICAL_NAME) %>% 
    filter(CAS_RN %in% cas_rn_vec) %>% 
    pull(CHEMICAL_NAME) %>% 
    sort()
}

#' create the inorganics datasets based on the vector passed in
#'
#' @return a dataset
#' @param dataset
#' @param inorg_vec a vector of chemical names from create_inorg_vector()
#' 
create_inorg_df <- function(dataset, inorg_vec){
  
  dataset %>% 
    filter(CHEMICAL_NAME %in% inorg_vec) %>% 
    mutate(CHEMICAL_NAME = factor(CHEMICAL_NAME, levels = inorg_vec)) %>% 
    arrange(CHEMICAL_NAME, SYS_LOC_CODE, SAMPLE_DATE) %>% 
    mutate(CHEMICAL_NAME = as.character(CHEMICAL_NAME)) # keep this in
          
}
