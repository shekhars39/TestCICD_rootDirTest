#' function to create datasets for the screening summary
#' edited version of the create_datasets() function from 1000760_Monash R008 
#' similar to the report_datasets function but more lightweight
#'
#' @return a list but only has one dataframe
#' @param dataset 
#' @param criteria the criteria to use
#' @param start_date the start date to filter on
#' @param end_date the end date to filter on
#' 
screening_report_datasets <- function(dataset, criteria, start_date, end_date = max(dataset$SAMPLE_DATE)){
  
  # chem data maximum
  chem <- dataset %>%
    filter(SAMPLE_DATE >= start_date,
           SAMPLE_DATE <= end_date,
           !is.na(REPORT_RESULT_VALUE)) %>%
    group_by(SYS_SAMPLE_CODE, CAS_RN) %>% 
    mutate(rank = rank(-REPORT_RESULT_VALUE, ties.method = "random")) %>% # highest to lowest for duplicates
    ungroup() %>% 
    filter(rank == 1) %>% # takes max for duplicates
    select(-rank) 
  
  # chems joined with criteria - all data
  chem_criteria <- chem %>%
    # left join here instead of inner join
    left_join(criteria |> 
                # these put in to make it work - called Standard Criterion!!!
                mutate(action_level_code = "Standard Criterion") |> 
                select(-CHEMICAL_NAME, -CRITERIA_TEXT),
              by = c("CAS_RN", "MATRIX_CODE"),
              relationship = "many-to-many") %>%
    report_unit_conversion() 
  
  return(list("chem_criteria" = chem_criteria))
}


#' initial data conversion for the screening
#'
#' @return a list of data frames
#' @param dataset 
#'
screening_initial <- function(dataset){
  
  # loc code order
  loc_code_order <- str_sort(unique(dataset$SYS_LOC_CODE), numeric = TRUE)
  
  # results one to many based on the criteria
  initial <- dataset %>% 
    criteria_unit_conversion() %>% 
    select(CHEMICAL_NAME, SYS_LOC_CODE, SAMPLE_DATE, SYS_SAMPLE_CODE, REPORT_RESULT_UNIT,
           INTERPRETED_QUALIFIERS, action_level_code, adjusted_criteria_value,  REPORT_RESULT_UNIT, REPORT_RESULT_VALUE, DETECT_FLAG) %>% 
    mutate(SYS_LOC_CODE = factor(SYS_LOC_CODE, levels = loc_code_order)) %>% 
    arrange(SYS_LOC_CODE, SYS_SAMPLE_CODE, SAMPLE_DATE)
  
  # only where not all the results have a 'U' qualifier. NAs are fine
  combined <- initial |>  
    group_by(CHEMICAL_NAME) |>  
    filter(
      # Priority 1: Keep rows where INTERPRETED_QUALIFIERS is NA
      any(is.na(INTERPRETED_QUALIFIERS)) | 
        # Priority 2: Keep rows where not all values in INTERPRETED_QUALIFIERS contain 'U'
        !all(str_detect(INTERPRETED_QUALIFIERS, "U"))
    ) |> 
    ungroup() |> 
    arrange(CHEMICAL_NAME, SYS_LOC_CODE)
  
  crits_converted <- initial %>%
    select(CHEMICAL_NAME, action_level_code, adjusted_criteria_value) %>% 
    filter(!is.na(adjusted_criteria_value)) %>% 
    distinct() %>% # needed as duplicates above for the diff criteria
    pivot_wider(id_cols = CHEMICAL_NAME, names_from = action_level_code, values_from = adjusted_criteria_value) %>% 
    left_join(initial %>% 
                distinct(CHEMICAL_NAME, REPORT_RESULT_UNIT),
              by = "CHEMICAL_NAME") 
  
  return(list("combined" = combined,
              "crits_converted" = crits_converted))
}

#' result formatting helper function for the excel output
#'
#' @return nothing, just formats the cells
#' @param dataset 
#' @param workbook the workbook object
#' @param res_col the result column
#' @param row_end the row to stop at
#' @param crit_col the critera column
#' @param format_style the styling object to use
#' @param prog progress if in a shiny app
#' 
screening_result_format <- function(dataset, workbook, res_col, row_end, crit_col, format_style, prog = 0.75/length(dataset)){
  # prog is set at 0.75 to partly account for the bits that happen before and after this
  # these are openxlsx expressions

  col_let <- excel_cols[res_col] # the result column
  qual_let <- excel_cols[(res_col + 1)] # the qualifier column

  # if it is a detect (does not have 'U' qualifier), make bold
  detect_formula <- glue('NOT(ISNUMBER(SEARCH("U", ${qual_let}6)))')

  conditionalFormatting(workbook, "Data with criteria",
                        cols = res_col,
                        rows = 6:row_end,
                        type = "expression",
                        rule = detect_formula,
                        style = bold_style)

  # if non-detect, make italic
  non_detect_formula <- glue('ISNUMBER(SEARCH("U", ${qual_let}6))')

  conditionalFormatting(workbook, "Data with criteria",
                        cols = res_col,
                        rows = 6:row_end,
                        type = "expression",
                        rule = non_detect_formula,
                        style = italic_style)

  # then, colours it in if above criteria and has a criteria (not blank) and does not have 'U' qualifier
  criteria_formula <- glue('AND(${col_let}6>${crit_col}6,
                           ${crit_col}6<>"",
                           NOT(ISNUMBER(SEARCH("U", ${qual_let}6))))')

  conditionalFormatting(workbook, "Data with criteria",
                        cols = res_col,
                        rows = 6:row_end,
                        type = "expression",
                        rule = criteria_formula,
                        style = format_style)

  if(isRunning()){
    incProgress(prog, detail = "Formatting cells")
  }

}

#' helper function for headings in the output
#' 
#' @return a data frame
#' 
#' @param combined_dataset 
#'
screening_headings_helper <- function(combined_dataset){
  
  # heading bits
  headings1 <- combined_dataset %>% 
    distinct(SYS_SAMPLE_CODE, SYS_LOC_CODE, SAMPLE_DATE) %>% 
    mutate(SAMPLE_DATE = as.character(SAMPLE_DATE)) %>% 
    pivot_longer(-SYS_SAMPLE_CODE, names_to = "item", values_to = "value") %>% 
    pivot_wider(id_cols = item, names_from = SYS_SAMPLE_CODE, values_from = value)
  
  headings2 <- bind_cols(headings1 %>% 
                           select(item),
                         map_dfc(seq(from = 2, to = ncol(headings1), by = 1),
                                 ~headings1 %>% 
                                   select(.x) %>% 
                                   mutate(empty = NA)))
  
  return(headings2)
}


#' helper function for additional headings bits
#' 
#' @return a list of outputs used with other functions
#' 
#' @param dataset 
#' @param dataset_wider wide dataset object
#' @param headings output from screening_headings_helper()
#' 
screening_criteria_data_heading_bits <- function(dataset, dataset_wider, headings){
  
  # the system sample codes
  sys_samp_codes <- dataset %>% 
    select(CHEMICAL_NAME, SYS_SAMPLE_CODE, Result = REPORT_RESULT_VALUE, Q = INTERPRETED_QUALIFIERS, REPORT_RESULT_UNIT) %>% 
    distinct(SYS_SAMPLE_CODE) %>% 
    pull()
  
  # the names up to the report unit
  rru_colnum <- match("Unit", colnames(dataset_wider)) # renamed
  init_dat_colnames <- colnames(dataset_wider)[1:rru_colnum] # up to and including rru
  
  dat_colnames <- c(init_dat_colnames,
                    rep(c("Result", "Qualifier"), (length(colnames(dataset_wider)) - length(init_dat_colnames)) / 2))
  
  # this gets put in the excel table
  df <- as_tibble(dataset_wider, .name_repair = ~dat_colnames)
  
  # more heading bits
  blanks <- rep("", length(sys_samp_codes))
  initial_blanks <- setdiff(init_dat_colnames, "Unit")
  new_heading_row <- c(initial_blanks, "Sample Name", c(rbind(sys_samp_codes, blanks)))
  
  heading_rows <- bind_cols(as_tibble(matrix(nrow = 2, ncol = length(initial_blanks)), .name_repair = ~ initial_blanks),
                            headings %>% 
                              mutate(item = case_when(item == "SYS_LOC_CODE" ~ "Well ID",
                                                      item == "SAMPLE_DATE" ~ "Sample Date"))) %>% 
    as_tibble(.name_repair = ~new_heading_row) 
  
  # output
  return(list("heading_rows" = heading_rows,
              "df" = df,
              "init_dat_colnames" = init_dat_colnames,
              "dat_colnames" = dat_colnames,
              "rru_colnum" = rru_colnum))
}



#' creates the screening summary file and uses the previous conversion function
#' 
#' @return the formatted output for a word document
#' 
#' @param criteria_dataset the output from earlier functions
#' 
screening_with_qualifier <- function(crit_dataset){

  # results one to many based on the criteria
  initial <- screening_initial(crit_dataset)
  
  combined <- initial$combined # also used later in the summary table

  # heading bits
  crit_headings <- screening_headings_helper(combined)
  
  # data with criteria wide
  data_wider <- right_join(initial$crits_converted,
                    combined %>% 
                      select(CHEMICAL_NAME,  SYS_SAMPLE_CODE,
                             Result = REPORT_RESULT_VALUE, Q = INTERPRETED_QUALIFIERS, REPORT_RESULT_UNIT) %>% 
                      distinct() %>% # needed as duplicates above for the diff criteria
                      pivot_wider(id_cols = c(CHEMICAL_NAME,  REPORT_RESULT_UNIT),
                                  names_from = SYS_SAMPLE_CODE, 
                                  values_from = c(Result, Q),
                                  names_vary = 'slowest'),
                    by = c("CHEMICAL_NAME", "REPORT_RESULT_UNIT")) %>% 
    # adding the locale sorts regardless of capital letters
    arrange(CHEMICAL_NAME, .locale="en") %>% 
    rename(Unit = REPORT_RESULT_UNIT) %>% 
    convert_mu_symbol(Unit)
  
  crit_head <- screening_criteria_data_heading_bits(combined, data_wider, crit_headings)
  
  # excel stuff - data with criteria --------------------------------------------------
  wb <- createWorkbook()
  modifyBaseFont(wb, fontSize = 10)
  addWorksheet(wb, "Data with criteria")
  writeData(wb, 1, crit_head$heading_rows, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 1)
  addStyle(wb, sheet = 1, style = lft_header_style, rows = 2:5, cols = 1:ncol(crit_head$heading_rows), gridExpand = TRUE)
  writeData(wb, 1, crit_head$df, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 5, keepNA = FALSE) 
  # merge for the chem name column
  mergeCells(wb, "Data with criteria", cols = 1, rows = 1:5)
  # merge for the data columns
  data_result_cols <- seq(length(crit_head$init_dat_colnames) + 1, length(crit_head$dat_colnames), by = 2)
  
  # keep in here as this works, but the standalone doesn't (not sure why not)
  top_cell_merge <- function(col_num){
    mergeCells(wb, "Data with criteria", cols = col_num:(col_num + 1), rows = 1)
    mergeCells(wb, "Data with criteria", cols = col_num:(col_num + 1), rows = 2)
    mergeCells(wb, "Data with criteria", cols = col_num:(col_num + 1), rows = 3)
    mergeCells(wb, "Data with criteria", cols = col_num:(col_num + 1), rows = 4)
  }

  walk(data_result_cols, ~top_cell_merge(.x))

  # check criteria used --------------------------------------
  if("Standard Criterion" %in% colnames(crit_head$df)){
    pos_pri <- match("Standard Criterion", colnames(crit_head$df))
    letter_pri <- LETTERS[pos_pri]
    mergeCells(wb, "Data with criteria", cols = pos_pri, rows = 1:5)
    walk(data_result_cols, ~screening_result_format(crit_head$df, wb, .x, 5 + nrow(crit_head$df), letter_pri, crit_mcl))
  }
  
  # # just for pH if <6.5 or >8.5
  # if("pH" %in% unique(crit_head$df$CHEMICAL_NAME)){
  #   
  #   # get row number
  #   pH_row <- crit_head$df %>% 
  #     select(CHEMICAL_NAME) %>% 
  #     mutate(row = row_number()) %>% 
  #     filter(CHEMICAL_NAME == "pH") %>% 
  #     pull(row)
  #   
  #   pH_result_cells <- map(LETTERS[which(crit_head$dat_colnames == "Result")], ~glue("${.x}${5 + pH_row}"))
  #   
  #   walk(LETTERS[which(crit_head$dat_colnames == "Result")],
  #        function(.x){
  #          pH_cell <- glue("${.x}${5 + pH_row}")
  #          
  #          # not blank and out of range
  #          pH_formula <- glue('AND(OR({pH_cell}<6.5, {pH_cell}>8.5),
  #                             {pH_cell}<>"")')
  #          
  #          conditionalFormatting(wb, "Data with criteria",
  #                                cols = which(LETTERS == .x),
  #                                rows = 5 + pH_row,
  #                                type = "expression",
  #                                rule = pH_formula,
  #                                style = light_yellow)
  #        })
  # }
  
  # add the text at the bottom ---------------
  text_start_row <- 5 + nrow(crit_head$df) + 2
  text_end_row <- text_start_row + nrow(screen_notes) # from app_datasets
  text_abbrevs_col <- length(crit_head$init_dat_colnames) + 1
  writeData(wb, 1, screen_notes, startCol = 1, startRow = text_start_row)
  # format the bits under 'Notes' - not the table above
  conditionalFormatting(wb, "Data with criteria", type = "contains", rule = "Orange", cols = 1, rows = text_start_row:text_end_row, style = crit_mcl)
  conditionalFormatting(wb, "Data with criteria", type = "contains", rule = "bold", cols = 1, rows = text_start_row:text_end_row, style = bold_style)
  conditionalFormatting(wb, "Data with criteria", type = "contains", rule = "italic", cols = 1, rows = text_start_row:text_end_row, style = italic_style)
  # conditionalFormatting(wb, "Data with criteria", type = "contains", rule = "underline", cols = 1, rows = text_start_row:text_end_row, style = underline_style)
  conditionalFormatting(wb, "Data with criteria", type = "contains", rule = "pH values", cols = 1, rows = text_start_row:text_end_row, style = light_yellow)
  writeData(wb, 1, screen_abbrevs, startCol = text_abbrevs_col, startRow = text_start_row)
  setColWidths(wb, sheet = 1, cols = c(1, crit_head$rru_colnum), widths = c(26, 13))
  freezePane(wb, sheet = 1, firstActiveRow = 6, firstActiveCol = 3)
  
  # second tab with summary table ---------------------------------
  # initial
  sum_table1 <- combined %>% 
    filter(!is.na(action_level_code),
           # not pH and exceeding or pH and out of range
           ((CHEMICAL_NAME != "pH" & REPORT_RESULT_VALUE > adjusted_criteria_value) |
              (CHEMICAL_NAME == "pH" & REPORT_RESULT_VALUE < 6.5) |
              (CHEMICAL_NAME == "pH" & REPORT_RESULT_VALUE > 8.5)),
           # interpreted qualifiers does not contain a 'U' or is NA
           (!str_detect(INTERPRETED_QUALIFIERS, "U")| is.na(INTERPRETED_QUALIFIERS))) %>% 
    select(SYS_LOC_CODE, CHEMICAL_NAME, CRITERIA_EXCEEDING = action_level_code,
           SAMPLE_DATE, SYS_SAMPLE_CODE, REPORT_RESULT_VALUE, REPORT_RESULT_UNIT, INTERPRETED_QUALIFIERS) 
  
  # if the summary table needs to be created
  if(nrow(sum_table1) > 0){
    if(length(unique(sum_table1$CRITERIA_EXCEEDING) > 1)){
      # final
      sum_table <- sum_table1 %>% 
        pivot_wider(names_from = CRITERIA_EXCEEDING, values_from = CRITERIA_EXCEEDING) %>% 
        unite("EXCEEDING_STANDARDS", unique(sum_table1$CRITERIA_EXCEEDING), na.rm = TRUE, sep = "; ") %>% 
        convert_mu_symbol(REPORT_RESULT_UNIT) %>% 
        arrange(SYS_LOC_CODE, CHEMICAL_NAME, SAMPLE_DATE) |> 
        # remove the exceeding standard for now as it just says 'Standard_Criterion'
        # fix it when the CRITERIA_TEXT is fixed
        select(-EXCEEDING_STANDARDS)
      
      # write to excel
      addWorksheet(wb, "Exceedance summary")
      writeData(wb, "Exceedance summary", sum_table, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 1)
      text_start_row2 <- nrow(sum_table) + 3
      text_end_row2 <- text_start_row2 + nrow(exceedance_summary_notes) # from app_datasets
      writeData(wb, "Exceedance summary", exceedance_summary_notes, startCol = 1, startRow = text_start_row2)
    }
  }

  # end -------------------
  return(wb)
}
