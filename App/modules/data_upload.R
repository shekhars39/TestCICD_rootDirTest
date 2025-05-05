#  ui side ----------------------
data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Get the data upload template"),
    p("Uploaded data needs to be in the right format"),
    downloadButton(ns("download"),
                 "Get upload template"),
    br(),
    h2("Upload your dataset"),
    p("Upload your EQuIS output file in the template provided. Limit 16MB."),
    fileInput(ns("upload"),
              label = NULL,
              multiple = FALSE,
              accept = ".xlsx"),
    uiOutput(ns("contents")),
    uiOutput(ns("d_load"))
  ) # end tag list
}# end ui

# server side ---------------
data_upload_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    
    # reactive variables-----------------------------------------------------------
    
    # download spreadsheet template ---------------------------------------
    output$download <- downloadHandler(
      filename <- "general_use_upload_template.xlsx",
      content = function(file) {
        file.copy("./upload_template/general_use_upload_template_v6.xlsx", file)
      }
    )
    
    # outputs ---------------------------------------------------
    output$contents <- renderUI({

      if(is.null(rv$sheet_names)) return()
      
      withProgress(message = 'Please wait',
                   detail = 'Reading in data ...',
                   value = 1/4, {
                     
                     # make sure it contains a DATA sheet. message in case it doesn't.
                     if("DATA" %in% rv$sheet_names == FALSE) {
                       showModal(modalDialog(
                         p("Your upload does not contain a sheet called 'DATA'. Please look at the template for the correct sheet names and required columns.")
                       ))
                     }
                     
                     # too many messages about the expected formats
                     rv$raw_df <- suppressWarnings(read_excel(input$upload$datapath,
                                                              sheet = "DATA",
                                                              guess_max = 50000))  # needs to be enough
                     
                     # get the column names
                     column_names <- colnames(rv$raw_df)
                     
                     # progress
                     incProgress(2/4, 
                                detail = "Checking duplicates ...")
                     
                     # check whether it has all the columns -- from app_datasets.R
                     rv$has_all_cols <- all(wanted_column_names %in% column_names)
                     
                     # check for non duplicate units
                     rv$has_dup_units <- rv$raw_df |> 
                       count(CAS_RN, REPORT_RESULT_UNIT) |> 
                       count(CAS_RN) |> 
                       filter(n > 1)
                     
                     # these are the duplicate ones
                     rv$duplicate_units_by_cas_rn <- rv$raw_df |> 
                       filter(CAS_RN %in% rv$has_dup_units$CAS_RN) |> 
                       count(CAS_RN, CHEMICAL_NAME, REPORT_RESULT_UNIT)
                     
                     # make sure it contains a DATA sheet. message in case it doesn't.
                     if(nrow(rv$has_dup_units) > 0) {
                       ns <- session$ns
                       
                       showModal(modalDialog(
                         p("Your 'DATA' sheet contains chemicals with more than one distinct 'REPORT_RESULT_UNIT' per chemical. ",
                           "Each CAS_RN should have the same REPORT_RESULT_UNIT."),
                         dataTableOutput(ns("duplicate_units_by_cas_rn"))
                       ))
                     }
                     
                     # progress
                     incProgress(3/4, 
                                 detail = "Checking other sheets ...")
                     
                     # raw criteria initial read
                     raw_criteria_initial <- suppressWarnings(read_excel(input$upload$datapath,
                                                                         sheet = "CRITERIA",
                                                                         guess_max = 50)) |> 
                       mutate(CAS_RN = as.character(CAS_RN),
                              CHEMICAL_NAME = as.character(CHEMICAL_NAME),
                              CRITERIA_UNIT = as.character(CRITERIA_UNIT))
                     
                     # see what columns there are
                     raw_criteria_initial_cols <-  colnames(raw_criteria_initial)
                     
                     # see what the criteria value column is called
                     if("CRITERIA_VALUE" %in% colnames(raw_criteria_initial)){
                       # then it is new. rename it so that it works with old functions
                       raw_criteria_initial2 <- raw_criteria_initial |> 
                         mutate(ACTION_LEVEL_VALUE = as.numeric(CRITERIA_VALUE))
                     } else {
                       # it is old, change to numeric anyway
                       raw_criteria_initial2 <- raw_criteria_initial |> 
                         mutate(ACTION_LEVEL_VALUE = as.numeric(ACTION_LEVEL_VALUE))
                     }
                     
                     # then check if the name of the criteria is defined
                     if("CRITERIA_TEXT" %in% colnames(raw_criteria_initial)){
                       rv$raw_criteria_df <- raw_criteria_initial2 |> 
                         # formatting in case empty as above
                         mutate(CRITERIA_TEXT = as.character(CRITERIA_TEXT)) |> 
                         select(CAS_RN, CHEMICAL_NAME, ACTION_LEVEL_VALUE, CRITERIA_UNIT, CRITERIA_TEXT) |> 
                         distinct()
                     } else {
                       # where the column is not in the sheet
                       rv$raw_criteria_df <- raw_criteria_initial2 |> 
                         # just call the criteria Standard
                         mutate(CRITERIA_TEXT = "Standard") |> 
                         select(CAS_RN, CHEMICAL_NAME, ACTION_LEVEL_VALUE, CRITERIA_UNIT, CRITERIA_TEXT) |> 
                         distinct()
                     }
                     
                     # check CAS_RN with multiple CHEMICAL_NAME
                     rv$cas_rn_with_multiple_chem_names <- rv$raw_df |> 
                       distinct(CAS_RN, CHEMICAL_NAME) |> 
                       add_count(CAS_RN) |> 
                       filter(n > 1) |> 
                       select(-n)
                     
                     # make sure it contains a DATA sheet. message in case it doesn't.
                     if(nrow(rv$cas_rn_with_multiple_chem_names) > 0) {
                       ns <- session$ns
                       
                       showModal(modalDialog(
                         p("Your 'DATA' sheet contains one or more CAS_RN assigned to more than one CHEMICAL_NAME. ",
                           "Each CAS_RN should only match with one CHEMICAL_NAME. See below."),
                         dataTableOutput(ns("cas_rn_multiple_chem_names"))
                       ))
                     }
                     
                     # check CHEMICAL_NAME with more than one CAS_RN
                     rv$chem_name_with_multiple_cas_rn <- rv$raw_df |> 
                       distinct(CAS_RN, CHEMICAL_NAME) |> 
                       add_count(CHEMICAL_NAME) |> 
                       filter(n > 1) |> 
                       select(-n)
                     
                     # make sure it contains a DATA sheet. message in case it doesn't.
                     if(nrow(rv$chem_name_with_multiple_cas_rn) > 0) {
                       ns <- session$ns
                       
                       showModal(modalDialog(
                         p("Your 'DATA' sheet contains one or more CHEMICAL_NAME assigned to more than one CAS_RN. ",
                           "Each CHEMICAL_NAME should only match with one CAS_RN. See below."),
                         dataTableOutput(ns("chem_name_multiple_cas_rn"))
                       ))
                     }
                     
                     # check chems with duplicate criteria
                     rv$chem_crit_check <- rv$raw_criteria_df |> 
                       count(CAS_RN, CHEMICAL_NAME) |> 
                       filter(n > 1)
                     
                     # the chems with dup criteria
                     rv$chem_crit_dups <- rv$raw_criteria_df |> 
                       filter(CAS_RN %in% rv$chem_crit_check$CAS_RN)
                     
                     # make sure it contains a DATA sheet. message in case it doesn't.
                     if(nrow(rv$chem_crit_check) > 0) {
                       ns <- session$ns
                       
                       showModal(modalDialog(
                         p("Your 'CRITERIA' sheet contains multiple criteria for at least one chemical. ",
                           "There should only be one criteria per chemical. See below."),
                         dataTableOutput(ns("chem_crit_dups"))
                       ))
                     }
                     
                     # the annotation lines df - which might be empty
                     if("ANNOTATION_LINES" %in% rv$sheet_names){
                       print("has annotiation line data")
                       rv$annotation_df <- suppressWarnings(read_excel(input$upload$datapath,
                                                                       sheet = "ANNOTATION_LINES",
                                                                       guess_max = 50)) |> 
                         mutate(DATE = as_date(DATE)) |> 
                         select(DATE, LABEL, LINETYPE) |> 
                         drop_na()
                     } else {
                       showModal(modalDialog(
                         p("The 'ANNOTATION_LINES' is missing. ")
                       ))
                     }
                     
                     # the well groups df - which might be empty
                     if("WELL_GROUPS" %in% rv$sheet_names){
                       print("has well groups")
                       rv$well_groups_df <- suppressWarnings(read_excel(input$upload$datapath,
                                                                        sheet = "WELL_GROUPS",
                                                                        guess_max = 50)) |> 
                         select(SYS_LOC_CODE, LOC_GROUP_CODE) |> 
                         # remove empty
                         filter(!is.na(LOC_GROUP_CODE))
                     } else {
                       showModal(modalDialog(
                         p("The 'WELL_GROUPS' is missing. ")
                       ))
                       rv$well_groups_df <- NULL
                     }
                     
                     # the analyte_groups df - which might be empty
                     if("ANALYTE_GROUPS" %in% rv$sheet_names){
                       print("has analyte groups")
                       rv$analyte_groups_df <- suppressWarnings(read_excel(input$upload$datapath,
                                                                           sheet = "ANALYTE_GROUPS",
                                                                           guess_max = 50)) |> 
                         select(ANALYTE_GROUP, CAS_RN, CHEMICAL_NAME) |> 
                         # remove empty
                         filter(!is.na(CAS_RN))
                       
                       # check if VOCs in analyte groups
                       if("VOCs" %in% unique(rv$analyte_groups_df$ANALYTE_GROUP)){
                         # the voc group
                         rv$voc_analyte_group <- rv$analyte_groups_df |> 
                           filter(ANALYTE_GROUP == "VOCs")
                         
                         # ones without a molecular weight - probably incorrectly entered
                         rv$voc_no_molecular_weight <- rv$voc_analyte_group |> 
                           filter(!CAS_RN %in% voc_molecular_weights$CAS_RN)
                       } 
                       
                     } else {
                       showModal(modalDialog(
                         p("The 'ANALYTE_GROUPS' is missing. ")
                       ))
                       
                       rv$analyte_groups_df <- NULL
                     }
                     
                     # if there are VOCs that don't have molecular weights
                     if(isTruthy(rv$voc_no_molecular_weight)) {
                       if(nrow(rv$voc_no_molecular_weight) > 0){
                         ns <- session$ns
                         
                         showModal(modalDialog(
                           p("Your 'ANALYTE_GROUP' sheet contains at least one analyte in the 'VOC' group that is not in our list. 
              Please check the analyte and contact the app authors if this needs to be fixed."),
                           dataTableOutput(ns("voc_no_molecular_weight"))
                         ))
                       }
                     }
                     
                     # check the sheets
                     if(isTruthy(rv$well_groups_df) &
                        isTruthy(rv$annotation_df) &
                        isTruthy(rv$analyte_groups_df)){
                       
                       # this is the text that is shown as part of the output - could add to this
                       if(rv$has_all_cols == TRUE){
                         # has the right columns, assign text, but proceed to next check
                         text <- p(HTML("<span style='color:blue'><b>Click the button below to run the app with your dataset</b></span>"))
                         
                         # check for multiple criteria
                         if(nrow(rv$chem_crit_check) > 0) {
                           # has multiple criteria
                           text <- p(HTML("<span style='color:red'><b>Stop! Your CRITERIA sheet contains multiple criteria for at least one chemical</b></span>"))
                         }
                         
                         # check for duplicate units
                         if(nrow(rv$has_dup_units) > 0){
                           # has duplicate units
                           text <- p(HTML("<span style='color:red'><b>Stop! Your DATA contains chemical(s) with multiple units</b></span>"))
                         }
                         
                       } else {
                         # does not contain the right columns
                         text <- p(HTML("<span style='color:red'><b>Stop! Your DATA sheet does not contain the right columns</b></span>"))
                       }
          
                     } else {
                       text <- p(HTML("<span style='color:red'><b>Not all the sheets are in the Excel file</b></span>"))
                       
                     }
                     
                     # the output
                     return(text)
                     
                   }) # end progress
      
    })
    
    # tables ------------------------------------------------------------
    # these go in to the modalDialog() boxes as additional info
    output$duplicate_units_by_cas_rn <- renderDataTable(datatable(rv$duplicate_units_by_cas_rn))
    
    output$chem_crit_dups <- renderDataTable(datatable(rv$chem_crit_dups))
    
    output$cas_rn_multiple_chem_names <- renderDataTable(datatable(rv$cas_rn_with_multiple_chem_names))
    
    output$chem_name_multiple_cas_rn <- renderDataTable(datatable(rv$chem_name_with_multiple_cas_rn))
    
    output$voc_no_molecular_weight <- renderDataTable(datatable(rv$voc_no_molecular_weight))
    
    # dynamic inputs -----------------------------------------------------
    
    output$d_load <- renderUI({
      req(rv$has_all_cols)
      ns <- session$ns
      # only show the button if the right cols are there 
      # and the criteria has is not duplicated
      # and all the sheets are there
      if(isTruthy(rv$has_all_cols) & 
         nrow(rv$chem_crit_check) == 0 & 
         nrow(rv$has_dup_units) == 0 &
         isTruthy(rv$well_groups_df) &
         isTruthy(rv$annotation_df) &
         isTruthy(rv$analyte_groups_df)){
        actionButton(ns("load"),
                     "Load data")
      } else {
        NULL
      }
      
    })
    
    # observers -----------------------------------------------------------------------
    
    # observe the file upload
    observeEvent(input$upload$datapath, {
      
      rv$sheet_names <- excel_sheets(input$upload$datapath)
      
    })

    #observe the load button
    observeEvent(input$load, {
      
      withProgress(message = 'Please wait',
                   detail = 'Creating interim tables ...',
                   value = 1/4, {
                     
                     # screening dataset - function from data_wrangling_functions.R
                     rv$screening_df <- initial_clean(rv$raw_df, wanted_column_names)
                     print("created screening_df")
                     
                     # clean dataset 
                     rv$clean_df <- rv$screening_df |>  
                       highest_filter()  # see ./functions/data_wrangling_functions.R
                     print("created clean_df")
                     
                     # values from the clean dataset
                     rv$wells <- unique(rv$clean_df$SYS_LOC_CODE)
                     rv$chems <- unique(rv$clean_df$CHEMICAL_NAME)
                     rv$date_range <- c(min(rv$clean_df$SAMPLE_DATE), max(rv$clean_df$SAMPLE_DATE))
                     
                     # other datasets
                     rv$cas_rn_to_chem_name <- cas_rn_for_chemname_graph_groups(rv$clean_df)
                     
                     
                     # progress
                     incProgress(2/4, 
                                 detail = "Checking chemical groupings and criteria ...")
                     
                     rv$graph_groupings <- create_graph_groupings(rv$cas_rn_to_chem_name, cas_rn_graph_groups, cas_rn_analyte_short)
                     
                     rv$graph_groupings_list <- c(
                       # the original ones
                       rv$graph_groupings %>% 
                         split(.$group) %>% 
                         map(~.x %>% pull(CHEMICAL_NAME)),
                       # the additional ones
                       rv$graph_groupings %>% 
                         split(.$group2) %>% 
                         map(~.x %>% pull(CHEMICAL_NAME)))
                     
                     print("created graph_groupings_list")
                     
                     # dataset for coc geochem
                     rv$gchem_df <- rv$clean_df |> 
                       left_join(rv$graph_groupings, by = c("CAS_RN", "CHEMICAL_NAME")) |>  
                       filter(FRACTION != "D" | is.na(FRACTION)) # keep T or N
                     
                     print("created gchem_df")
                     
                     # the chems
                     rv$inorgs_chem_order <- create_inorg_vector(rv$clean_df, inorg_chems_cas_rn)
                     
                     # a filtered version for the inorganic graphs
                     rv$inorg_df <- create_inorg_df(rv$clean_df, rv$inorgs_chem_order)
                     
                     print("created inorgs_df")
                     
                     # criteria - might need to convert the mu values
                     rv$criteria_df <- rv$raw_criteria_df %>% 
                       filter(!is.na(ACTION_LEVEL_VALUE)) %>% 
                       distinct(CAS_RN, CHEMICAL_NAME, ACTION_LEVEL_VALUE, CRITERIA_UNIT, CRITERIA_TEXT) %>% 
                       # rename to match the old code to make it easier
                       rename(action_level = ACTION_LEVEL_VALUE) %>% 
                       mutate(action_level_code = "Standard", # this should change, but issues downstream
                              MATRIX_CODE = "WG")
                     
                     print("created criteria_df")
                     
                     # progress
                     incProgress(3/4, 
                                 detail = "Checking initial values ...")
                     
                     # initial well_group if it exists
                     if(isTruthy(rv$well_groups_df) && nrow(rv$well_groups_df) > 0){
                       rv$well_group_initial <- unique(rv$well_groups_df$LOC_GROUP_CODE)[1]
                     } else {
                       rv$well_group_initial <- NULL
                     }
                     
                     # initial well selections if the well group exists
                     if(isTruthy(rv$well_groups_df) && nrow(rv$well_groups_df) > 0){
                       rv$wells_selected_initial <- rv$well_groups_df |> 
                         filter(LOC_GROUP_CODE == rv$well_group_initial) |> 
                         pull(SYS_LOC_CODE)
                     } else {
                       # whatever the first one is
                       rv$wells_selected_initial <- unique(rv$screening_df$SYS_LOC_CODE)[1]
                     }
                     
                     # initial analyte group if it exists
                     if(isTruthy(rv$analyte_groups_df) && nrow(rv$analyte_groups_df) > 0){
                       rv$analyte_group_initial <- unique(rv$analyte_groups_df$ANALYTE_GROUP)[1]
                     } else {
                       rv$analyte_group_initial <- NULL
                     }
                     
                     # initial analyte selections if the analyte group exists - These are CAS_RN
                     if(isTruthy(rv$analyte_groups_df) && nrow(rv$analyte_groups_df) > 0){
                       rv$analytes_selected_initial <- rv$analyte_groups_df |> 
                         filter(ANALYTE_GROUP == rv$analyte_group_initial) |> 
                         pull(CAS_RN)
                     } else {
                       # the default subset ones
                       rv$analytes_selected_initial <- default_subset_analytes
                     }
                     
                   }) # end progress
      
      

    }) # end observe
    
  }) # end module
} # end server