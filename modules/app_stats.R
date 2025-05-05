#  ui side ----------------------
app_stats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        h1("Application Statistics"),
        uiOutput(ns("d_well_groups")),
        uiOutput(ns("d_wells")),
        uiOutput(ns("d_analyte_groups")),
        uiOutput(ns("d_chems")),
        radioButtons(ns("substitute_vals"),
                     label = "Use actual values or substituted values for Mann-Kendall trend test",
                     choices = c("Use actual values" = "AV",
                                 "Substituted values" = "SV"),
                     selected = "AV",
                     inline = TRUE),
        p("If 'Substituted values' is chosen, then non-detect values will be substituted with the highest 'U' qualified value.
        Additionally, detected results lower than the highest 'U' qualified value are also substitued for the 'U' detection limit value"),
        br(),
        sliderInput(ns("conf_level"),
                    label = "Confidence level for Mann Kendall trend test",
                    min = 0.8,
                    max = 0.99,
                    step = 0.05,
                    ticks = FALSE,
                    value = 0.95,
                    width = "50%"),
        uiOutput(ns("d_dates")), # from server side
        
    ),
    box(width = 12,
        HTML(glue("<h3>General notes:</h3><ul>
                  <li>Values calculated here may not match those generated using ProUCL</li>
                  <li>Dissolved values from the inorganic analytes are excluded from the analysis.</li>
                  </ul>")),
        br(),
        
        HTML(glue("<h3>Trend logic:</h3><ul>
                  <li>A Mann-Kendall test is run on each Chemical/Well combination</li>
                  <li>The trend is then calculated based the information in pp. 276-277 of the ProUCL Technical Guide, v5.20 </li>
                  <li>Trends are only calculated where there are six or more values, and fewer than 50 per cent non-detects</li>
                  <li>If there are more than 40 observations in the dataset, trends are calculated using only the most recent 40, as per the GSI Toolkit.</li>
                  </ul>")),
        br(),
        HTML(glue("<h3>LCL/UCL logic:</h3><ul>
                  <li>If all values for a Chemical/Well combination are detected (all uncensored),
                  then LCL and UCL calculated from an ordinary nonparametric bootstrap</li>
                  <li>If there are at least two distinct, detected values, then the LCL and UCL and calculated via a bootstrapped Kaplan-Meier method</li>
                  <li>Otherwise the LCL and UCL are not calculated</li>
                  </ul>")),
        br(),
        actionButton(ns("button"), "Generate table"),
        br(),
        uiOutput(ns("d_date_text")),
        uiOutput(ns("d_download")),
        br(),
        DTOutput(ns("mk_conf_pred_table"))
        
    )
    
  ) # end tag list
}# end ui

#  server side ---------------
app_stats_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables-----------------------------------------------------------
    r_wells <- reactive(input$wells)
    r_well_groups <- reactive(input$well_groups)
    r_analyte_groups <- reactive(input$analyte_groups)
    r_substitute_vals <- reactive(input$substitute_vals)
    
    # reactive datasets --------------------------
    
    
    # outputs ---------------------------------------------------
    output$mk_conf_pred_table <- renderDataTable({
      req(rv$mk_conf_pred_list)
      req(rv$mk_conf_pred_table)

      if(length(rv$mk_conf_pred_list) > 0){
        rv$mk_conf_pred_table %>%
          datatable(options = list(scrollX = TRUE))
      }

    })
    
    output$text <- renderUI({
      
      if(input$button == 0){
        p("You can download the table once it has been generated.")
      }
    })
    
    # dynamic inputs -----------------------------------------------------
    
    # date range filter
    output$d_dates <- renderUI({
      
      req(rv$clean_df)
      ns <- session$ns
      
      dateRangeInput(ns("dates"),
                     label = "Date range:",
                     start = rv$date_range[1],
                     end = rv$date_range[2],
                     min = rv$date_range[1],
                     max = rv$date_range[2],
                     width = "50%")
    })
    
    # well group selection 
    output$d_well_groups <- renderUI({
      req(rv$well_group_initial)
      
      ns <- session$ns
      
      if(isTruthy(rv$well_group_initial)){
        # if there are well groups
        tagList(
          selectInput(ns("well_groups"),
                      label = "Select well group",
                      choices = unique(rv$well_groups_df$LOC_GROUP_CODE),
                      selected = rv$well_group_initial,
                      width = "50%")
        )
        # this is just the initial selection. will need to be observed and updated
      }
      # otherwise nothing
      
    })
    
    # well selection
    output$d_wells <- renderUI({
      req(rv$wells)
      
      ns <- session$ns
      
      selectizeInput(ns("wells"),
                     label = "Select wells",
                     choices = str_sort(rv$wells, numeric = TRUE),
                     selected = rv$wells_selected_initial,
                     multiple = TRUE)
    })
    
    # analyte group selection 
    output$d_analyte_groups <- renderUI({
      req(rv$analyte_group_initial)
      
      ns <- session$ns
      
      if(isTruthy(rv$analyte_group_initial)){
        # if there are well groups
        tagList(
          selectInput(ns("analyte_groups"),
                      label = "Select analyte group",
                      choices = c(unique(rv$analyte_groups_df$ANALYTE_GROUP),
                                  "All chemicals"),
                      selected = rv$analyte_group_initial,
                      width = "50%")
        )
        # this is just the initial selection. will need to be observed and updated
      }
      # otherwise nothing
      
    })
    
    
    # chem selection
    output$d_chems <- renderUI({
      req(rv$chems)
      
      ns <- session$ns
      
      selectizeInput(ns("chems"),
                     label = "Select chemicals",
                     choices = str_sort(rv$chems, numeric = TRUE),
                     selected = rv$clean_df %>%
                       filter(CAS_RN %in% rv$analytes_selected_initial) %>%
                       distinct(CHEMICAL_NAME) %>%
                       pull(CHEMICAL_NAME) %>%
                       sort(),
                     multiple = TRUE)
    })
    
    # download button
    output$d_download <- renderUI({
      req(rv$mk_conf_pred_table)
      
      ns <- session$ns
      
      if(isTruthy(rv$mk_conf_pred_table)){
        if(length(rv$mk_conf_pred_list) > 0){
          downloadButton(ns("download"))
        }
      } else {
        NULL
      }
      
    })
    
    output$d_date_text <- renderUI({
      if(is.null(rv$mk_conf_pred_list)){
        return()
      } else if (length(rv$mk_conf_pred_list) == 0){
        HTML("<span style='color:red'>Not enough dates have been selected. Please select a larger date range.</span>")
      } else {
        return()
      }
      
    })
    
    
    # downloads -----------------------------------------------------
    output$download <- downloadHandler(
      filename = function(){
        glue("Stats_{format(Sys.Date(), '%Y_%m_%d')}.xlsx")
      },
      content = function(con) {
        
        # extra details
        wb_details <- tribble(~Item, ~Detail,
                              "Start Date", format(input$dates[1], "%d %b %Y"),
                              "End Date", format(input$dates[2], "%d %b %Y"),
                              "Mann Kendall Test confidence level", as.character(input$conf_level))
        
        # the header
        init_cols <- c("General", rep("", 14), # 15 columns
                       "Parameter estimation and Confidence limits", rep("", 11), # 12 columns
                       # no prediction limits!!!
                       "Trend analysis", rep("", 8))  # 9 columns (7 plus extra GSI)
        
        index_init_cols <- which(init_cols != "")
        
        top_row <- as_tibble(matrix(nrow = 0, ncol = length(init_cols)), .name_repair = ~ init_cols)
        
        end_row <- nrow(rv$mk_conf_pred_table) + 2 # makes the formatting work
        abbrevs_row <- end_row + 2 # where the abbreviations notes start
        
        # these were previously booleans, now yes, no with NC as well - the width for these
        boolean_cols_width <- which(colnames(rv$mk_conf_pred_table) %in% c("UCL > Standard",
                                                                     "LCL > Standard",
                                                                     "Latest Result > mean",
                                                                     "Latest Result > Standard"))
        
        # only these get formatted
        boolean_cols_formatting <- which(colnames(rv$mk_conf_pred_table) %in% c("UCL > Standard",
                                                                                "LCL > Standard",
                                                                                "Latest Result > Standard"))
        
        # everything except for these are auto col width and the boolean ones
        skinny_cols <- setdiff(which(!colnames(rv$mk_conf_pred_table) %in% 
                                       c("SYS_LOC_CODE", "Min Date", "Max Date", "Min detected", "Max detected",
                                         "Parameter est. method", "CL method", "ProUCL Method Trend Direction")),
                               boolean_cols_width)
        
        # manual override skinny - these didn't work in auto but override
        manual_skinny <- which(colnames(rv$mk_conf_pred_table) %in% c("RCRA regulated chemical", "LCL of the mean", "UCL of the mean",
                                                                      "UCL > Standard", "LCL > Standard", "Latest Result > mean",
                                                                      "N (background)", "ND (background)", "UPL method", "UPL conf",
                                                                      "UPL of background", "Latest Result > UPL of background",
                                                                      "TS Slope", "TS Intercept", "p-value", "Confidence Level %",
                                                                      "Dataset n"))
        
        # the column number of the 'Standard' column
        standard_col_num <- which(colnames(rv$mk_conf_pred_table) == "Standard")
        
        wb <- createWorkbook()
        modifyBaseFont(wb, fontSize = 10)
        addWorksheet(wb, "Sheet 1")
        writeData(wb, 1, top_row, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 1)
        # write the data in two parts
        # up to and including Standard gets written as an NA when is.na
        writeData(wb, 1, rv$mk_conf_pred_table[,1:standard_col_num], headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 2, keepNA = TRUE, na.string = "NA")
        # everything after standard gets NC when is.na
        writeData(wb, 1, rv$mk_conf_pred_table[,(standard_col_num + 1):length(colnames(rv$mk_conf_pred_table))],
                  headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 2, startCol = (standard_col_num + 1),
                  keepNA = TRUE, na.string = "NC")
        # formatting
        setColWidths(wb, 1, cols = skinny_cols, widths = "auto")
        setColWidths(wb, 1, cols = boolean_cols_width, widths = 6)
        setColWidths(wb, 1, cols = manual_skinny, widths = 10)
        mergeCells(wb, "Sheet 1", cols = index_init_cols[1]:(index_init_cols[2]-1), rows = 1)
        mergeCells(wb, "Sheet 1", cols = index_init_cols[2]:(index_init_cols[3]-1), rows = 1)
        # no prediction limit so the next one changed
        mergeCells(wb, "Sheet 1", cols = index_init_cols[3]:length(init_cols), rows = 1)
        freezePane(wb, "Sheet 1", firstActiveRow = 3, firstActiveCol = 4)
        # # conditional formatting for mann kendall
        # conditionalFormatting(wb, "Sheet 1",
        #                       type = "contains",
        #                       cols = which(str_detect(colnames(rv$mk_conf_pred_table), "Direction|Toolkit")),
        #                       rows = 1:end_row,
        #                       rule = "Increasing", 
        #                       style = increasing_trend)
        # conditionalFormatting(wb, "Sheet 1",
        #                       type = "contains",
        #                       cols = which(str_detect(colnames(rv$mk_conf_pred_table), "Direction|Toolkit")),
        #                       rows = 1:end_row,
        #                       rule = "Decreasing", 
        #                       style = decreasing_trend)
        # conditional formatting for booleans -- but they're not booleans anymore
        conditionalFormatting(wb, "Sheet 1",
                              type = "contains",
                              cols = boolean_cols_formatting,
                              rows = 1:end_row,
                              rule = "Yes",
                              style = increasing_trend)
        # add the text at the bottom 
        writeData(wb, "Sheet 1", app_stats_abbrevs, startCol = 1, startRow = abbrevs_row)
        # add the extra details
        addWorksheet(wb, "Details")
        writeData(wb, 2, wb_details, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 1)
        setColWidths(wb, 2, cols = c(1, 2), widths = "auto")
        # save
        saveWorkbook(wb, con)
      }
    )
    
    # observers -----------------------------------------------------------------------
    
    observeEvent(input$button,{
      
      if(isTruthy(r_well_groups())){
        # if there is a well group filter
        
        new_wells <- rv$well_groups_df |> 
          filter(SYS_LOC_CODE %in% r_wells()) |> 
          pull(SYS_LOC_CODE)
        
        initial_df <- rv$clean_df |> 
          filter(SYS_LOC_CODE %in% new_wells)
      } else {
        
        # no well group filter
        initial_df <- rv$clean_df
      }
      
      # data before splitting
      mk_data_all <- initial_df |> 
        # filter based on inputs
        filter(CHEMICAL_NAME %in% input$chems,
               SYS_LOC_CODE %in% input$wells,
               SAMPLE_DATE >= input$dates[1],
               SAMPLE_DATE <= input$dates[2],
               !is.na(REPORT_RESULT_UNIT))
      
      # check if the minimum value is positive
      min_value <- min(mk_data_all$REPORT_RESULT_VALUE)
      if(min_value > 0){
        
        # split data
        rv$mk_conf_pred_list <- mk_data_all |>  
          mk_group_filter_split_helper() 
        
        # if not empty create table
        if(length(rv$mk_conf_pred_list) > 0){
          
          withProgress(
            message ='Please wait',
            detail ='Creating table (1/3)',
            value = 0, {
              
              # most of the table
              mk_conf_pred1 <- map_df(rv$mk_conf_pred_list,
                                      ~envstats_mk_conf_pred(.x,
                                                             input$substitute_vals,
                                                             input$conf_level,
                                                             prog = 0.25/length(.x))) # slow
              
              # extra progress, probably redundant
              incProgress(1,
                          detail = "Combining and cleaning (3/3)") 
              
              # combining and cleaning
              rv$mk_conf_pred_table <- mk_conf_pred_end_helper(mk_conf_pred1,
                                                               bg_upl_df = NULL,
                                                               criteria_units_table = rv$criteria_df) 
              
            })
        }
      } else {
        # minimum value not positive
        showModal(modalDialog(
          p("Your dataset contains one or more nondetect result values of zero. ",
          "Replace these values with an appropriate detection limit to run the statistics module.")
        ))
        
      }


    })
    
    # observe the well group selection (if it exists)
    observeEvent(input$well_groups,{
      print(input$well_groups)
      
      new_wells <- rv$well_groups_df |>
        filter(LOC_GROUP_CODE == r_well_groups()) |>
        pull(SYS_LOC_CODE)
      
      # this is not namespaced!!!!
      updateSelectizeInput(session = session,
                           inputId = "wells",
                           selected = new_wells)
    })
    
    
    # observe the analyte group selection (if it exists)
    observeEvent(input$analyte_groups,{
      print(input$analyte_groups)
      
      if(input$analyte_groups == "All chemicals"){
        # all chemicals
        new_analytes <- rv$chems
      
      } else {
        # the ones in the analyte group
        new_analytes <- rv$analyte_groups_df |>
          filter(ANALYTE_GROUP == r_analyte_groups()) |>
          distinct(CAS_RN) |> 
          inner_join(rv$clean_df |> 
                       distinct(CAS_RN, CHEMICAL_NAME),
                     by = "CAS_RN") |> 
          arrange(CHEMICAL_NAME) |> 
          pull(CHEMICAL_NAME)
        
      }
      
      # this is not namespaced!!!!
      updateSelectizeInput(session = session,
                           inputId = "chems",
                           selected = new_analytes)
    })
    
  }) # end module
} # end server