#  ui side ----------------------
screening_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Data Screening Tables"),
                   HTML("This module will generate: </br>",
                        "<ol>",
                        "<li>a formatted Excel table that screens data results against numerical criteria loaded with the data input file, and</li>",
                        "<li>a summary of all chemicals and locations that exceed the standards, for the date range, wells, and chemicals selected below</li>",
                        "</ol>",
                        "Depending on the quantity of data, it may take several minutes to generate. ", 
                        "This module is best used to screen data from a most recent monitoring event."),
                   br(),
                   uiOutput(ns("d_dates")),
                   uiOutput(ns("d_well_groups")),
                   uiOutput(ns("d_wells")),
                   uiOutput(ns("d_analyte_groups")),
                   uiOutput(ns("d_chems")),
                   br(),
                   actionButton(ns("generate"), "Generate dataset"),
                   p("Once the dataset is generated, a download button will appear here and you will be able to download the file."),
                   br(),
                   uiOutput(ns("d_download"))
               ) # end box
      ) # end fluid row
    ) # end fluid page
  ) # end tag list
} # end ui

#  server side ---------------
screening_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {

    # reactive variables-----------------------------------------------------------
    r_wells <- reactive(input$wells)
    r_chems <- reactive(input$chems)
    r_well_groups <- reactive(input$well_groups)
    r_analyte_groups <- reactive(input$analyte_groups)

    # reactive datasets --------------------------


    # outputs ---------------------------------------------------


    # dynamic inputs -----------------------------------------------------

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
                      width = "50%"),
          p("This will change the selected wells. Individual wells can be added or removed from the selection.")
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
                     label = "Select wells for screening summary",
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
                      choices = c("Chemicals with criteria",
                                  unique(rv$analyte_groups_df$ANALYTE_GROUP)),
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
      
      # default selected chems
      if(nrow(rv$raw_criteria_df) > 0){
        selected_cas_rn <- rv$raw_criteria_df$CAS_RN
      } else {
        selected_cas_rn <- default_subset_analytes
      }
      
      selectizeInput(ns("chems"),
                     label = "Select chemicals for screening summary",
                     choices = str_sort(rv$chems, numeric = TRUE),
                     selected = rv$clean_df %>%
                       filter(CAS_RN %in% selected_cas_rn) %>%
                       distinct(CHEMICAL_NAME) %>%
                       pull(CHEMICAL_NAME) %>%
                       sort(),
                     multiple = TRUE)
    })

    output$d_dates <- renderUI({

      req(rv$screening_df)
      ns <- session$ns

      dateRangeInput(ns("dates"),
                     label = "Date range:",
                     start = ymd(rv$date_range[2] - days(30)),
                     end = rv$date_range[2],
                     min = rv$date_range[1],
                     max = rv$date_range[2],
                     width = '50%')

    })

    # download button
    output$d_download <- renderUI({
      req(rv$groundwater_data)
      req(rv$show_screen_download)

      ns <- session$ns

      if(isTruthy(rv$groundwater_data) & isTruthy(rv$show_screen_download)){
        downloadButton(ns("download"))
      } else {
        NULL
      }

    })


    # observers -----------------------------------------------------------------------


    observeEvent(input$generate,{

      # combines the criteria and the data
      rv$scrn_groundwater <- screening_report_datasets(rv$screening_df %>%
                                                         filter(CHEMICAL_NAME %in% r_chems(),
                                                                SYS_LOC_CODE %in% r_wells(),
                                                                !is.na(REPORT_RESULT_UNIT)),
                                                       rv$criteria_df,
                                                       input$dates[1],
                                                       input$dates[2])
      
      # check that the SYS_SAMPLE_CODES are unique
      num_sample_dates <- rv$scrn_groundwater$chem_criteria |> 
        distinct(SYS_LOC_CODE, SYS_SAMPLE_CODE, SAMPLE_DATE) |> 
        nrow()
      
      num_sample_codes <- rv$scrn_groundwater$chem_criteria |> 
        distinct(SYS_LOC_CODE, SYS_SAMPLE_CODE) |> 
        nrow()
      
      if(num_sample_dates == num_sample_codes){
        # if they're the same
        rv$groundwater_data <- rv$scrn_groundwater$chem_criteria %>%
          arrange(SYS_LOC_CODE, SAMPLE_DATE, CHEMICAL_NAME)
        
        if(nrow(rv$groundwater_data) > 0){
          
          # generate the excel file formatting if there is data
          withProgress(
            message ='Please wait',
            detail ='Generating file ...',
            value = 0, {
              rv$wb <- screening_with_qualifier(rv$groundwater_data)
            })
          
          rv$show_screen_download <- TRUE
          
        } else {
          # not enough data
          rv$show_screen_download <- FALSE
          
          showModal(modalDialog(
            p("Not enough data for the well group in the selected period.")
          ))
        }
        
      } else {
        # there are duplicates
        showModal(modalDialog(
          p("There are duplicate SYS_SAMPLE_CODES in this dataset. You won't be able to run Data Screening Tables module.")
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
      
      if(input$analyte_groups == "Chemicals with criteria"){
        # all chemicals with criteria
        new_analytes <- rv$clean_df %>%
          filter(CAS_RN %in% rv$raw_criteria_df$CAS_RN) %>%
          distinct(CHEMICAL_NAME) %>%
          pull(CHEMICAL_NAME) %>%
          sort()
        
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

    # downloads -----------------------------------------------------
    output$download <- downloadHandler(
      filename = function(){

        glue("ScreeningSummary_{format(Sys.Date(), '%Y_%m_%d')}.xlsx")

      },
      content = function(con) {

        # extra details about what is being filtered for
        wb_details <- tribble(~Item, ~Detail,
                              "Wells", r_wells(),
                              "Chemicals", r_chems(),
                              "Start Date", format(input$dates[1], "%d %b %Y"),
                              "End Date", format(input$dates[2], "%d %b %Y"))

        addWorksheet(rv$wb, "Details")
        writeData(rv$wb, "Details", wb_details, headerStyle = lft_header_style, borders = "all", borderStyle = "thin", startRow = 1)
        saveWorkbook(rv$wb, con)
      }
    )

  }) # end module
} # end server