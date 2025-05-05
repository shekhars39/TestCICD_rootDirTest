# ui side ----------------------
coc_geochem_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
        box(width = 12,
            h1("Chlorinated Ethenes and Geochemistry"),
            HTML("This module will generate time series graphs for chlorinated ethenes, daughter products, water quality parameters, and redox parameters in: </br>
                 <ol>
                 <li>interactive format by individual well, and</li>
                 <li>downloadable format by well group, for the selected date range</li>
                 </ol>
                 The graphs will also show a vertical line for the event entered into the data input file 'Annotation Line' fields.</br>"),
            br(),
            HTML("Filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
                 "When only the year is shown in the x-axis label, this represents the start of the year.</br>"),
            br(),
            uiOutput(ns("d_dates")), # from server side
            br(),
            # this is as the lines are shown by default on this page, but not other pages!
            # there is a constant_min_date in the function for well_cell_graphs
            radioButtons(ns("show_annotation_lines"),
                         label = "Show vertical annotation lines",
                         choices = c("Yes", "No"),
                         selected = "Yes",
                         inline = TRUE),
            radioButtons(ns("molar_conc_type"),
                         label = "Molar concencentration graph type",
                         choices = c("Stacked area" = "stacked_area",
                                     "Stacked bar" = "stacked_bar"),
                         selected = "stacked_area",
                         inline = TRUE),
            p("The stacked bar graph is more interactive, but may not be suitable for your datatset."),
            radioButtons(ns("cedp"),
                         label = "Format of chlorinated ethenes and daughter products graphs:",
                         choices = c("Two graphs: daughter products separate" = "faceted",
                                     "Single graph, all analytes" = "combined"),
                         selected = "faceted",
                         inline = TRUE),
            br(),
            h2("Generate word document output of graphs"),
            p("Download graphs for the selected wells in the well group."),
            uiOutput(ns("d_well_groups")),
            uiOutput(ns("d_wells_word")), # from server side
            uiOutput(ns("d_download")),
            br(),
            h2("Interactive charts"),
            uiOutput(ns("d_wells")), # from server side
            actionButton(ns("generate"), "Generate graphs"),
            uiOutput(ns("charts")) %>%
              withSpinner()
            )
      )

    ) # end page
    
  ) # end tag list
}# end ui

# server side ------------------------------------------------------------------------
coc_geochem_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables----------------------------------------------------------------
    r_well_groups <- reactive(input$well_groups)
    r_molar_conc_type <- reactive(input$molar_conc_type)
    
    # reactive_datasets----------------------------------------------------------

    
    # dynamic inputs ------------------------------------------------------------------

    # well selection
    output$d_wells <- renderUI({
      req(rv$wells)

      ns <- session$ns

      selectInput(ns("wells"),
                  label = "Select well to analyse",
                  choices = str_sort(rv$wells, numeric = TRUE),
                  selected = rv$wells_selected_initial,
                  width = '50%')

    })
    
    # date selection
    output$d_dates <- renderUI({
      
      req(rv$gchem_df)
      ns <- session$ns
      
      dateRangeInput(ns("dates"),
                     label = "Date range:",
                     start = rv$date_range[1],
                     end = rv$date_range[2],
                     min = rv$date_range[1],
                     max = rv$date_range[2],
                     width = '50%')
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
                      width = "50%"),
          p("This will change the selected wells. Individual wells can be added or removed from the selection.")
        )
        # this is just the initial selection. will need to be observed and updated
      }
      # otherwise nothing
      
    })
    
    # well selection for word doc
    output$d_wells_word <- renderUI({
      req(rv$wells)
      
      ns <- session$ns
      
      selectizeInput(ns("wells_word"),
                     label = "Select wells for word document",
                     choices = str_sort(rv$wells, numeric = TRUE),
                     selected = rv$wells_selected_initial,
                     multiple = TRUE)
    })

    # outputs -----------------------------------------------------------------------
    
    # uses intermediate outputs
    output$charts <- renderUI({
      req(rv$gchem_charts)

      ns <- session$ns

      tagList(
        h3("Water quality"),
        if(rv$wq_val == TRUE){
          girafeOutput(ns("wq"),
                       height = plotly_height_val) %>%
            withSpinner()
        } else {
          p("No data")
        },
        
        h3("TOC and Redox Parameters"),
        if(rv$tocr_val == TRUE){
          girafeOutput(ns("tocr"),
                       height = plotly_height_val) %>%
            withSpinner()
        } else {
          p("No data")
        },
        
        h3("Chlorinated Ethene and Daughter Product Mass"),
        if(rv$ce_norm_val == TRUE){
          girafeOutput(ns("ce_norm"),
                       height = plotly_height_val) %>%
            withSpinner()
        } else {
          p("No data")
        },
        if(rv$ce_log10_val == TRUE){
          girafeOutput(ns("ce_log10"),
                       height = plotly_height_val) %>%
            withSpinner()
        } else {
          p("No data")
        },
        
        h3("Chlorinated Ethene and Daughter Product Concentration"),
        p("Note: If you're not able to see the lines in the graph below, but there are lines in the graph above,
          perhaps change to a stacked area graph, remove the annotation line, or change the date filters."),
        uiOutput(ns("d_download2")),
        if(rv$ce_molar_val == TRUE){
          girafeOutput(ns("ce_molar"),
                       height = plotly_height_val) %>%
            withSpinner()
        } else {
          p("No data")
        },
        br()
      )

    })
    

    # observers -----------------------
    

    # observe wells and dates for chart
    event_trigger <- reactive({
      list(input$wells, input$dates)
    })

    observeEvent(input$generate, {

      # start date to show annotation lines even if before the miniumum date
      graph_min <- if_else(input$show_annotation_lines == "Yes",
                           floor_date(min(c(input$dates[1], min(rv$annotation_df$DATE))), "6 months"),
                           input$dates[1])

      # those are created in modules/data_upload.R
      rv$gchem_charts <- well_cell_graphs_ggg_v2(dataset = rv$gchem_df |> 
                                                   filter(SAMPLE_DATE >= input$dates[1],
                                                          SAMPLE_DATE <= input$dates[2]),
                                                 well_selected = input$wells,
                                                 scale_type = "normal",
                                                 graph_groupings_list = rv$graph_groupings_list, # added
                                                 graph_groupings = rv$graph_groupings, # added
                                                 criteria_dataset = rv$criteria_df,
                                                 constant_min_date = graph_min,
                                                 annotation_df = rv$annotation_df,
                                                 show_annotations = input$show_annotation_lines,
                                                 molar_conc_type = input$molar_conc_type,
                                                 chem_lookup = rv$cas_rn_to_chem_name,
                                                 is_ggiraph = TRUE,
                                                 cedp_type = input$cedp)
      
      # tables for download - note that this is a list of two outputs, interim and final
      rv$molar_conc_df <- get_molar_conc_df(dataset = rv$gchem_df |> 
                                              filter(SAMPLE_DATE >= input$dates[1],
                                                     SAMPLE_DATE <= input$dates[2]),
                                            analytes_selected = rv$graph_groupings_list$`Chlorinated Ethene and Daughter Product`,
                                            well_selected = input$wells,
                                            molar_mass_table = molecular_weights,
                                            factor_order_vector = chlor_eth_factor_order)
      
    })
    
    # observe to create intermediate outputs dependent on if there is data
    observe({
      
      if(is.null(rv$gchem_charts$wq)) {
        rv$wq_val <- FALSE
      } else {
        output$wq <- renderGirafe(rv$gchem_charts$wq)
        rv$wq_val <- TRUE
      }
      
      # print(glue("rv$wq_val: {rv$wq_val}"))
      
      if(is.null(rv$gchem_charts$tocr)) {
        rv$tocr_val <- FALSE
      } else {
        output$tocr <- renderGirafe(rv$gchem_charts$tocr)
        rv$tocr_val <- TRUE
      }
      
      # print(glue("rv$tocr_val: {rv$tocr_val}"))
      
      if(is.null(rv$gchem_charts$ce_norm)) {
        rv$ce_norm_val <- FALSE
      } else {
        output$ce_norm <- renderGirafe(rv$gchem_charts$ce_norm)
        rv$ce_norm_val <- TRUE
      }
      
      # print(glue("rv$ce_norm_val: {rv$ce_norm_val}"))
      
      if(is.null(rv$gchem_charts$ce_log10)) {
        rv$ce_log10_val <- FALSE
      } else {
        output$ce_log10 <- renderGirafe(rv$gchem_charts$ce_log10)
        rv$ce_log10_val <- TRUE
      }
      
      # print(glue("rv$ce_log10_val: {rv$ce_log10_val}"))
      
      if(is.null(rv$gchem_charts$ce_molar)) {
        rv$ce_molar_val <- FALSE
      } else {
        output$ce_molar <- renderGirafe(rv$gchem_charts$ce_molar)
        rv$ce_molar_val <- TRUE
      }

      # print(glue("rv$ce_molar_val: {rv$ce_molar_val}"))
    })
    
    # observe wells for word document
    observeEvent(input$wells_word,{
      
      rv$wells_word <- input$wells_word
    })
    
    
    # observe the well group selection (if it exists)
    observeEvent(input$well_groups,{
      print(input$well_groups)
      
      new_wells <- rv$well_groups_df |> 
        filter(LOC_GROUP_CODE == r_well_groups()) |> 
        pull(SYS_LOC_CODE)
      
      # this is not namespaced!!!!
      updateSelectizeInput(session = session,
                           inputId = "wells_word",
                           selected = new_wells)
    })
    
    
    # downloads -----------------------------------------------------
    # download button
    output$d_download <- renderUI({
      ns <- session$ns
      downloadButton(ns("download"))
    })
    
    output$download <- downloadHandler(
      filename = function(){
        glue("CoC_GChem_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        
        # data filtered for well group and dates
        df <- rv$gchem_df %>% 
          filter(SAMPLE_DATE >= input$dates[1],
                 SAMPLE_DATE <= input$dates[2],
                 SYS_LOC_CODE %in% input$wells_word)
        
        # only the wells selected for the word document
        wells <- str_sort(unique(df$SYS_LOC_CODE)[unique(df$SYS_LOC_CODE) %in% rv$wells_word],
                          numeric = TRUE)
        
        # graphs
        withProgress(
          message ='Please wait',
          detail ='Generating graphs ...',
          value = 0, {
            
            # graph start date
            # start date to show annotation lines even if before the miniumum date
            graph_min <- if_else(input$show_annotation_lines == "Yes",
                                 floor_date(min(c(input$dates[1], min(rv$annotation_df$DATE))), "6 months"),
                                 input$dates[1])

            rv$word_graphs_ce <- map(wells, ~well_cell_graphs_ggg_v2(dataset = df |> 
                                                                       filter(SAMPLE_DATE >= input$dates[1],
                                                                              SAMPLE_DATE <= input$dates[2]),
                                                                     well_selected = .x,
                                                                     scale_type = "normal",
                                                                     graph_groupings_list = rv$graph_groupings_list, 
                                                                     graph_groupings = rv$graph_groupings, 
                                                                     criteria_dataset = rv$criteria_df,
                                                                     constant_min_date = graph_min,
                                                                     annotation_df = rv$annotation_df,
                                                                     show_annotations = input$show_annotation_lines,
                                                                     molar_conc_type = input$molar_conc_type,
                                                                     chem_lookup = rv$cas_rn_to_chem_name,
                                                                     is_ggiraph = FALSE,
                                                                     cedp_type = input$cedp,
                                                                     prog = 0.5/length(.x)))

          })
        
        # word doc
        withProgress(message ='Please wait',
                     detail ='Rendering document ...',
                     value = 0, {

          tempReport <- file.path(tempdir(), "ce_gchem_tsp.rmd")
          file.copy("markdown_templates/ce_gchem_tsp.Rmd", tempReport, overwrite = TRUE)
          tempTemplate <- file.path(tempdir(), "blank.docx")
          file.copy(word_template_file, tempTemplate, overwrite = TRUE)
          
          params <- list(word_graphs = rv$word_graphs_ce,
                         wells = wells,
                         date_range = input$dates,
                         rendered_by_shiny = TRUE)
          
          rmarkdown::render(tempReport,
                            output_file = file,
                            params = params,
                            envir = new.env(parent = globalenv())
          )
          
          # to stop the downloads timing out
          # edit the /etc/shiny-server/shiny-server.conf file
          # https://stackoverflow.com/questions/44428211/network-error-triggering-the-download-reportreport-generation-action-in-server
          # https://stackoverflow.com/questions/51520166/updating-shiny-server-config-to-change-timeout-error
          
        }) # end with progress
        
      } # end content
    ) # end download
    
    
    # download button for molar conc table
    output$d_download2 <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$molar_conc_df)){
        downloadButton(ns("download2"),
                       label = "Download graph data")
      }
    })
    
    # output for molar conc table
    output$download2 <- downloadHandler(
      filename = glue("molar_conc_table.xlsx"),
      content = function(file) {
        
        sheets <- list("molar_conc" = rv$molar_conc_df)
        
        openxlsx::write.xlsx(sheets, file, asTable = TRUE, colWidths = "auto")
        
      } # end content
    ) # end download

  }) # end module
} # end server