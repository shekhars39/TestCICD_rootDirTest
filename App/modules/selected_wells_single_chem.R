# ui side ----------------------
selected_wells_chems_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Selected Wells"),
                   HTML("This module will generate time series graphs for the selected analyte for locations from the selected date range in both single and faceted formats. ",
                        "The graphs show a vertical line for the event entered into the data input file 'Annotation Line' fields. ",
                        "Filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
                        "When only the year is shown in the x-axis label, this represents the start of the year. ",
                        "</br></br>",
                        "There are user options for y-axis scale, analyte fraction type, and color palettes"),
                   br(),
                   uiOutput(ns("d_dates")), # from server side
                   br(),
                   uiOutput(ns("d_analyte")), # server side
                   br(),
                   radioButtons(ns("show_annotation_lines"),
                                label = "Show vertical annotation lines",
                                choices = c("Yes", "No"),
                                selected = "Yes",
                                inline = TRUE),
                   uiOutput(ns("d_well_groups")),
                   uiOutput(ns("d_wells")), # from server side
                   uiOutput(ns("well_length_text")),
                   br(),
                   radioButtons(ns("scale"),
                                label = "Y-axis scale",
                                choices = c("normal", "log10"),
                                selected = "normal",
                                inline = TRUE),
                   br(),
                   radioButtons(ns("frac"),
                                label = "Fraction",
                                choices = c("T (Total or Normal)" = "T",
                                            "D (Dissolved)" =  "D"),
                                selected = "T",
                                inline = TRUE),
                   selectInput(ns("col_pal"), 
                               label = "Select desired palette:",
                               choices = colour_palettes,
                               selected = "R4",
                               width = '50%'),
                   uiOutput(ns("well_length_text_palette")),
                   br(),
                   h2("Interactive graphs"),
                   actionButton(ns("generate"), "Generate graphs"),
                   br(),
                   h3("Single Chemical, Single Graph"),
                   h4("Generate word document output of single chemical graph"),
                   p("Download the graph for the analyte for the selected wells. ",
                     "You will need to press the 'Generate graphs' button below first for the download button to appear."),
                   uiOutput(ns("d_chem_single_all")),
                   uiOutput(ns("d_download1")),
                   br(),
                   girafeOutput(ns("graph"),
                                height = plotly_height_val) %>% 
                     withSpinner(),
                   uiOutput(ns("d_download_graph1")),
                   br(),
                   h3("Single Chemical, Individual Graphs"),
                   h4("Generate word document output of faceted graph"),
                   p("Download the graph for the analyte for the selected wells in a faceted chart. ",
                     "You will need to press the 'Generate graphs' button below first for the download button to appear."),
                   uiOutput(ns("d_download2")),
                   uiOutput(ns("facet_charts")) %>% 
                     withSpinner()
               )
      )
      
    ) # end page
    
  ) # end tag list
}# end ui

# data server side ---------------
selected_wells_chems_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables---------------------------------------------------------
    r_analyte <- reactive(input$analyte)
    r_wells <- reactive(input$wells)
    r_scale <- reactive(input$scale)
    r_frac <- reactive(input$frac)
    r_well_groups <- reactive(input$well_groups)
    r_col_pal <- reactive(input$col_pal)
    
    # reactive_datasets------------------------------------------------------------
    r_filtered_data <- reactive(rv$clean_df |> 
                                  filter(CHEMICAL_NAME == input$analyte,
                                         SYS_LOC_CODE %in% input$wells,
                                         SAMPLE_DATE >= input$dates[1],
                                         SAMPLE_DATE <= input$dates[2],
                                         FRACTION == input$frac) %>% 
                                  convert_mu_symbol(REPORT_RESULT_UNIT)) 

    
    # dynamic inputs --------------------------------------------------------------
    output$d_dates <- renderUI({
      
      req(rv$clean_df)
      ns <- session$ns
      
      dateRangeInput(ns("dates"),
                     label = "Date range:",
                     start = rv$date_range[1],
                     end = rv$date_range[2],
                     min = rv$date_range[1],
                     max = rv$date_range[2],
                     width = '50%')
    })
    
    
    output$d_analyte <- renderUI({
      
      req(rv$clean_df)
      ns <- session$ns
      
      selectInput(ns("analyte"),
                  label = "Select analyte to analyse",
                  choices = sort(unique(rv$clean_df$CHEMICAL_NAME)),
                  selected = sort(unique(rv$clean_df$CHEMICAL_NAME))[1],
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
    
    
    # well selection 
    output$d_wells <- renderUI({
      req(rv$wells)
      
      ns <- session$ns
      
      selectizeInput(ns("wells"),
                     label = "Select well(s) to analyse",
                     choices = str_sort(rv$wells, numeric = TRUE),
                     selected = rv$wells_selected_initial,
                     multiple = TRUE)
    })
    
    # outputs ------------------------------------------------------------------------------
    
    output$graph <- renderGirafe({
      
      if(!is.null(rv$selected_wells_chart)){
        rv$selected_wells_chart
      }
    })
    
    # text for well length warning
    output$well_length_text <- renderUI({
      
      req(input$wells)
      
      text <- if_else(length(input$wells) > 8,
                      "<p style='color:#FF0000';>Note: It is easier to tell wells apart when <b>eight or fewer wells</b> are selected.</p>",
                      "")
      
      HTML(text)
    })
    
    # text for well length warning for palettes
    output$well_length_text_palette <- renderUI({
      
      req(input$wells)
      
      text <- case_when(input$col_pal == "R4" & length(input$wells) > 8 ~ 
                          "<p style='color:#FF0000';>Note: The 'R4' palette can handle a maxiumum of eight (8) wells. Choose fewer wells or a different palette.</p>",
                        input$col_pal == "Classic Tableau" & length(input$wells) > 10 ~ 
                          "<p style='color:#FF0000';>Note: The 'Classic Tableau' palette can handle a maxiumum of eight (10) wells. Choose fewer wells or a different palette.</p>",
                        TRUE ~ "")
      
      HTML(text)
    })
    
    # single chem chart faceted
    output$facet_charts <- renderUI({
      
      req(rv$faceted_charts)
      
      ns <- session$ns
      
      tagList(
        map(rv$faceted_charts, ~girafe(ggobj = .x))
      )
    })
    
    
    # observers ------------------------------------------------------------------------------
    # generate graphs when button pressed
    observeEvent(input$generate,{
      
      # check palette choice
      if(input$col_pal == "R4" & length(input$wells) <= 8 | 
         input$col_pal == "Classic Tableau" & length(input$wells) <= 10 |
         !input$col_pal %in% c("R4", "Classic Tableau")) {
        
        # if good, then do the rest
        if(nrow(r_filtered_data()) > 0){
          rv$show_selected_wells_chart <- TRUE
          
          rv$selected_wells_chart <- single_chem_selected_wells_ggg(r_filtered_data(),
                                                                    chem_selected = input$analyte,
                                                                    wells_selected = input$wells,
                                                                    crit_data = rv$criteria_df,
                                                                    annotation_df = rv$annotation_df,
                                                                    col_pal = input$col_pal,
                                                                    show_annotations = input$show_annotation_lines,
                                                                    y_scale = input$scale,
                                                                    is_ggiraph = TRUE)
          
          # static version
          rv$selected_wells_png <- single_chem_selected_wells_ggg(r_filtered_data(),
                                                                  chem_selected = input$analyte,
                                                                  wells_selected = input$wells,
                                                                  crit_data = rv$criteria_df,
                                                                  annotation_df = rv$annotation_df,
                                                                  col_pal = input$col_pal,
                                                                  show_annotations = input$show_annotation_lines,
                                                                  y_scale = input$scale,
                                                                  is_ggiraph = FALSE)
          
          # faceted version of chart
          rv$faceted_charts <- single_chem_many_facets_ggg(r_filtered_data(),
                                                           annotation_df = rv$annotation_df,
                                                           max_num = 12,
                                                           legend_pos = "bottom",
                                                           crit_data = rv$criteria_df,
                                                           show_annotations = input$show_annotation_lines,
                                                           y_scale = input$scale,
                                                           is_ggiraph = TRUE)
          
        } else {
          # not enough data
          rv$show_selected_wells_chart <- FALSE
          rv$selected_wells_chart <- NULL
          
          showModal(modalDialog(
            p("No data for the well group in the selected period.")
          ))
          
        }
      } else {
        
        showModal(modalDialog(
          p("Please select fewer wells, or choose a different colour palette.")
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
    
    
    # downloads -----------------------------------------------------
    
    # download button for single chart
    output$d_download1 <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_selected_wells_chart)){
        downloadButton(ns("download1"),
                       label = "Download Word document of single graph")
      }
    })
    
    # extra button for graph for single chart
    output$d_download_graph1 <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_selected_wells_chart)){
        downloadButton(ns("download_graph1"),
                       label = "Download single graph as .png")
      }
    })
    
    output$d_chem_single_all <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_selected_wells_chart)){
        radioButtons(ns("chem_single_all"),
                     label = "Chemicals in word output",
                     choices = c("Selected only", "All available"),
                     selected = "Selected only",
                     width = '50%',
                     inline = TRUE)
      }
    })
    
    # single chart
    output$download1 <- downloadHandler(
      filename = function(){
        if(input$chem_single_all == "Selected only"){
          glue("Selected_Wells_{input$analyte}_{format(Sys.Date(), '%Y_%m_%d')}.docx")
        } else {
          glue("Selected_Wells_All_Chems_{format(Sys.Date(), '%Y_%m_%d')}.docx")
        }
        
      },
      content = function(file) {
        
        # graphs
        withProgress(
          message ='Please wait',
          detail ='Generating graphs ...',
          value = 0, {
            
            
            # these bits needed so that it doesn't crash when producing the 'D' fraction ones.
            # might also speed it up by not doing the unneeded chems
            selected_wells_word_filtered <- reactive(
              
              if(input$chem_single_all == "Selected only"){
                
                rv$clean_df %>%
                  filter(CHEMICAL_NAME == r_analyte(),
                         SYS_LOC_CODE %in% r_wells(),
                         SAMPLE_DATE >= input$dates[1],
                         SAMPLE_DATE <= input$dates[2],
                         FRACTION == r_frac()) %>% 
                  convert_mu_symbol(REPORT_RESULT_UNIT)
              } else {
                rv$clean_df %>%
                  filter(SYS_LOC_CODE %in% r_wells(),
                         SAMPLE_DATE >= input$dates[1],
                         SAMPLE_DATE <= input$dates[2],
                         FRACTION == r_frac()) %>% 
                  convert_mu_symbol(REPORT_RESULT_UNIT)
              })
            
            r_chems_word <- reactive(selected_wells_word_filtered() %>%
                                       distinct(CHEMICAL_NAME) %>%
                                       pull(CHEMICAL_NAME) |> 
                                       sort())
            
            
            rv$word_graphs_selected_chems <- map(r_chems_word(), 
                                                 ~single_chem_selected_wells_ggg(selected_wells_word_filtered(),
                                                                                 chem_selected = .x,
                                                                                 wells_selected = r_wells(),
                                                                                 crit_data = rv$criteria_df,
                                                                                 annotation_df = rv$annotation_df,
                                                                                 col_pal = r_col_pal(),
                                                                                 show_annotations = input$show_annotation_lines,
                                                                                 y_scale = r_scale(),
                                                                                 prog = 1/length(.x)))
            
          })
        
        withProgress(message ='Please wait',
                     detail ='Rendering document ...',
                     value = 0, {
                       
                       tempReport <- file.path(tempdir(), "selected_wells.rmd")
                       file.copy("markdown_templates/selected_wells.Rmd", tempReport, overwrite = TRUE)
                       tempTemplate <- file.path(tempdir(), "blank.docx")
                       file.copy(word_template_file, tempTemplate, overwrite = TRUE)
                       
                       params <- list(word_graphs = rv$word_graphs_selected_chems,
                                      chems = r_chems_word(),
                                      wells = r_wells(),
                                      date_range = input$dates,
                                      frac = r_frac(),
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
    
    # single chart
    output$download_graph1 <- downloadHandler(
      filename = function(){
        glue("{r_analyte()}_selected.png")
        },
      content = function(file) {
        ggsave(filename = file,
               plot = rv$selected_wells_png)
      } # end content
    ) # end download
    
    
    # download button faceted chart
    output$d_download2 <- renderUI({
      req(rv$faceted_charts)
      req(rv$show_selected_wells_chart)
      
      ns <- session$ns
      
      if(isTruthy(rv$faceted_charts) & isTruthy(rv$show_selected_wells_chart)){
        downloadButton(ns("download2"),
                       label = "Download Word document of faceted graph")
      } else {
        NULL
      }
    })
    
    # faceted chart
    output$download2 <- downloadHandler(
      filename = function(){
        glue("Facets_{input$analyte}_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        
        rv$word_graphs_an <- single_chem_many_facets_ggg(r_filtered_data(),
                                                         annotation_df = rv$annotation_df,
                                                         max_num = 12,
                                                         legend_pos = "bottom",
                                                         crit_data = rv$criteria_df,
                                                         show_annotations = input$show_annotation_lines,
                                                         y_scale = input$scale)
        
        withProgress(message ='Please wait',
                     detail ='Rendering document ...',
                     value = 0, {
                       
                       tempReport <- file.path(tempdir(), "single_chem_facets.rmd")
                       file.copy("markdown_templates/single_chem_facets.Rmd", tempReport, overwrite = TRUE)
                       tempTemplate <- file.path(tempdir(), "blank.docx")
                       file.copy(word_template_file, tempTemplate, overwrite = TRUE)
                       
                       params <- list(word_graphs = rv$word_graphs_an,
                                      analyte = input$analyte,
                                      date_range = input$dates,
                                      frac = input$frac,
                                      rendered_by_shiny = TRUE)
                       
                       rmarkdown::render(tempReport,
                                         output_file = file,
                                         params = params,
                                         envir = new.env(parent = globalenv())
                       )
                     }) # end with progress
        
        # to stop the downloads timing out
        # edit the /etc/shiny-server/shiny-server.conf file
        # https://stackoverflow.com/questions/44428211/network-error-triggering-the-download-reportreport-generation-action-in-server
        # https://stackoverflow.com/questions/51520166/updating-shiny-server-config-to-change-timeout-error
        
      } # end content
    ) # end download
    
  }) # end module
} # end server