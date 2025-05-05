# ui side ----------------------
inorganics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
        box(width = 12,
            h1("Inorganics Graphs"),
            HTML("This module will generate time series graphs for selected inorganic parameters in:",
                 "<ol>",
                 "<li>interactive format by individual well, and </li>",
                 "<li>downloadable individual graphs format by well group, for the selected date range. </li>",
                 "</ol>",
                 "The graphs show a vertical line for the event entered into the data input file 'Annotation Line' fields. ",
                 "Filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
                 "When only the year is shown in the x-axis label, this represents the start of the year. ",
                 "The user may select whether total (T) or dissolved (D) fractions be plotted. There are also options for y-axis scale and color palette.</br>"),
            br(),
            uiOutput(ns("d_dates")), # from server side
            br(),
            radioButtons(ns("frac_filter"),
                         label = "Fraction",
                         choices = c("T", "D"),
                         selected = "T",
                         inline = TRUE),
            radioButtons(ns("scale"),
                         label = "Y-axis scale",
                         choices = c("normal", "log10"),
                         selected = "log10",
                         inline = TRUE),
            radioButtons(ns("show_annotation_lines"),
                         label = "Show vertical annotation lines",
                         choices = c("Yes", "No"),
                         selected = "Yes",
                         inline = TRUE),
            selectInput(ns("col_pal"), 
                        label = "Select desired palette:",
                        choices = colour_palettes,
                        selected = "R4",
                        width = '50%'),
            uiOutput(ns("well_length_text_palette")),
            br(),
            uiOutput(ns("d_well_groups")),
            uiOutput(ns("d_wells")), # from server side
            uiOutput(ns("well_length_text")),
            br(),
            h2("Generate word document output of graphs"),
            p("Download graphs for all inorganic analytes for the selected wells. ",
              "You will need to press the 'Generate graphs' button below first for the download button to appear."),
            br(),
            uiOutput(ns("d_download")),

            h2("Interactive graphs"),
            HTML("The horizontal dotted line shows the Standard (if applicable) for the selected analyte. ",
              "Filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
              "When only the year is shown in the x-axis label, this represents the start of the year. "),
            br(),
            uiOutput(ns("d_analyte")),
            br(),
            actionButton(ns("generate"), "Generate graphs"),
            br(),
            uiOutput(ns("charts")) 
        )
      )
    ) # end page
  ) # end tag list
}# end ui

# server side ---------------
inorganics_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables----------------------------------------------------
    r_analyte <- reactive(input$analyte)
    r_wells <- reactive(input$wells)
    r_frac_filter <- reactive(input$frac_filter)
    r_well_groups <- reactive(input$well_groups)
    r_scale <- reactive(input$scale)
    r_show_annotation_lines <- reactive(input$show_annotation_lines)
    
    # dynamic inputs ------------------------------------------------------------
    
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
    
    output$d_dates <- renderUI({
      
      req(rv$inorg_df)
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
      
      req()
      ns <- session$ns
      
      selectInput(ns("analyte"),
                  label = "Select analyte to analyse",
                  choices = rv$inorgs_chem_order,
                  selected = rv$inorgs_chem_order[1],
                  width = '50%')
    })

    # # outputs -------------------------------
    output$graph <- renderGirafe({
      
      if(!is.null(rv$inorg_ggiraph)){
        rv$inorg_ggiraph
        }
      })
    
    output$charts <- renderUI({
      req(input$analyte)
      req(rv$inorg_df)
      req(rv$len_inorg_wells)
      
      ns <- session$ns

      graph_height <- reactive(100 + (ceiling((rv$len_inorg_wells / 3)) * 150))
      graph_height_text <- reactive(
        if (graph_height() < 800){
          "800px"
        } else{
          paste0(graph_height(), "px")
        }
        )
      
      # with facets and graphs generated outside so as to set height
      tagList(
        girafeOutput(ns("graph"),
                     height = graph_height_text()) %>%
          withSpinner()
      )
      
    })
    
    # text for well length warning
    output$well_length_text <- renderUI({
      
      req(rv$len_inorg_wells)
      
      text <- if_else(rv$len_inorg_wells > 15,
                      "<p style='color:#FF0000';>Note that the graphs look better when there are <b>15 or fewer wells</b> selected.</p>",
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
    
    
    # # observers -----------------------

    # length of wells for graph size
    observeEvent(input$wells, {
      
      rv$len_inorg_wells <- length(input$wells)
      
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
    
    # generate graphs when button pressed
    observeEvent(input$generate,{
      
      filtered_df <- reactive(rv$inorg_df %>% 
                                filter(SAMPLE_DATE >= input$dates[1],
                                       SAMPLE_DATE <= input$dates[2],
                                       CHEMICAL_NAME == r_analyte(),
                                       SYS_LOC_CODE %in% r_wells(),
                                       FRACTION == r_frac_filter()))
      
      if(nrow(filtered_df()) > 0){
        rv$show_inorg_chart <- TRUE
        
        rv$inorg_ncols <- if_else(rv$len_inorg_wells == 4, 2, 3)

        rv$inorg_ggiraph <- inorg_graphs_ggg_v2(filtered_df(),
                                                r_analyte(), # these double up now because of the filtered_df, but just keep them
                                                r_wells(),
                                                r_frac_filter(),
                                                rv$criteria_df,
                                                y_scale = r_scale(),
                                                annotation_df = rv$annotation_df,
                                                show_annotations = r_show_annotation_lines(),
                                                col_pal = input$col_pal,
                                                is_ggiraph = TRUE,  # will produce a warning due progress - ignore it
                                                ncols = rv$inorg_ncols)
      } else {
        # not enough data
        rv$show_inorg_chart <- FALSE
        rv$inorg_ggiraph <- NULL
        
        showModal(modalDialog(
          p("Not enough data for the well group in the selected period.")
        ))
      }
      
    })
    

    # downloads -----------------------------------------------------
    # download button
    output$d_download <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_inorg_chart)){
        tagList(
          radioButtons(ns("chems_selected_all"),
                       label = "Analytes to include",
                       choices = c("Selected analyte only" = "selected",
                                   "All inorganics analytes" = "all"),
                       selected = "all",
                       inline = TRUE),
          downloadButton(ns("download"))
        )
        
      }
    })
    
    output$download <- downloadHandler(
      filename = function(){
        glue("Inorganics_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        
        # graphs
        withProgress(
          message ='Please wait',
          detail ='Generating graphs ...',
          value = 0, {
            
            if(input$chems_selected_all == "all"){
              # these bits needed so that it doesn't crash when producing the 'D' fraction ones.
              # might also speed it up by not doing the unneeded chems
              inorg_word_filtered <- reactive(rv$inorg_df |>  
                                                filter(SAMPLE_DATE >= input$dates[1],
                                                       SAMPLE_DATE <= input$dates[2],
                                                       FRACTION == r_frac_filter()))
              
            } else {
              # selected only
              inorg_word_filtered <- reactive(rv$inorg_df |> 
                                                filter(SAMPLE_DATE >= input$dates[1],
                                                       SAMPLE_DATE <= input$dates[2],
                                                       CHEMICAL_NAME == r_analyte(),
                                                       FRACTION == r_frac_filter()))
            }
            
            r_chems_word <- reactive(inorg_word_filtered() %>% 
                                       distinct(CHEMICAL_NAME) %>% 
                                       pull(CHEMICAL_NAME))
            
            rv$word_graphs_inorg <- map(r_chems_word(), ~inorg_graphs_ggg_v2(rv$inorg_df %>% 
                                                                       filter(SAMPLE_DATE >= input$dates[1],
                                                                              SAMPLE_DATE <= input$dates[2]),
                                                                     .x,
                                                                     # the extra filters above make the next args a bit redundant, but keep
                                                                     r_wells(), # all of them
                                                                     r_frac_filter(),
                                                                     rv$criteria_df,
                                                                     y_scale = r_scale(),
                                                                     annotation_df = rv$annotation_df,
                                                                     show_annotations = r_show_annotation_lines(),
                                                                     col_pal = input$col_pal,
                                                                     prog = 1/length(.x),
                                                                     ncols = rv$inorg_ncols))
          })
        
        # word doc
        withProgress(message ='Please wait',
                     detail ='Rendering document ...',
                     value = 0, {
                       
                       tempReport <- file.path(tempdir(), "inorg_tsp.rmd")
                       file.copy("markdown_templates/inorg_tsp.Rmd", tempReport, overwrite = TRUE)
                       tempTemplate <- file.path(tempdir(), "blank.docx")
                       file.copy(word_template_file, tempTemplate, overwrite = TRUE)
                       
                       params <- list(word_graphs = rv$word_graphs_inorg,
                                      chems = r_chems_word(),
                                      wells = r_wells(),
                                      frac_filter = r_frac_filter(),
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


  }) # end module
} # end server