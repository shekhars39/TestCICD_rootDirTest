# ui side ----------------------
total_voc_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Total Volatile Organic Compounds"),
                   HTML("This module will generate time series graphs for sum of all mass concentration and molar concentration volatile organic compound (VOC) results in each location for a selected date range,
                        with the options to view interactively, download as a Word file, or download as a png file. ",
                        "There are user options for y-axis scale and color palettes. ",
                        "</br></br>",
                        "In the mass concentration graphs, filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
                        "When only the year is shown in the x-axis label, this represents the start of the year."),
                   br(),
                   br(),
                   uiOutput(ns("d_well_groups")),
                   uiOutput(ns("d_wells")),
                   uiOutput(ns("d_chems")),
                   br(),
                   uiOutput(ns("d_dates")), 
                   br(),
                   radioButtons(ns("scale"),
                                label = "Y-axis scale (Mass concentration chart only)",
                                choices = c("normal", "log10"),
                                selected = "log10",
                                inline = TRUE),
                   radioButtons(ns("yaxis_range_type"),
                                label = "Y-axis range type (Mass concentration chart only)",
                                choices = c("autofit",
                                            "user-defined" = "user"),
                                selected = "autofit",
                                inline = TRUE),
                   conditionalPanel(condition = "input.yaxis_range_type == 'user'",
                                        column(width = 6,
                                               numericInput(ns("yaxis_range_lower"),
                                                            label = "Y-axis range lower limit",
                                                            value = 1e-2,
                                                            width = "50%")),
                                        column(width = 6,
                                               numericInput(ns("yaxis_range_upper"),
                                                            label = "Y-axis range upper limit",
                                                            value = 1e3,
                                                            width = "50%")),
                                    ns = NS(id)),
                   br(),
                   radioButtons(ns("use_rep_value"),
                                label = "Use a representative value for data points when all VOCs are nondetect (Mass concentration chart only)",
                                choices = c("Yes", "No"),
                                selected = "No",
                                inline = TRUE),
                   conditionalPanel(condition = "input.use_rep_value == 'Yes'",
                                    column(width = 6,
                                    numericInput(ns("rep_value"),
                                                 label = "Representative value for data point when all VOCs are nondetect",
                                                 value = 1e-2,
                                                 width = "50%")),
                                    ns = NS(id)),
                   br(),
                   selectInput(ns("col_pal"), 
                               label = "Select desired palette:",
                               choices = colour_palettes,
                               selected = "R4",
                               width = '50%'),
                   radioButtons(ns("show_annotation_lines"),
                                label = "Show vertical annotation lines",
                                choices = c("Yes", "No"),
                                selected = "Yes",
                                inline = TRUE),
                   uiOutput(ns("well_length_text_palette")),
                   br(),
                   uiOutput(ns("d_download1")),
                   br(),
                   h2("Interactive graphs"),
                   uiOutput(ns("d_generate")),
                   br(),
                   uiOutput(ns("d_download_data")),
                   h3("Mass concentrations"),
                   girafeOutput(ns("mass_graph"),
                                height = plotly_height_val) |>  
                     withSpinner(),
                   uiOutput(ns("d_download_mass_graph")),
                   br(),
                   h3("Molar concentrations"),
                   girafeOutput(ns("molar_graph"),
                                height = plotly_height_val) |> 
                     withSpinner(),
                   uiOutput(ns("d_download_molar_graph"))
                   
               )
      )
      
    ) # end page
    
  ) # end tag list
}# end ui

# data server side ---------------
total_voc_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables---------------------------------------------------------
    r_wells <- reactive(input$wells)
    r_scale <- reactive(input$scale)
    r_well_groups <- reactive(input$well_groups)
    r_col_pal <- reactive(input$col_pal)
    r_dates <- reactive(input$dates)
    r_chems <- reactive(input$chems)
    r_yaxis_range_type <- reactive(input$yaxis_range_type)
    r_use_rep_value <- reactive(input$use_rep_value)
    
    # initialise these
    rv$tvoc_subtitle <- "Selected Wells" # subtitle - is also the filename prefix
    rv$tvoc_yaxis_range <- c(NA_real_, NA_real_)
    rv$tvoc_rep_value <- NA_real_
    
    # reactive_datasets------------------------------------------------------------
    
    
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
    
    # chem selection
    output$d_chems <- renderUI({
      req(rv$analyte_groups_df)
      
      ns <- session$ns
      
      # default selected chems
      if("VOCs" %in% unique(rv$analyte_groups_df$ANALYTE_GROUP)){
        print("Using VOCs from ANALYTE_GROUPS")
        
        selected_cas_rn <- rv$voc_analyte_group |> 
          pull(CAS_RN)
        
        # for below
        chem_choices <- rv$clean_df %>%
          filter(CAS_RN %in% selected_cas_rn) %>%
          distinct(CHEMICAL_NAME) %>%
          pull(CHEMICAL_NAME) %>%
          sort()
        
        tagList(
          selectizeInput(ns("chems"),
                         label = "Chemicals to sum",
                         choices = chem_choices,
                         selected = chem_choices,
                         multiple = TRUE),
          p("Chemicals designated as VOCs in the data input file. 
            You can delete chemicals from this list here in the module, 
            but you can only add new chemicals by revising and reloading your data input file.")
        )
        
      } else {
        
        showModal(modalDialog(
          p("Your upload does not contain a 'VOCs' group in the ANALYTE_GROUP sheet.",
            strong("You won't be able to run the analysis in this module."))
        ))
      }

    })
    
    
    # generate button
    output$d_generate <- renderUI({
      req(rv$analyte_groups_df)
      
      ns <- session$ns
      
      # default selected chems
      if("VOCs" %in% unique(rv$analyte_groups_df$ANALYTE_GROUP)){

        # add the button
        actionButton(ns("generate"), "Generate graphs")
        
      } else {
        
        HTML("<p style='color:#FF0000';>Your upload does not contain a 'VOCs' group in the ANALYTE_GROUP sheet. 
             <b>You won't be able to run the analysis in this module.</b></p>")
      }
      
    })
    
    # outputs ------------------------------------------------------------------------------
    output$mass_graph <- renderGirafe({
      if(!is.null(rv$total_voc)){
        rv$total_voc
      }
    })
    
    output$molar_graph <- renderGirafe({
      if(!is.null(rv$tvoc_molar)){
        rv$tvoc_molar
      }
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

    
    # observers -----------------------
    # generate graphs when button pressed
    observeEvent(input$generate,{
      
      # check palette choice
      if(input$col_pal == "R4" & length(input$wells) <= 8 | 
         input$col_pal == "Classic Tableau" & length(input$wells) <= 10 |
         !input$col_pal %in% c("R4", "Classic Tableau")) {
        
        # if good, then do generate datasets - ggiraph orig
        rv$total_voc <- total_voc_mass_graph_ggg(dataset = rv$gchem_df,
                                                 wells_selected = r_wells(),
                                                 analytes_selected = r_chems(),
                                                 use_rep_value = r_use_rep_value(), 
                                                 rep_value = rv$tvoc_rep_value,
                                                 y_scale = r_scale(),
                                                 y_scale_type = r_yaxis_range_type(),
                                                 y_scale_range = rv$tvoc_yaxis_range,
                                                 min_date = r_dates()[1],
                                                 max_date = r_dates()[2],
                                                 col_pal = r_col_pal(),
                                                 criteria_dataset = rv$criteria_df,
                                                 annotation_df = rv$annotation_df,
                                                 show_annotations = input$show_annotation_lines,
                                                 is_ggiraph = TRUE)
        
        # static orig
        rv$total_voc_png <- total_voc_mass_graph_ggg(dataset = rv$gchem_df,
                                                     wells_selected = r_wells(),
                                                     analytes_selected = r_chems(),
                                                     use_rep_value = r_use_rep_value(), 
                                                     rep_value = rv$tvoc_rep_value,
                                                     y_scale = r_scale(),
                                                     y_scale_type = r_yaxis_range_type(),
                                                     y_scale_range = rv$tvoc_yaxis_range,
                                                     min_date = r_dates()[1],
                                                     max_date = r_dates()[2],
                                                     col_pal = r_col_pal(),
                                                     criteria_dataset = rv$criteria_df,
                                                     annotation_df = rv$annotation_df,
                                                     show_annotations = input$show_annotation_lines,
                                                     is_ggiraph = FALSE)
        
        # tables for download - note that this is a list of two outputs, interim and final
        rv$total_voc_df <- get_total_voc_df(dataset = rv$gchem_df,
                                            wells_selected = r_wells(),
                                            analytes_selected = r_chems(),
                                            use_rep_value = r_use_rep_value(), 
                                            rep_value = rv$tvoc_rep_value)
        
        # ggiraph molar - this is for multiple wells - ggiraph version only at the moment
        rv$tvoc_molar <- molar_conc_sum_graph_ggg(dataset = rv$gchem_df,
                                                  wells_selected = r_wells(),
                                                  analytes_selected = r_chems(),
                                                  molar_mass_table = voc_molecular_weights,
                                                  min_date = r_dates()[1],
                                                  max_date = r_dates()[2],
                                                  col_pal = r_col_pal(),
                                                  annotation_df = rv$annotation_df,
                                                  show_annotations = input$show_annotation_lines,
                                                  is_ggiraph  = TRUE)
        
        # static molar
        rv$tvoc_molar_png <- molar_conc_sum_graph_ggg(dataset = rv$gchem_df,
                                                  wells_selected = r_wells(),
                                                  analytes_selected = r_chems(),
                                                  molar_mass_table = voc_molecular_weights,
                                                  min_date = r_dates()[1],
                                                  max_date = r_dates()[2],
                                                  col_pal = r_col_pal(),
                                                  annotation_df = rv$annotation_df,
                                                  show_annotations = input$show_annotation_lines,
                                                  is_ggiraph  = FALSE)
        
        # tables for download - note that this is a list of two outputs, interim and final
        rv$molar_conc_sum_df <- get_molar_conc_sum_df(dataset = rv$gchem_df,
                                                      wells_selected = r_wells(),
                                                      analytes_selected = r_chems(),
                                                      molar_mass_table = voc_molecular_weights,
                                                      min_date = r_dates()[1],
                                                      max_date = r_dates()[2])
        
      } else {
        
        showModal(modalDialog(
          p("Please select fewer wells, or choose a different colour palette.")
        ))
      }
      
      # show if using representative value
      if(r_yaxis_range_type() == "user"){
        showModal(
          modalDialog(
            p(glue("Y-axis limited to values between {input$yaxis_range_lower} and {closest_higher(cust_y_poss_vals, input$yaxis_range_upper)}. ", 
                   "Values outside of this range are not shown."))
          )
        )
      }
      
    })
    
    # observe the well group selection (if it exists)
    observeEvent(input$well_groups,{
      print(input$well_groups)
      
      new_wells <- rv$well_groups_df |> 
        filter(LOC_GROUP_CODE == r_well_groups()) |> 
        pull(SYS_LOC_CODE)
      
      # assign subtitle to the selected well group - is also the filename prefix
      rv$tvoc_subtitle <- input$well_groups
      
      # this is not namespaced!!!!
      updateSelectizeInput(session = session,
                           inputId = "wells",
                           selected = new_wells)
    })
    
    # observe the y axis range
    observeEvent(input$yaxis_range_lower | input$yaxis_range_upper,{
      
      rv$tvoc_yaxis_range <- c(input$yaxis_range_lower, input$yaxis_range_upper)
    })
    
    # observe the representative value
    observeEvent(input$rep_value,{
      
      rv$tvoc_rep_value <- input$rep_value
    })
    
    # downloads -----------------------------------------------------
    
    # download button
    output$d_download1 <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$total_voc_png)){
        tagList(
          h2("Generate word document output of graphs"),
          downloadButton(ns("download1"),
                         label = "Download Word document with both graphs")
        )
      }
    })
    
    # extra button for  top graph
    output$d_download_mass_graph <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$total_voc_png)){
        downloadButton(ns("download_mass_graph"),
                       label = "Download mass graph as .png")
      }
    })
    
    # extra button for bottom graph
    output$d_download_molar_graph <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$tvoc_molar_png)){
        downloadButton(ns("download_molar_graph"),
                       label = "Download molar graph as .png")
      }
    })
    
    # download for word document
    output$download1 <- downloadHandler(
      filename = function(){
        glue("{rv$tvoc_subtitle}_TVOC_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        
        # word doc
        withProgress(message ='Please wait',
                     detail ='Rendering document ...',
                     value = 0, {
                       
                       tempReport <- file.path(tempdir(), "tvoc.rmd")
                       file.copy("markdown_templates/tvoc.Rmd", tempReport, overwrite = TRUE)
                       tempTemplate <- file.path(tempdir(), "blank.docx")
                       file.copy(word_template_file, tempTemplate, overwrite = TRUE)
                       
                       params <- list(word_mass_graph = rv$total_voc_png,
                                      word_molar_graph = rv$tvoc_molar_png,
                                      subtitle = rv$tvoc_subtitle,
                                      wells = r_wells(),
                                      chems = r_chems(),
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
    
    output$download_mass_graph <- downloadHandler(
      filename = function(){
        glue("{rv$tvoc_subtitle}_TVOC_mass.png")
        },
      content = function(file) {
        ggsave(filename = file,
               plot = rv$total_voc_png)
      } # end content
    ) # end download
    
    
    output$download_molar_graph <- downloadHandler(
      filename = function(){
        glue("{rv$tvoc_subtitle}_TVOC_molar.png")
        },
      content = function(file) {
        ggsave(filename = file,
               plot = rv$tvoc_molar_png)
      } # end content
    ) # end download
    
    
    # download button for molar conc table
    output$d_download_data <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$molar_conc_sum_df) & isTruthy(rv$total_voc_df)){
        tagList(
          h3("Graph data"),
          p("These tables contain the mass and molar concentrations used in the graphs"),
          downloadButton(ns("download_data"),
                         label = "Download graph data"),
          br(),
          HTML("When only the year is shown in the x-axis label, this represents the start of the year. ")
        )
      }
    })
    
    # output for molar conc table
    output$download_data <- downloadHandler(
      filename = function(){
        glue("{rv$tvoc_subtitle}_Total_VOCs_data.xlsx")
        },
      content = function(file) {
        
        # sheets - rename a couple of columns too
        sheets <- list("total_voc_interim" = rv$total_voc_df$interim,
                       "total_voc_final" = rv$total_voc_df$final |> 
                         rename(`Total VOC Concentration (ug/L)` = REPORT_RESULT_VALUE),
                       "sum_molar_conc_interim" = rv$molar_conc_sum_df$interim |> 
                         rename(`Molar Concentration (umol/L)` = value),
                       "sum_molar_conc_final" = rv$molar_conc_sum_df$final |> 
                         rename(`Total Molar Concentration (umol/L)` = value))
        
        # the preloaded table - if it exists
        if(isTruthy(rv$voc_analyte_group)){
          
          voc_table <- rv$voc_analyte_group |> 
            select(-ANALYTE_GROUP)
          
          if(nrow(voc_table) > 0){
            # if there is VOC data, add to list
            sheets$`Uploaded VOCs` <- voc_table
          }
        }

        openxlsx::write.xlsx(sheets, file, asTable = TRUE, colWidths = "auto")
      } # end content
    ) # end download
    
  }) # end module
} # end server