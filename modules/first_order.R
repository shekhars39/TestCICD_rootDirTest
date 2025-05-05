# ui side ----------------------
first_order_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("First-order Model"),
                   p("This module is in testing. You'll need criteria data as well as a VOC analyte to run this."),
                   uiOutput(ns("d_well")), # from server side
                   uiOutput(ns("d_analyte")), # from server side
                   p("Only analytes in the 'VOC' group in the 'ANALYTE_GROUPS' sheet with criteria in the 'CRITERIA' sheet can be selected"),
                   uiOutput(ns("d_dates")), # from server side
                   radioButtons(ns("frac"),
                                label = "Fraction",
                                choices = c("T", "D"), # might need to change this later
                                selected = "T",
                                inline = TRUE),
                   sliderInput(ns("conf_level"),
                               label = "Confidence level for model confidence limits",
                               min = 0.8,
                               max = 0.99,
                               step = 0.05,
                               ticks = FALSE,
                               value = 0.95,
                               width = "50%"),
                   br(),
                   uiOutput(ns("d_generate")),
                   br(),
                   uiOutput(ns("d_download")),  
                   br(),
                   
                   h2("Dataset information"),
                   DTOutput(ns("values_df")),  
                   DTOutput(ns("table")),
                   br(),
                   
                   h2("Model coefficients"),
                   p("First order model"),
                   DTOutput(ns("lm_tidy")),
                   br(),
                   p("Exponential first-order model"),
                   DTOutput(ns("exp_tidy")),
                   
                   h2("Model summary"),
                   DTOutput(ns("lm_glance")),
                   
                   h2("Charts"),
                   girafeOutput(ns("lm_graph"),
                                height = plotly_height_val) |>  
                     withSpinner(),
                   girafeOutput(ns("exp_graph"),
                                height = plotly_height_val) |>  
                     withSpinner()
                   )

      ) # end fluid page
      
    ) # end page
    
  ) # end tag list
}# end ui

# data server side --------------------------------------------------------------------
first_order_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables--------------------
    # choices for wells, chems and units
    r_vals_df <- reactive({
      rv$clean_df %>%
        distinct(SYS_LOC_CODE, CHEMICAL_NAME, REPORT_RESULT_UNIT)
    })
    
    # selected
    r_dates <- reactive(input$dates)
    r_well <- reactive(input$well)
    r_analyte <- reactive(input$analyte)
    r_scale <- reactive(input$scale)
    r_frac <- reactive(input$frac)
    
    
    # reactive_datasets----------------------

    
    # dynamic inputs ---------------------------------------------------------------------
    
    output$d_well <- renderUI({
      req(rv$clean_df)
      
      ns <- session$ns
      
      selectInput(ns("well"),
                  label = "Select well to analyse",
                  choices = sort(unique(rv$clean_df$SYS_LOC_CODE)),
                  selected = rv$wells_selected_initial,
                  width = '50%')
      
    })
    
    output$d_analyte <- renderUI({
      req(rv$analyte_groups_df)
      
      ns <- session$ns
      
      # default selected chems
      if("VOCs" %in% unique(rv$analyte_groups_df$ANALYTE_GROUP)){
        print("Using VOCs from ANALYTE_GROUPS")
        
        # the ones in the VOCs group
        voc_cas_rn <- unique(rv$voc_analyte_group$CAS_RN)
        # the ones with criteria
        criteria_cas_rn <- unique(rv$criteria_df$CAS_RN)
        # in both groups
        intersection_cas_rn <- intersect(voc_cas_rn, criteria_cas_rn)
        
        if(length(intersection_cas_rn) > 0){
          # if there are chems in the VOC group and in the dataset
          rv$allow_first_order <- TRUE
          
          # for below
          chem_choices <- rv$gchem_df |> 
            filter(CAS_RN %in% intersection_cas_rn) |>
            distinct(CHEMICAL_NAME) |>
            pull(CHEMICAL_NAME) |>
            sort()
          
          print(chem_choices)
          
          tagList(
            selectInput(ns("analyte"),
                        label = "Select analyte",
                        choices = chem_choices,
                        selected = chem_choices[1],
                        width = "50%")
          )
          
        } else {
          rv$allow_first_order <- FALSE
          
          showModal(modalDialog(
            p("Your dataset does not contain data for chemicals in from 'VOCs' group in the ANALYTE_GROUP sheet.",
              strong("You won't be able to run the analysis in this module."))
          ))
          
        }
        
      } else {
        rv$allow_first_order <- FALSE
        
        showModal(modalDialog(
          p("Your upload does not contain a 'VOCs' group in the ANALYTE_GROUP sheet.",
            strong("You won't be able to run the analysis in this module."))
        ))
      }
      
    })
    
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
    

    # outputs -------------------------------------------------------------------

    output$lm_graph <- renderGirafe({
      if(!is.null(rv$first_order)){
        rv$first_order$lm_graph
      }
    })
    
    output$exp_graph <- renderGirafe({
      if(!is.null(rv$first_order)){
        rv$first_order$exp_graph
      }
    })
    
    output$lm_tidy  <- renderDT({
      if(isTruthy(rv$first_order$lm_tidy)){
        datatable(rv$first_order$lm_tidy)
      } 
      
    })
    
    output$exp_tidy  <- renderDT({
      if(isTruthy(rv$first_order$exp_tidy)){
        datatable(rv$first_order$exp_tidy)
      } 
      
    })
    
    output$lm_glance  <- renderDT({
      if(isTruthy(rv$first_order$lm_glance)){
        datatable(rv$first_order$lm_glance)
      } 
      
    })
    

    output$values_df  <- renderDT({
      if(isTruthy(rv$first_order$values_df)){
        datatable(rv$first_order$values_df)
      } 
      
    })
    
    output$table  <- renderDT({
      if(isTruthy(rv$first_order$table)){
        datatable(rv$first_order$table)
      } 
      
    })

    
    # observers --------------------------------------------------------------------
    # generate graphs when button pressed
    observeEvent(input$generate,{
      
      # interactive version
      rv$first_order <- first_order_data(data_df = rv$gchem_df, 
                                         well_selected = r_well(),
                                         analyte_selected = r_analyte(),
                                         min_date = r_dates()[1],
                                         max_date = r_dates()[2],
                                         frac = r_frac(),
                                         criteria_df = rv$criteria_df,
                                         molar_mass_table = voc_molecular_weights,
                                         conf_level = input$conf_level,
                                         is_ggiraph = TRUE)
      
    })

    

    # downloads -----------------------------------------------------------------------
    
    # generate button
    output$d_generate <- renderUI({
      ns <- session$ns
      
      if(rv$allow_first_order == TRUE){
        actionButton(ns("generate"),
                     "Generate tables and graphs")
      } else {
        HTML("<p style='color:#FF0000';>You won't be able to run the first-order model!</p>")
      }
    })
    
    
    # download button for document
    output$d_download <- renderUI({
      ns <- session$ns

      if(isTruthy(rv$first_order)){
        downloadButton(ns("download"),
                       "Download table and graphs")
      }
    })

    output$download <- downloadHandler(
      filename = function(){
        glue("{r_well()}_{r_analyte()}_first_order_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "first_order.Rmd")
        file.copy("markdown_templates/first_order.Rmd", tempReport, overwrite = TRUE)
        tempTemplate <- file.path(tempdir(), "blank.docx")
        file.copy(word_template_file, tempTemplate, overwrite = TRUE)

        # pass in parameters
        params <- list(dataset = rv$gchem_df,
                       well = r_well(),
                       analyte = r_analyte(),
                       min_date = r_dates()[1],
                       max_date = r_dates()[2],
                       frac = r_frac(),
                       criteria_df = rv$criteria_df,
                       molar_mass_table = voc_molecular_weights,
                       conf_level = input$conf_level,
                       rendered_by_shiny = TRUE)

        # make sure that pandoc is installed on the VM
        rmarkdown::render(tempReport,
                          output_file = file,
                          output_format = 'word_document',
                          params = params,
                          envir = new.env(parent = globalenv()))

      }
    )
    
    
    

    
    # graph download buttons ---------------------------------------------------------
    

    
    
    
  }) # end module
} # end server

