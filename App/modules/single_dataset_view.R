# ui side ----------------------
single_dataset_view_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Single Dataset View"),
                   p("This module allows the user to investigate a single parameter and location dataset in more detail on a single page. ",
                   "It will generate a table with all data results, a time series graph with a Theil Sen trendline, a boxplot,
                   and a statistical analysis summary for the selected date range. ",
                   "This page is interactive, and all generated content can be downloaded in a formatted, bookmarked Word file. ",
                   "Statistical methods are the same as those presented under the Statistical Analysis module."),
                   uiOutput(ns("d_well")), # from server side
                   uiOutput(ns("d_analyte")), # from server side
                   uiOutput(ns("d_dates")), # from server side
                   radioButtons(ns("frac"),
                                label = "Fraction",
                                choices = c("T", "D"), # might need to change this later
                                selected = "T",
                                inline = TRUE),
                   radioButtons(ns("substitute_vals"),
                                label = "Use actual values or substituted values for Mann-Kendall trend test",
                                choices = c("Use actual values" = "AV",
                                            "Substituted values" = "SV"),
                                selected = "AV",
                                inline = TRUE),
                   p("If 'Substituted values' is chosen, then non-detect values will be substituted with the highest 'U' qualified value. 
                     Additionally, detected results lower than the highest 'U' qualified value are also substitued for the 'U' detection limit value"),
                   br(),
                   br(),
                   actionButton(ns("generate"), "Generate tables and graphs"),
                   br(),
                   uiOutput(ns("d_download")),  
                   br(),
                   h2("Table"),
                   uiOutput(ns("text")),
                   DTOutput(ns("table"))
                   ), # end box
      
               box(width = 12,
                   h2("Graphs"),
                   fluidRow(
                     column(width = 6,
                            h3("Time series plot"),
                            radioButtons(ns("scale"),
                                         label = "Select scale for graph",
                                         choices = c("log", "normal"),
                                         selected = "normal",
                                         inline = TRUE),
                            HTML("The graph will only show if there is more than one time point. ",
                                 "The horizontal dotted line shows the Standard (if applicable) for the selected analyte. ",
                                 "Filled points are <b>Detects</b> while hollow points are <b>Non-detects</b>. ",
                                 "A Theil-Sen line is plotted if the scale is 'normal',  
                        while a smoothed line is plotted for detected data only if the scale is 'log'. ",
                                 "Negative values for ORP will not show on the log10 scale."),
                            girafeOutput(ns("ts_chart")),
                            uiOutput(ns("d_download_ts_graph"))
                            ), # end column
                     column(width = 6,
                            h3("Boxplot"),
                            p("Points are jittered (random variation to the location of each point) as it is helpful when visualising (otherwise) overlapping points."),
                            girafeOutput(ns("boxplot")),
                            uiOutput(ns("d_download_boxplot"))
                            ) # end column
                   ) # end fluid row
                   ), # end box
      
               box(width = 12,
                   h2("Statistics"),
                   p("These summary statistics may differ from those done using ProUCL. For details see the 'Application Statistics' tab."),
                   h3("Dataset details"),
                   DTOutput(ns("summary_t1")),
                   h3("Parameter estimation and confidence limits"),
                   DTOutput(ns("summary_t2")),
                   h3("Trend details"),
                   DTOutput(ns("summary_t3"))
                   ) # end box
      ) # end fluid page
      
    ) # end page
    
  ) # end tag list
}# end ui

# data server side --------------------------------------------------------------------
single_dataset_view_server <- function(id, rv) {
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
    r_substitute_vals <- reactive(input$substitute_vals)
    
    
    # analyte choices
    r_filtered_analytes <- reactive({
      req(r_well())
      
      r_vals_df() %>% 
        filter(SYS_LOC_CODE == r_well()) %>%
        pull(CHEMICAL_NAME) %>%
        sort()
      })
    
    # reactive_datasets----------------------
    # the output at the bottom
    r_summary_table_wide <- reactive({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        rv$r_single_initial_df %>% 
          envstats_mk_conf_pred(substitute_vals = input$substitute_vals) %>% 
          mk_conf_pred_end_helper(bg_upl_df = NULL,
                                  criteria_units_table = rv$criteria_df)
      }
      
    })
    
    # mann kendall test with slope and intercept
    r_kendall <- reactive({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        
        table <- tryCatch(bind_rows(envstats_kendall2(rv$r_single_initial_df$REPORT_RESULT_VALUE, test_alternative = "two.sided"),
                                    envstats_kendall2(rv$r_single_initial_df$REPORT_RESULT_VALUE, test_alternative = "greater")),
                          error = function(e) tibble(NA))
        
        ts_values <- theil_sen_helper(rv$r_single_initial_df)
        
        slope <- ts_values$slope
        intercept <- ts_values$intercept
        
        
        return(list("table" = table,
                    "slope" = slope,
                    "intercept" = intercept))
      }
      
    })
    
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
      req(r_filtered_analytes())

      ns <- session$ns
      
      selectInput(ns("analyte"),
                  label = "Select analyte to analyse",
                  choices = r_filtered_analytes(),
                  width = '50%')

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
    

    # outputs -------------------------------
    
    output$text <- renderUI({
      if(isTruthy(rv$show_single_dataset_charts)){
        p("") # nothing
      } else {
        p("Tables and graphs will be rendered when you click the button to generate them.")
      }
    })
    
    # boxplot ------------
    output$boxplot <- renderGirafe({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        g <- single_dataset_jittered_boxplot(rv$r_single_initial_df, is_ggiraph = TRUE)
        girafe(ggobj = g,
               # no button - there is a manual one
               options = list(opts_toolbar(saveaspng = FALSE)))
      }
      
    })
    
    # mann kendall chart outputs ----------------------------
    output$ts_chart <- renderGirafe({

      if(isTruthy(rv$show_single_dataset_charts)){
        
        # the graph -- initial then conditional depending on scale
        g1 <- single_dataset_graph_initial_ggg(rv$r_single_initial_df, selected_scale = input$scale, is_ggiraph = TRUE) 
        
        if(input$scale == "log"){
          # for ggiraph
          g <- g1 + 
            geom_smooth(data = rv$r_single_initial_df %>% 
                          filter(DETECT_FLAG == "Y"),
                        aes(linetype = "Thiel-Sen regression line"),
                        colour = "blue",
                        se = FALSE,
                        key_glyph = "path")
          
        } else {
          # for ggiraph -- normal
          g <- g1 + 
            geom_abline(aes(slope = r_kendall()$slope,
                            intercept = r_kendall()$intercept,
                            linetype = "Thiel-Sen regression line"),
                        colour = "blue",
                        key_glyph = "path")
        }
        
        # ggiraph on the output 
        girafe(ggobj = g,
               # no button - there is a manual one
               options = list(opts_toolbar(saveaspng = FALSE)))
        
      }
      
    })
    
    # data values - simple version with data
    output$table  <- renderDT({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        datatable(rv$r_single_initial_df)
      } 
       
    })
    
    # note if the subsets change, these will need to be changed in the markdown document as well
    # dataset details
    output$summary_t1 <- renderDT({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        if(nrow(r_summary_table_wide()) > 0){
          datatable(r_summary_table_wide()[1:14])
        }
      } 
      
    })
    
    # param est details and CL
    output$summary_t2 <- renderDT({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        if(nrow(r_summary_table_wide()) > 0){
          datatable(r_summary_table_wide()[15:26])
        }
      } 
      
    })
    
    # pram est CL details
    output$summary_t3 <- renderDT({
      
      if(isTruthy(rv$show_single_dataset_charts)){
        if(nrow(r_summary_table_wide()) > 0){
          datatable(r_summary_table_wide()[27:35])
        }
      } 

    })
    
    # observers -----------------------
    observeEvent(input$generate,{
      
      rv$r_single_initial_df <- rv$clean_df %>% 
        filter(SAMPLE_DATE >= r_dates()[1],
               SAMPLE_DATE <= r_dates()[2],
               SYS_LOC_CODE == r_well(),
               CHEMICAL_NAME == r_analyte(),
               FRACTION == r_frac()) %>% 
        select(SYS_LOC_CODE, CAS_RN, CHEMICAL_NAME,
               SAMPLE_DATE, REPORT_RESULT_VALUE, REPORT_RESULT_UNIT, DETECT_FLAG, INTERPRETED_QUALIFIERS, FRACTION) %>% 
        arrange(SAMPLE_DATE) %>% 
        convert_mu_symbol(REPORT_RESULT_UNIT)
      
      
      if(nrow(rv$r_single_initial_df) > 0){
        rv$show_single_dataset_charts <- TRUE
        
      } else {
        # not enough data
        rv$show_single_dataset_charts <- FALSE
        
        showModal(modalDialog(
          p("Not enough data for the selected variables.")
        ))
        
      }
      
      
      # time series graph for download when button pressed
      if(input$scale == "log"){
        # for ggiraph
        rv$sdv_ts_graph <- single_dataset_graph_initial_ggg(rv$r_single_initial_df, input$scale, is_ggiraph = FALSE)  + 
          geom_smooth(data = rv$r_single_initial_df %>% 
                        filter(DETECT_FLAG == "Y"),
                      aes(linetype = "Thiel-Sen regression line"),
                      colour = "blue",
                      se = FALSE,
                      size = 0.5,
                      key_glyph = "path")
        
      } else {
        # for ggiraph -- normal
        rv$sdv_ts_graph <- single_dataset_graph_initial_ggg(rv$r_single_initial_df, input$scale, is_ggiraph = FALSE)  + 
          geom_abline(aes(slope = r_kendall()$slope,
                          intercept = r_kendall()$intercept,
                          linetype = "Thiel-Sen regression line"),
                      colour = "blue",
                      key_glyph = "path")
      }
      
    })
    

    # downloads -----------------------------------------------------
    # download button for document
    output$d_download <- renderUI({
      ns <- session$ns
      
      if(isTruthy(rv$show_single_dataset_charts)){
        downloadButton(ns("download"),
                       "Download table and graphs")
      }
    })
    
    output$download <- downloadHandler(
      filename = function(){
        glue("{r_well()}_{r_analyte()}_{format(Sys.Date(), '%Y_%m_%d')}.docx")
      },
      content = function(file) {
        tempReport <- file.path(tempdir(), "single_dataset.Rmd")
        file.copy("markdown_templates/single_dataset.Rmd", tempReport, overwrite = TRUE)
        tempTemplate <- file.path(tempdir(), "blank.docx")
        file.copy(word_template_file, tempTemplate, overwrite = TRUE)
        
        # pass in parameters
        params <- list(analyte = r_analyte(),
                       well = r_well(),
                       dataset = rv$r_single_initial_df,
                       scale = input$scale,
                       slope = r_kendall()$slope,
                       intercept = r_kendall()$intercept,
                       summary_table_wide = r_summary_table_wide(),
                       frac = r_frac(),
                       date_range = input$dates,
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
    
    # extra button for graph for ts chart
    output$d_download_ts_graph <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_single_dataset_charts)){
        downloadButton(ns("download_ts_graph"),
                       label = "Download graph as .png")
      }
    })
    
    # extra button for graph for boxplot chart
    output$d_download_boxplot <- renderUI({
      ns <- session$ns
      if(isTruthy(rv$show_single_dataset_charts)){
        downloadButton(ns("download_boxplot"),
                       label = "Download graph as .png")
      }
    })
    
    # download button for graphs - ts
    output$download_ts_graph <- downloadHandler(
      filename = function(){
        glue("{r_well()}_{r_analyte()}_ts.png")
        },
      content = function(file) {
        ggsave(filename = file,
               plot = rv$sdv_ts_graph)
      } # end content
    ) # end download
    
    
    # download button for graphs - boxplot
    output$download_boxplot <- downloadHandler(
      filename = function(){
        glue("{r_well()}_{r_analyte()}_boxplot.png")
        },
      content = function(file) {
        ggsave(filename = file,
               plot = single_dataset_jittered_boxplot(rv$r_single_initial_df, is_ggiraph = FALSE))
      } # end content
    ) # end download
    
    
    
  }) # end module
} # end server

