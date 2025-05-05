# ui side ----------------------
data_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Dataset information"),
                   h2("Data"),
                   uiOutput(ns("data_info")),
                   girafeOutput(ns("graph"),
                                height = plotly_height_val),
                   br(),
                   
                   h2("Criteria"),
                   uiOutput(ns("crit_info")),
                   br(),
                   dataTableOutput(ns("crit_table")),
                   br(),
                   
                   h2("Annotation lines"),
                   uiOutput(ns("ann_info")),
                   br(),
                   dataTableOutput(ns("ann_table")),
                   br(),
                   
                   h2("Well groups"),
                   uiOutput(ns("wg_info")),
                   br(),
                   dataTableOutput(ns("wg_table")),
                   br(),
                   
                   h2("Analyte groups"),
                   uiOutput(ns("ag_info")),
                   br(),
                   dataTableOutput(ns("ag_table")),
                   br()
                   
               )
      )
      
    ) # end page
  ) # end tag list
}# end ui


# server side ---------------
data_summary_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # reactive variables-----------------------------------------------------------
    
    # reactive datasets -----------------------------------

    year_df <- reactive(rv$clean_df %>% 
                          mutate(year = year(SAMPLE_DATE)) %>% 
                          group_by(year) %>% 
                          summarise(rows = n()) %>% 
                          ungroup())
    
    chemical_summary <- reactive(rv$clean_df %>% 
                                   distinct(CAS_RN, CHEMICAL_NAME, SYS_LOC_CODE) %>% 
                                   arrange(CHEMICAL_NAME, SYS_LOC_CODE))
    
    # outputs ---------------------------------------------------
  
    
    output$data_info <- renderUI({
      
      # uploaded data
      data_rows <- comma(nrow(rv$screening_df))
      data_min_date <- min(rv$screening_df$SAMPLE_DATE)
      data_max_date <- max(rv$screening_df$SAMPLE_DATE)
      

      text <- glue("Loaded data has {data_rows} rows with a date range from {data_min_date} to {data_max_date}.</br>")
      HTML(text)
    })
    
    # graph of dates
    output$graph <- renderGirafe({
      
      req(year_df())
      
      g <- ggplot(year_df(), aes(year, rows)) + 
        theme_bw() +
        geom_col_interactive(aes(tooltip = glue("Year: {year}
                                           Rows: {comma(rows)}"))) +
        labs(x = "Year",
             y = "Records") + 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_x_continuous(breaks = seq(min(year_df()$year), max(year_df()$year), 1))
      
      girafe(ggobj = g)
      
    })
    
    
    # criteria info
    output$crit_info <- renderUI({
      
      # criteria data
      if(nrow(rv$raw_criteria_df) > 0){
        criteria_text <- glue("Criteria loaded for {nrow(rv$raw_criteria_df)} chemicals.")
      } else {
        criteria_text <- "No criteria data added."
      }
      
      text <- glue(
        "Optional sheet. {criteria_text}</br>")
      HTML(text)
    })
    
    output$crit_table <- renderDataTable({
      
      rv$raw_criteria_df |> 
        datatable()
    })
    
    # annotation lines
    output$ann_info <- renderUI({
      
      if(nrow(rv$annotation_df) > 0){
        annotation_text <- "Annotation dates loaded."
      } else {
        annotation_text <- "No vertical annotation lines."
      }
      
      text <- glue(
        "Optional sheet. {annotation_text}</br>")
      HTML(text)
    })
    
    output$ann_table <- renderDataTable({
      
      rv$annotation_df |> 
        datatable()
    })
    
    # well groups
    output$wg_info <- renderUI({
      
      if(isTruthy(rv$well_groups_df)){
        wg_text <- "Well groups loaded."
      } else {
        wg_text <- "No well groups loaded."
      }
      
      text <- glue(
        "Optional sheet. {wg_text}</br>")
      HTML(text)
    })
    
    output$wg_table <- renderDataTable({
      
      if(isTruthy(rv$well_groups_df)){
        rv$well_groups_df |> 
          datatable()
      }
    })
    
    # analyte groups
    output$ag_info <- renderUI({
      
      if(isTruthy(rv$analyte_groups_df)){
        ag_text <- "Analyte groups loaded."
      } else {
        ag_text <- "No analyte groups loaded."
      }
      
      text <- glue(
        "Optional sheet. {ag_text}</br>")
      HTML(text)
    })
    
    output$ag_table <- renderDataTable({
      
      if(isTruthy(rv$analyte_groups_df)){
        rv$analyte_groups_df |> 
          datatable()
      }
    })
    
    # downloads -----------------------------------------------------

    
    # dynamic inputs -----------------------------------------------------
    
    # observers -----------------------------------------------------------------------

    
  }) # end module
} # end server