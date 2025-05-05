# options ---------------------------------------------------------------------------
options(scipen = 999)

# max upload size
options(shiny.maxRequestSize = 16*1024^2) # 16MB - this is arbitrary but don't want it too big for loading time
# also change the text in data_upload.R

# load libraries ----------------------------------------------------------------
source("load_libraries.R")

# # load datasets -----------------------------------------------------------------
source("data/app_constants.R")
source("data/app_datasets.R")
source("data/voc_molecular_weights.R")

# load functions --------------------------------------------------------------------------
list.files("functions", pattern = ".R") |> 
  walk(~source(glue("functions/{.x}")))

# load modules ----------------------------------------------------------------------
list.files("modules") |> 
  walk(~source(glue("modules/{.x}")))

# load styling (openxlsx)------------------------------------------------------------------
list.files("lft_templates", pattern = ".R") |> 
  walk(~source(glue("lft_templates/{.x}")))

# ui side ----------------------------------------------------------------------------
ui <- dashboardPage(title = "General Application for Groundwater Analytics",
                    
                    # header --------------------------------------------------------------------------
                    dashboardHeader(title = div(
                      img(src="CDMSmith_logo_web_WhiteGr.png",
                          width = 90,
                          style = "padding-right: 10px;"), 
                      "GAGA"),
                      titleWidth = 250
                    ),
                    
                    # sidebar ------------------------------------------------------------------------------
                    dashboardSidebar(
                      width = 350,
                      sidebarMenuOutput("menu")
                    ),
                    
                    # body ---------------------------------------------------------------------------------
                    dashboardBody(
                      useShinyjs(),
                      includeCSS("www/style.css"),
                      tags$head(HTML('<title>General Application for Groundwater Analytics</title> <link rel="icon" href="CDMSmith_logo_web_BlueGr.png" type="image/png"/>')),
                      
                      tabItems(
                        tabItem(tabName = "app_info",
                                app_info_ui("app_info")),
                        
                        tabItem(tabName = "summary",
                                data_summary_ui("summary")),

                        tabItem(tabName = "geochem",
                                coc_geochem_ui("coc_gc")),
                        
                        tabItem(tabName = "total_voc",
                                total_voc_ui("total_voc")),

                        tabItem(tabName = "inorganics",
                                inorganics_ui("inorg")),

                        tabItem(tabName = "app_stats",
                                app_stats_ui("app_stats")),

                        tabItem(tabName = "screening",
                                screening_ui("screening")),

                        tabItem(tabName = "stats_notes",
                                stats_notes_ui("stats_notes")),

                        tabItem(tabName = "single_dataset_view",
                                single_dataset_view_ui("single_dataset_view")),

                        tabItem(tabName = "selected_wells_chems",
                                selected_wells_chems_ui("selected_wells_chems")),
                        
                        tabItem(tabName = "first_order",
                                first_order_ui("first_order"))
                      ) # end tab items
                    ) # end dashboard body
) # end dashboard page


# server side ---------------------------------------------------------------------------------  
server <- function(input, output, session) {
  
  # reactive values -----------------------------------------------------------------
  rv <- reactiveValues()
  
  # dynamic menu ---------------------------------------------------------------------------------
  
  # initial menu - before data gets uploaded
  output$menu <- renderMenu({
    sidebarMenu(
      id="tabs",
      menuItem("Home", tabName = "app_info"),
      conditionalPanel("input.tabs == 'app_info'",
                       id = "cp1",
                       data_upload_ui("data_upload")),
      menuItem("Statistical Notes", tabName = "stats_notes")
    )
  })
  
  # modules ui ----------------------------------------------------------------------
  data_upload_ui("data_upload")
  app_info_ui("app_info")
  data_summary_ui("summary")
  coc_geochem_ui("coc_gc")
  total_voc_ui("total_voc")
  inorganics_ui("inorg")
  stats_notes_ui("stats_notes")
  app_stats_ui("app_stats")
  screening_ui("screening")
  single_dataset_view_ui("single_dataset_view")
  selected_wells_chems_ui("selected_wells_chems")
  # first_order_ui("first_order")
  
  # modules server --------------------------------------------------------------
  data_upload_server("data_upload", rv = rv)
  app_info_server("app_info", rv = rv)
  data_summary_server("summary", rv = rv)
  coc_geochem_server("coc_gc", rv = rv)
  total_voc_server("total_voc", rv = rv)
  inorganics_server("inorg", rv = rv)
  stats_notes_server("stats_notes", rv = rv)
  app_stats_server("app_stats", rv = rv)
  screening_server("screening", rv = rv)
  single_dataset_view_server("single_dataset_view", rv = rv)
  selected_wells_chems_server("selected_wells_chems", rv = rv)
  # first_order_server("first_order", rv = rv)
  
  # observed ---------------------------------------------------------------------
  
  # menu updates when data loaded
  observeEvent(input[["data_upload-load"]], {

    output$menu <- renderMenu({

      # change active tab from app info to data summary when button clicked
      updateTabItems(session,
                     "tabs",
                     "summary")

      # change sidebar to show all options
      sidebarMenu(
        id="tabs",
        menuItem(HTML("<b>Home</b>"), tabName = "app_info"),
        menuItem(HTML("<b>Dataset Information</b>"), tabName = "summary"), 
        menuItem(HTML("<b>Data Screening Tables</b>"), tabName = "screening"),
        menuItem(HTML("<b>Time Series Graphs</b>"), startExpanded = TRUE,
                 menuItem("Chlorinated Ethenes and Geochemistry", tabName = "geochem"),
                 menuItem("Total Volatile Organic Compounds", tabName = "total_voc"),
                 menuItem("Inorganics Graphs", tabName = "inorganics"),
                 menuItem("Selected Wells / Single Parameter", tabName = "selected_wells_chems")
        ),
        menuItem(HTML("<b>Statistical Analysis</b>"), startExpanded = TRUE,
                 menuItem("Statistical Notes", tabName = "stats_notes"),
                 menuItem("Application Statistics", tabName = "app_stats")
        ),
        menuItem(HTML("<b>Single Dataset View</b>"), tabName = "single_dataset_view")#,
        # menuItem(HTML("<b>First-order Model</b>"), tabName = "first_order")
      )

    })

  })
  
} # end server

# Run the application -------------------------------------------------------------------------------
shinyApp(ui = ui,
         server = server)
