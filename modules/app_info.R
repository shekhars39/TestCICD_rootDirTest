# ui side ----------------------
app_info_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      # liability popup
      uiOutput(ns("liability")),
      
      fluidRow(width = 12,
               box(width = 12,
                   h1("General Application for Groundwater Analytics (GAGA)"),
                   
                   h2("Introduction"),
                   introduction_text, # from app_constants.R
                   br(),
                   
                   h2("User guide"),
                   p("The GAGA user guide provides more information about this application."),
                   downloadLink(ns("download"),
                                "Download the GAGA user guide"),
                   br(),
                   
                   h2("Intellectual Property Protection Notice"),
                   liability_text, # from app_constants.R

                   h2("Development information"),
                   HTML("This application was developed with funding from the CDM Smith R&D Program in partnership with CDM Smith Australia. ",
                        "You can find the source code in the application's ",
                        "<a href = 'https://github.com/cdmsmithinc/1001406_general_groundwater'>Github repository</a> (CDM Smith licence required)."),
                   
                   h2("Contacts"),
                   HTML("<b>Emma Ehret, Environmental Engineer - </b>",
                        "Application Design, Technical Direction, User Experience, Project Implementation - CDM Smith:",
                        "<a href = 'mailto: ehretel@cdmsmith.com'>ehretel@cdmsmith.com</a></br>"),
                   HTML("<b>William Lai, Senior Data Scientist - </b>",
                        "Application Development, Programmer - CDM Smith Australia:",
                        "<a href = 'mailto: laiw@cdmsmith.com'>laiw@cdmsmith.com</a></br>"),
                   HTML("<b>Hannah Rolston, Environmental Engineer - </b>",
                        "User Experience, Project Implementation - CDM Smith:",
                        "<a href = 'mailto: rolstonhm@cdmsmith.com'>rolstonhm@cdmsmith.com</a></br>"),

                   h2("Version details"),
                   # these details in data/app_constants.R
                   HTML(glue("Version: {version_number}</br>
                             Release date: {format(ymd(version_date), date_format_dmy)}"))
               )
      )
    ) # end page
  ) # end tag list
}# end ui

# server side ---------------
app_info_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    
    # the liability popup
    output$liability <- renderUI({
      showModal(modalDialog(liability_text,
                            title = "Intellectual Property Protection Notice",
                            footer = modalButton("Click here to accept these terms and enter the application."))
                )
      })
    
    # download the user guide
    output$download <- downloadHandler(
      filename <- "GAGA Guide.v2_2025_01.pdf",
      content = function(file) {
        file.copy(filename, file)
      }
    )
    
  }) # end module
} # end server