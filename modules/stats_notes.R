# ui side ----------------------
stats_notes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(width = 12,
               box(width = 12,
                   h1("Statistical Notes"),
                   p("The application makes use of the following approaches and guidance documents:"),
                   stat_notes_text, # from app_constants.R
                   br(),
                   p("The statistical analysis makes use of R packages such as:"),
                   stats_packages_text # from app_constants.R
               )
      )

    ) # end page
  ) # end tag list
}# end ui

# server side ---------------
stats_notes_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    

  }) # end module
} # end server