
library(shiny)
library(shinyjs)
library(shinyBS)
library(shinyWidgets)
library(leaflet)
library(sf)
library(DT)
library(dplyr)

cdm_palette <- c("#7ac143", "#ffd65a", "#00bbe5", "#f47b20", "#dbe0bd", "#c0b7af")

vals <- reactiveValues(
  Data = mtcars
)

# Define UI for CDM Template
ui <- tagList(
  useShinyjs(),
  includeCSS("www/style.css"),
  fluidPage(list(tags$head(HTML(
    ' <link rel="apple-touch-icon" sizes="180x180" href="logo/apple-touch-icon.png">
      <link rel="icon" type="image/png" sizes="32x32" href="logo/favicon-32x32.png">
      <link rel="icon" type="image/png" sizes="16x16" href="logo/favicon-16x16.png">
      <link rel="manifest" href="logo/site.webmanifest">
      <link rel="mask-icon" href="logo/safari-pinned-tab.svg" color="#5bbad5">
      <link rel="shortcut icon" href="logo/favicon.ico">
      <meta name="msapplication-TileColor" content="#ffffff">
      <meta name="msapplication-config" content="logo/browserconfig.xml">
      <meta name="theme-color" content="#ffffff"> '
    )))),
  navbarPage(id = "nav", position = "fixed-top", windowTitle = "CDM Smith",  collapsible = TRUE,
             title = div(
               span(style = "padding-left:15px;padding-right:15px;", img(src="logo/CDMSmith_logo_web_WhiteGr.png", style = "width:70px;")),
                              span(style = "padding-right:15px;", "Template Shiny") # UPDATE TITLE HERE
                         ),
             tabPanel("About", icon=icon("info"),
                    fluidPage(
                      fluidRow(
                        br(), # for the map to be full page the page starts too high
                         column(3, style = "padding-left:20px",
                                strong("Template styles for text:"),
                                h1("Heading 1"),
                                h2("Heading 2"),
                                h3("Heading 3"),
                                h4("Heading 4"),
                                h5("Heading 5"),
                                p("Paragraph text"),
                                hr(),
                                strong("Template styles for buttons:"),
                                br(), br(),
                                actionButton("btn1", "Blue Button", class = "btn-blue"),
                                br(), br(),
                                actionButton("btn2", "Green Button", class = "btn-green"),
                                br(), br(),
                                actionButton("btn3", "Light Blue Button", class = "btn-ltblue"),
                                br(), br(),
                                actionButton("btn4", "Orange Button", class = "btn-orange"),
                                br(), br(),
                                actionButton("btn5", "Yellow Button", class = "btn-yellow")
                                ),
                         column(3, 
                                strong("Use shinyWidgets:"),
                                br(), br(),
                                checkboxGroupButtons(
                                  inputId = "chk_somevalue",
                                  label = "Make a choice:",
                                  choices = c("Choice A", "Choice B"), 
                                  justified = TRUE, status = "ltblue",
                                  checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
                                ),
                                actionButton("btn_success", "Success Alert!", class = "btn-green"),
                                br(), br(),
                                actionButton("btn_error", "Error Alert!", class = "btn-orange"),
                                br(), br(),
                                actionButton("btn_confirm", "Confirm!", class = "btn-yellow"),
                                br(), br(),
                                actionButton("btn_input", "Input!", class = "btn-blue"),
                                br(), br(),
                                pickerInput(
                                  inputId = "myPicker", 
                                  label = "Select/deselect all + format selected", 
                                  choices = LETTERS, 
                                  options = list(
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3"
                                  ), 
                                  multiple = TRUE
                                ),
                                br(), br(),
                                dropdownButton(
                                  h3("List of Input"),
                                  selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
                                  selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                                  sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                                  circle = TRUE, status = "green", icon = icon("gear"), width = "300px",
                                  tooltip = tooltipOptions(title = "Click to see inputs !")
                                )
                                )
                      )
                    )
                      ),
             tabPanel("Map", icon = icon("map-marker"),
                      fluidPage(
                        fluidRow(
                          column(12, style = "padding:0;",
                                 leafletOutput("map", width = "100%", height = "1000px"),
                                 # add custom zoom controls to right side of map
                                 absolutePanel(id = "zoom_map",
                                               bottom = "auto", right = 10, left = "auto", top = 10, width = "auto", height = "auto",
                                               actionButton("map_zoom_in", "", icon = icon("plus")),
                                               br(),
                                               actionButton("map_zoom_out", "", icon = icon("minus")),
                                               br(),
                                               actionButton("map_zoom_whole", "", icon = icon("globe"))
                                 ),
                                 # add map panel to left side of map
                                 uiOutput("mapPanel")
                          )
                        )
                      )
                ),
#             navbarMenu("More",
               tabPanel("Table", icon = icon("table"),
                        fluidPage(
                          fluidRow(
                            column(12, style = "padding-left:20px",
                              h2("Example datatable with buttons:"),
                              dataTableOutput("tab_SampleTable"),
                              # code to be able to add buttons to the table
                              tags$script("$(document).on('click', '#tab_SampleTable button', function () {
                                          Shiny.onInputChange('lastClickId',this.id);
                                          Shiny.onInputChange('lastClick', Math.random())
                                          });")
                            )
                          )
                        )
              ),
              tabPanel("Map and Table", icon = icon("map"),
                       fluidPage(
                         fluidRow(
                           column(12, style = "padding:0;",
                                  leafletOutput("map1", width = "100%", height = "600px"),
                                  # add custom zoom controls to right side of map
                                  absolutePanel(id = "zoom_map1",
                                                bottom = "auto", right = 10, left = "auto", top = 10, width = "auto", height = "auto",
                                                actionButton("map1_zoom_in", "", icon = icon("plus")),
                                                br(),
                                                actionButton("map1_zoom_out", "", icon = icon("minus")),
                                                br(),
                                                actionButton("map1_zoom_whole", "", icon = icon("globe"))
                                  )
                           )
                         ),
                         fluidRow(
                           column(12, 
                                  dataTableOutput("tab_SplitTable"),
                                  # code to be able to add buttons to the table
                                  tags$script("$(document).on('click', '#tab_SplitTable button', function () {
                                              Shiny.onInputChange('lastClickId',this.id);
                                              Shiny.onInputChange('lastClick', Math.random())
                                              });")
                                  )
                         #)
                       )
              )
             )
  ) 
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ################
  ## ABOUT PAGE ##
  ################
  
  # success alert
  observeEvent(input$btn_success, {
    sendSweetAlert(session, title = "Success!", text = "All ok", type = "success")
  })
  
  # error alert
  observeEvent(input$btn_error, {
    sendSweetAlert(session, title = "Error!", text = "Something went wrong", type = "error")
  })
  
  # confirm alert
  observeEvent(input$btn_confirm, {
    confirmSweetAlert(session, inputId = "input_confirm",
                      title = "Warning!", text = "Do you want to confirm?", type = "warning",
                      btn_labels = c("No", "Yes"))
  })
  
  # input alert
  observeEvent(input$btn_input, {
    inputSweetAlert(session, inputId = "input_somthing",
                    title = "Input!", text = "Please input something", type = "info",
                    btn_labels = c("Cancel", "OK"))
  })
  
  ##############
  ## MAP PAGE ##
  ##############
  
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 19, zoomControl = FALSE)) %>% 
      setView(144.9980, -37.8230, 10) %>%
      addTiles(group = "Street Map", 
               options = providerTileOptions(opacity = 0.4, maxNativeZoom = 19, maxZoom = 19)) %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "Satellite", 
                       options = providerTileOptions(opacity = 0.8, maxNativeZoom = 19, maxZoom = 19)) %>%
      ## ADD ARCGIS ONLINE DATA
      # addEsriFeatureLayer(
      #   url = paste0("https://services1.arcgis.com/5REoMU162diWrxzT/ArcGIS/rest/services/",
      #                "BusStop/FeatureServer/0"),
      #   useServiceSymbology = TRUE, 
      #   labelProperty = "STOPSPECNA",  labelOptions = labelOptions(textsize = "12px"),
      #   markerType = "circleMarker", color = "#c0b7af", opacity = 1
      # ) %>%
      # addEsriFeatureLayer(
      #   url = paste0("https://services1.arcgis.com/VVapzOPgBae5joyC/arcgis/rest/services/",
      #                "Contam_Field_Inspection_investigation_locations/FeatureServer/0"),
      #   useServiceSymbology = TRUE,
      #   labelProperty = "Location_Code",  labelOptions = labelOptions(textsize = "12px"),
      #   markerType = "circleMarker", stroke = FALSE, fillColor = "#c0b7af", fillOpacity = 1, weight = 3,
      #   options = featureLayerOptions(token = "8SUvlf7l1Tp1Z0BNjp01qa52hTdtrqAi3eDXocls51hRmIoAcFEU-TyBx9Bw85wg83mvmk8ONSxX7oPUuC4Di4xJGFVDTpoz5EYjRMW7IwP6fB2kpEqA0Fx91JKp3TseNU3hS-W2PqepyPhE9ZaNSA..")
      #   
      # ) %>%
      #addDrawToolbar() %>%
      addScaleBar(position = "bottomleft", scaleBarOptions(imperial = F)) %>%
      addLayersControl(baseGroups = c("Street Map", "Satellite"), 
                       options = layersControlOptions(collapsed = TRUE), 
                       position  = "bottomleft")
  })
  
  output$mapPanel <- renderUI({
    absolutePanel(id = "controls", class = "panel panel-default", 
                  top = 10, left = 20, right = "auto", bottom = "auto", width = 280, height = "auto",
                  h3("Legend"),
                  ## PROGRAM
                  # checkboxes with icons using custom colours in css
                  checkboxGroupInput("chk_prog", label = strong("Program"), choiceValues = list("prog1", "prog2"),
                                     choiceNames = list(span(icon("square", class = "icon-red"), "Program 1"),
                                                        span(icon("square", class = "icon-blue"), "Program 2")),
                                     selected = c("prog1", "prog2")
                  ),
                  div(id = "cctv",
                      div(style = "margin-top:10px;margin-left:20px;font-weight:bold;", "Observations"),
                      div(style = "margin-left:20px;", icon("camera"), "Point (click for photo)"),
                      div(style = "margin-left:20px;margin-bottom:20px;", icon("ellipsis-h"), "Continuous")),
                  bsTooltip("cctv", "zoom in to display observations", placement = "right"),
                  ## PACKAGES
                  checkboxGroupInput("chk_pkg", label = strong("Packages"), choiceValues = list("pkg1", "pkg2"),
                                     choiceNames = list(span(icon("square", class = "icon-pink"), "Package 1"),
                                                        span(icon("square", class = "icon-brightyellow"), "Package 2")),
                                     selected = FALSE),
                  ## checkbox group with embedded images (using a local image because icon is not available. if icon is available use code above)
                  tags$div(HTML('<div id="chk_grp_events" class="form-group shiny-input-checkboxgroup shiny-input-container shiny-input-container-inline">
                                <label class="control-label" for="chk_grp_events">Events</label>
                                <div class="shiny-options-group">
                                <label class="checkbox-inline">
                                <input type="checkbox" name="chk_grp_events" value="avoid"/>
                                <span><img src="glyphicons-2-leaf.png"  width="18px" height="18px"/>Type 1</span>
                                </label><br/>
                                <label class="checkbox-inline">
                                <input type="checkbox" name="chk_grp_events" value="unavoid"/>
                                <span><img src="glyphicons-3-dog.png" width="18px" height="18px"/>Type 2</span>
                                </label>
                                </div>
                                </div>')),
                  ## BACKGROUND LAYERS
                  shinyjs::disabled(div(id = "bkg_layers",
                                        div(style = "margin:0;line-height:20px;height:20px;", checkboxInput("chk_layers", label = strong("Background Layers"), value = TRUE)),
                                        div(style = "margin-top:0;margin-left:20px;", icon("square", class = "icon-yellow"), "Layer 1"),
                                        div(style = "margin-left:20px;", icon("square", class = "icon-orange"), "Layer 2")
                  )),
                  bsTooltip("bkg_layers", "zoom in to display layers", placement = "right")
    )
  })
  
  
  # example of scale dependent layers
  observe({
    if(is.null(input$map_zoom)){return()}
    if(input$map_zoom >= 17){
      shinyjs::enable("bkg_layers")
      removeTooltip(session, id="bkg_layers")
    } else {
      shinyjs::disable("bkg_layers")
      addTooltip(session, id="bkg_layers", title = "zoom in to display layers", placement = "left")
    }
  })
  
  ###############
  ## Functions ##
  ###############

  # Returns a polygon of the current map extent
  mapBounds <- function(){
    lng1 <- input$map_bounds$west
    lng2 <- input$map_bounds$east
    lat1 <- input$map_bounds$north
    lat2 <- input$map_bounds$south
    bb <- st_sfc(st_polygon(list(rbind(c(lng1, lat1), c(lng1, lat2), c(lng2, lat2), c(lng2, lat1), c(lng1, lat1)))))
    bb <- bb %>% st_set_crs(4326)
    return(bb)
  }
  
  #######################
  ## Map Zoom Controls ##
  #######################
  
  # Zoom control - zoom out
  observeEvent(input$map_zoom_out ,{
    leafletProxy("map") %>% 
      setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
              lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
              zoom = input$map_zoom - 1)
  })
  # Zoom control - zoom in
  observeEvent(input$map_zoom_in ,{
    leafletProxy("map") %>% 
      setView(lat  = (input$map_bounds$north + input$map_bounds$south) / 2,
              lng  = (input$map_bounds$east + input$map_bounds$west) / 2,
              zoom = input$map_zoom + 1)
  })
  # Zoom control - zoom to extent
  observeEvent(input$map_zoom_whole ,{
    leafletProxy("map") %>% setView(144.9980, -37.8230, 10)
  })
  
  
  ################
  ## TABLE PAGE ##
  ################
  
  
  output$tab_SampleTable <- renderDataTable({
    DT <- vals$Data
    DT[["Actions"]]<- paste0('<div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-orange edit" id=edit_',1:nrow(DT),'>Edit</button></div>')
    datatable(DT, 
              rownames = FALSE,
              escape = FALSE,
              options = list(#dom = "ft",
                             pageLength = nrow(DT),
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
              )) 
  })
  
  
  observeEvent(input$lastClick, {
    print(input$lastClickId)
    selectedRow <- as.numeric(strsplit(input$lastClickId, "_")[[1]][2])
    # edit button in SampleTable 
    if(grepl(x = input$lastClickId, pattern = "edit")){
      sendSweetAlert(session, title = "Edit", text = paste0("Clicked on: ", vals$Data[selectedRow, 1]))             
    }
    # view button in SplitTable
    if(grepl(x = input$lastClickId, pattern = "view")){
      sendSweetAlert(session, title = "View", text = paste0("Clicked on: ", vals$Data[selectedRow, 1]))             
    }
  })
    
  
  ########################
  ## MAP AND TABLE PAGE ##
  ########################
  
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 19, zoomControl = FALSE)) %>% 
      setView(144.9980, -37.8230, 10) %>%
      addTiles(group = "Street Map", 
               options = providerTileOptions(opacity = 0.4, maxNativeZoom = 19, maxZoom = 19)) %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "Satellite", 
                       options = providerTileOptions(opacity = 0.8, maxNativeZoom = 19, maxZoom = 19)) %>%
    addScaleBar(position = "bottomleft", scaleBarOptions(imperial = F)) %>%
      addLayersControl(baseGroups = c("Street Map", "Satellite"), 
                       options = layersControlOptions(collapsed = TRUE), 
                       position  = "bottomleft")
  })
  
  output$tab_SplitTable <- renderDataTable({
    DT <- vals$Data
    DT[["Actions"]]<- paste0('<div class="btn-group" role="group" aria-label="Basic example">
             <button type="button" class="btn btn-green view" id=view_',1:nrow(DT),'>View</button></div>')
    datatable(DT, 
              rownames = FALSE,
              escape = FALSE,
              options = list(#dom = "ft",
                pageLength = nrow(10),
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              )) 
  })
  
  
  
  #######################
  ## Map Zoom Controls ##
  #######################
  
  # Zoom control - zoom out
  observeEvent(input$map1_zoom_out ,{
    leafletProxy("map1") %>% 
      setView(lat  = (input$map1_bounds$north + input$map1_bounds$south) / 2,
              lng  = (input$map1_bounds$east + input$map1_bounds$west) / 2,
              zoom = input$map1_zoom - 1)
  })
  # Zoom control - zoom in
  observeEvent(input$map1_zoom_in ,{
    leafletProxy("map1") %>% 
      setView(lat  = (input$map1_bounds$north + input$map1_bounds$south) / 2,
              lng  = (input$map1_bounds$east + input$map1_bounds$west) / 2,
              zoom = input$map1_zoom + 1)
  })
  # Zoom control - zoom to extent
  observeEvent(input$map1_zoom_whole ,{
    leafletProxy("map1") %>% setView(144.9980, -37.8230, 10)
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)

