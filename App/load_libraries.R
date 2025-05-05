# loads the libraries
# here so that the same libraries are loaded in the app and in the test space

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
library(glue)
library(lubridate)
library(scales)
library(readxl)
library(DT)
library(janitor)
library(shinycssloaders)
library(patchwork)
library(shinydashboard)
library(Kendall) # I don't think this is used any more
library(RobustLinearReg)
# library(fst) # for fast read
library(plotly) # I don't think this is used any more, in some unused functions though
library(openxlsx)
library(EnvStats) # the main package used for environmental statistical functions
# library(fitdistrplus) # call this explicitly because it overrides select- but perhaps use dplyr::select() explicitly
# library(NADA2) # call explicitly -- only NADA2::ATSmini() used
library(fst) # fst instead of a database for now
library(ggrepel) # text repel
library(ggiraph) # interactive ggplot
library(ggh4x) # extra ggplot2 functions
library(broom)


# # asynchonous functions
# library(future)
# plan(multisession)