# R Shiny Dashboard for Supplemental Homicide Reports - App

#====================================================================================================

source('R/ui.R', local = TRUE)
source('R/server.R')
source('R/global.R')

shinyApp(
  ui = shr_ui,
  server = shr_server
)



