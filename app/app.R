# R Shiny Dashboard for Supplemental Homicide Reports - App

#====================================================================================================

source('ui.R', local = TRUE)
source('server.R')
source('global.R')

shinyApp(
  ui = shr_ui,
  server = shr_server
)



