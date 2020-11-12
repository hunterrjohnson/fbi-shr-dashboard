# R Shiny Dashboard for Supplemental Homicide Reports - UI

#====================================================================================================

header <- dashboardHeader(title = 'U.S. Homicide Demographics', dropdownMenuOutput("messageMenu"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    # Select state
    selectInput( 'selected_state',
                 label = 'Select State',
                 choices = c('United States', sort(unique(states_shp$NAME))) ),
    # Select variables
    selectizeInput( 'selected_vars',
                    label = 'Select Variables',
                    choices = plotly_input_vars_both,
                    multiple = T,
                    options = list(maxItems = 7,
                                   placeholder = 'Choose up to seven variables'),
                    selected = c('Total Offenders','Total Victims') ),
    # Select year
    selectInput( 'selected_year', label = 'Select Year', choices = c(2019:1985) ),
    
    # Select demographics
    radioButtons( 'selected_demog', label = 'Select Race/Sex',
                  choices = c('Race','Sex','Both'),
                  selected = 'Race', inline = TRUE )
  )
)

body <- dashboardBody(
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # Time series
  fluidRow(
    uiOutput('trend_box')
  ),
  
  # U.S. map and pie charts
  fluidRow(
    box(width = 4, solidHeader = T, status = 'primary',
        title = 'U.S. Map',
        leafletOutput('us_map')
    ),
    uiOutput('pie_box')
  ),
  
  # Data table
  fluidRow(
    box(
      width = 12, collapsible = T,
      title = paste0('Table of U.S. Homicides, 1985 - 2019'),
      solidHeader = T, status = 'primary',
      dataTableOutput('shr_table'),
      downloadButton("downloadData", "Download")
    )
  )
  
)

shr_ui <- dashboardPage(
  header,
  sidebar,
  body
)



