# R Shiny Dashboard for Supplemental Homicide Reports - UI

#====================================================================================================

header <- dashboardHeader(title = 'FBI Supplementary Homicide Reports', dropdownMenuOutput("messageMenu"), titleWidth = 280)

sidebar <- dashboardSidebar(
  width = 280,
  sidebarMenu(
    
    # MAIN DATA SELECT
    selectInput( 'selected_category',
                 label = 'Data Category',
                 choices = c('Demographics',
                             'Victim/Offender Relationship',
                             'Weapon',
                             'Circumstances') ),
    
    # DEMOGRAPHIC PANEL
    conditionalPanel(
      condition = "input.selected_category == 'Demographics'",
      # Select state
      selectInput( 'selected_state_demog',
                   label = 'State',
                   choices = c('United States', sort(unique(main_df_wide[which(main_df_wide$State != 'United States'),'State']))) ),
      # Select year
      selectInput( 'selected_year_demog', label = 'Year', choices = c(2019:1985) ),
      # Select variables
      selectizeInput( 'selected_vars_demog',
                      label = 'Demographic Group',
                      choices = trend_inp_vars_demog_both,
                      multiple = T,
                      options = list(maxItems = 7,
                                     placeholder = 'Choose up to seven variables'),
                      selected = c('Total Offenders','Total Victims') ),
      # Select demographics
      radioButtons( 'selected_demog', label = 'Race/Sex',
                    choices = c('Race','Sex','Both'),
                    selected = 'Race', inline = TRUE )
    ),
    
    # VICTIM/OFFENDER RELATION PANEL
    conditionalPanel(
      condition = "input.selected_category == 'Victim/Offender Relationship'",
      # Select state
      selectInput( 'selected_state_vic_off',
                   label = 'State',
                   choices = c('United States', sort(unique(main_df_wide[which(main_df_wide$State != 'United States'),'State']))) ),
      # Select year
      selectInput( 'selected_year_vic_off', label = 'Year', choices = c(2019:1985) ),
      # Select variables
      selectizeInput( 'selected_vars_vic_off',
                      label = 'Victim Relation to Offender',
                      choices = trend_inp_vars_vic_off,
                      multiple = T,
                      options = list(maxItems = 7,
                                     placeholder = 'Choose up to seven variables'),
                      selected = c('Total Offenses','Unknown','Stranger') )
    ),
    
    # WEAPON PANEL
    conditionalPanel(
      condition = "input.selected_category == 'Weapon'",
      # Select state
      selectInput( 'selected_state_weapon',
                   label = 'State',
                   choices = c('United States', sort(unique(main_df_wide[which(main_df_wide$State != 'United States'),'State']))) ),
      # Select year
      selectInput( 'selected_year_weapon', label = 'Year', choices = c(2019:1985) ),
      # Select variables
      selectizeInput( 'selected_vars_weapon',
                      label = 'Weapon Used',
                      choices = trend_inp_vars_weap,
                      multiple = T,
                      options = list(maxItems = 7,
                                     placeholder = 'Choose up to seven variables'),
                      selected = c('Total Offenses','Handgun','Rifle') )
    ),
    
    # CIRCUMSTANCES PANEL
    conditionalPanel(
      condition = "input.selected_category == 'Circumstances'",
      # Select state
      selectInput( 'selected_state_circ',
                   label = 'State',
                   choices = c('United States', sort(unique(main_df_wide[which(main_df_wide$State != 'United States'),'State']))) ),
      # Select year
      selectInput( 'selected_year_circ', label = 'Year', choices = c(2019:1985) ),
      # Select variables
      selectizeInput( 'selected_vars_circ',
                      label = 'Circumstances',
                      choices = trend_inp_vars_circ,
                      multiple = T,
                      options = list(maxItems = 7,
                                     placeholder = 'Choose up to seven variables'),
                      selected = c('Total Offenses','Unknown') )
    )
    
  )
)

body <- dashboardBody(
  
  # CUSTOM CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  # TIME SERIES
  fluidRow(
    uiOutput('trend_box')
  ),
  
  # PIE CHART
  fluidRow(
    uiOutput('pie_box')
  ),
  
  # BAR CHART
  fluidRow(
    uiOutput('bar_box')
  ),
  
  # DATA TABLE
  fluidRow(
    uiOutput('table_box')
  )
  
)

shr_ui <- dashboardPage(
  header,
  sidebar,
  body
)



