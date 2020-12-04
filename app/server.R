# R Shiny Dashboard for Supplemental Homicide Reports - Server

#====================================================================================================

shr_server <- function(input, output, session) {
  
  #=====HEADER LINK=====#
  
  icon_g <- icon("github")
  icon_g[["attribs"]][["class"]] <- "fa fa-github"
  output$messageMenu <- renderMenu({
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Project in Github",
                   message = "Documentation, Source, Citation",
                   icon = icon_g,
                   href = "https://github.com/hunterrjohnson/fbi-shr-dashboard"),
                 badgeStatus = NULL,
                 icon = icon("info-circle fa-lg"),
                 headerText = "App Information"
    )
  })
  
  #=====UPDATE INPUTS=====#
  
  # STATE
  observe({
    x <- input$selected_state_demog
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_state_vic_off", selected = x)
    updateSelectInput(session, "selected_state_weapon", selected = x)
    updateSelectInput(session, "selected_state_circ", selected = x)
  })
  observe({
    x <- input$selected_state_vic_off
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_state_demog", selected = x)
    updateSelectInput(session, "selected_state_weapon", selected = x)
    updateSelectInput(session, "selected_state_circ", selected = x)
  })
  observe({
    x <- input$selected_state_weapon
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_state_demog", selected = x)
    updateSelectInput(session, "selected_state_vic_off", selected = x)
    updateSelectInput(session, "selected_state_circ", selected = x)
  })
  observe({
    x <- input$selected_state_circ
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_state_demog", selected = x)
    updateSelectInput(session, "selected_state_vic_off", selected = x)
    updateSelectInput(session, "selected_state_weapon", selected = x)
  })
  
  # YEAR
  observe({
    x <- input$selected_year_demog
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_year_vic_off", selected = x)
    updateSelectInput(session, "selected_year_weapon", selected = x)
    updateSelectInput(session, "selected_year_circ", selected = x)
  })
  observe({
    x <- input$selected_year_vic_off
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_year_demog", selected = x)
    updateSelectInput(session, "selected_year_weapon", selected = x)
    updateSelectInput(session, "selected_year_circ", selected = x)
  })
  observe({
    x <- input$selected_year_weapon
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_year_demog", selected = x)
    updateSelectInput(session, "selected_year_vic_off", selected = x)
    updateSelectInput(session, "selected_year_circ", selected = x)
  })
  observe({
    x <- input$selected_year_circ
    if (is.null(x))
      x <- character(0)
    updateSelectInput(session, "selected_year_demog", selected = x)
    updateSelectInput(session, "selected_year_vic_off", selected = x)
    updateSelectInput(session, "selected_year_weapon", selected = x)
  })
  
  #=====TIME SERIES PLOT=====#
  
  # BOX
  output$trend_box <- renderUI({
    if(input$selected_category == "Demographics") {
      plot_title <- paste0('Homicide Offenders/Victims by Demographic Group, ', input$selected_state_demog, ' 1985 - 2019')
      trend_box_function(title = plot_title)
    }
    else if(input$selected_category == "Victim/Offender Relationship") {
      plot_title <- paste0('Homicide Offenses by Victim Relation to Offender, ', input$selected_state_vic_off, ' 1985 - 2019')
      trend_box_function(title = plot_title)
    }
    else if(input$selected_category == "Weapon") {
      plot_title <- paste0('Homicide Offenses by Weapon Used, ', input$selected_state_weapon, ' 1985 - 2019')
      trend_box_function(title = plot_title)
    }
    else if(input$selected_category == "Circumstances") {
      plot_title <- paste0('Homicide Offenses by Circumstances, ', input$selected_state_circ, ' 1985 - 2019')
      trend_box_function(title = plot_title)
    }
  })
  
  # PLOT
  output$trend_plot <- renderPlotly({
    if(input$selected_category == "Demographics") {
      trend_function(data = main_df_long, state_inp = input$selected_state_demog, vars_inp = input$selected_vars_demog)
    }
    else if(input$selected_category == "Victim/Offender Relationship") {
      trend_function(data = circum_df, state_inp = input$selected_state_vic_off, vars_inp = input$selected_vars_vic_off)
    }
    else if(input$selected_category == "Weapon") {
      trend_function(data = weapon_df, state_inp = input$selected_state_weapon, vars_inp = input$selected_vars_weapon)
    }
    else if(input$selected_category == "Circumstances") {
      trend_function(data = reason_df, state_inp = input$selected_state_circ, vars_inp = input$selected_vars_circ)
    }
  })
  
  #=====PIE CHART=====#
  
  # BOX
  output$pie_box <- renderUI({
    if(input$selected_category == "Demographics") {
      # Get correct box title based on state input
      plot_title_pie <- paste0(input$selected_state_demog, ' Demographics, ', input$selected_year_demog)
      box(width = 12,
        title = plot_title_pie,
        solidHeader = T, status = 'primary',
        splitLayout(
          plotlyOutput('pies')
        )
      )
    }
  })
  
  # CHART
  output$pies <- renderPlotly({
    if(input$selected_category == "Demographics") {
      pie_function(census_data = census_df, off_data = main_df_offenders, vic_data = main_df_victims, 
                   state_inp = input$selected_state_demog, year_inp = input$selected_year_demog, demog_inp = input$selected_demog)
    }
  })
  
  #=====BAR CHART=====#
  
  # BOX
  output$bar_box <- renderUI({
    if(input$selected_category == "Victim/Offender Relationship") {
      plot_title_bar <- paste0("Homicide Offenses by Victim Relation to Offender, ", input$selected_state_vic_off, ' ', input$selected_year_vic_off)
      bar_box_function(plot_title_bar)
    }
    else if(input$selected_category == "Weapon") {
      plot_title_bar <- paste0("Homicide Offenses by Weapon Used, ", input$selected_state_weapon, ' ', input$selected_year_weapon)
      bar_box_function(plot_title_bar)
    }
    else if(input$selected_category == "Circumstances") {
      plot_title_bar <- paste0("Homicide Offenses by Circumstances, ", input$selected_state_circ, ' ', input$selected_year_circ)
      bar_box_function(plot_title_bar)
    }
  })
  
  # CHART
  output$bar_output <- renderPlotly({
    if(input$selected_category == "Victim/Offender Relationship") {
      bar_df <- circum_df %>% filter(State == input$selected_state_vic_off,
                                     Year == input$selected_year_vic_off,
                                     Variable %in% bar_inp_vars_vic_off)
      plot_ly(bar_df, x = ~Variable, y = ~Value, color = ~Variable, type = 'bar') %>%
        layout(xaxis = list(title = '', tickangle = 300), yaxis = list(title = ''),
               legend = list(y = 0.5, title = list(text='<b> Relation </b>')))
    }
    else if(input$selected_category == "Weapon") {
      bar_df <- weapon_df %>% filter(State == input$selected_state_weapon,
                                     Year == input$selected_year_weapon,
                                     Variable %in% bar_inp_vars_weap_bar)
      plot_ly(bar_df, x = ~Variable, y = ~Value, color = ~Variable, type = 'bar') %>%
        layout(xaxis = list(title = '', tickangle = 300), yaxis = list(title = ''),
               legend = list(y = 0.5, title = list(text='<b> Weapon </b>')))
    }
    else if(input$selected_category == "Circumstances") {
      bar_df <- reason_df %>% filter(State == input$selected_state_circ,
                                     Year == input$selected_year_circ,
                                     Variable %in% bar_inp_vars_circ)
      plot_ly(bar_df, x = ~Variable, y = ~Value, color = ~Variable, type = 'bar') %>%
        layout(xaxis = list(title = '', tickangle = 300), yaxis = list(title = ''),
               legend = list(y = 0.5, title = list(text='<b> Circumstances </b>')))
    }
  })
  
  #=====DATA TABLE=====#
  
  # BOX
  output$table_box <- renderUI({
    if(input$selected_category == "Demographics") {
      box(width = 12,
        title = "Table of Homicide Offenders/Victims by Demographic Group",
        solidHeader = T, status = 'primary',
        dataTableOutput('shr_table'),
        downloadButton("downloadData", "Download")
      )
    }
    else if(input$selected_category == "Victim/Offender Relationship") {
      box(width = 12,
        title = "Table of Homicide Offenses by Victim Relation to Offender",
        solidHeader = T, status = 'primary',
        dataTableOutput('shr_table'),
        downloadButton("downloadData", "Download")
      )
    }
    else if(input$selected_category == "Weapon") {
      box(width = 12,
        title = "Table of Homicide Offenses by Weapon Used",
        solidHeader = T, status = 'primary',
        dataTableOutput('shr_table'),
        downloadButton("downloadData", "Download")
      )
    }
    else if(input$selected_category == "Circumstances") {
      box(width = 12,
        title = "Table of Homicide Offenses by Circumstances",
        solidHeader = T, status = 'primary',
        dataTableOutput('shr_table'),
        downloadButton("downloadData", "Download")
      )
    }
  })
  
  # TABLE
  output$shr_table <- DT::renderDataTable({
    if(input$selected_category == "Demographics") {
      # Filter data table based on radio button input
      if(input$selected_demog %in% 'Race') filtered_data <- main_df_wide[,c('State','Year',trend_inp_vars_demog_race)]
      else if(input$selected_demog %in% 'Sex') filtered_data <- main_df_wide[,c('State','Year',trend_inp_vars_demog_sex)]
      else if(input$selected_demog %in% 'Both') filtered_data <- main_df_wide[,c('State','Year',trend_inp_vars_demog_both)]
      dt_function(data = filtered_data)
    }
    else if(input$selected_category == "Victim/Offender Relationship") dt_function(data = circum_df_wide)
    else if(input$selected_category == "Weapon") dt_function(data = weapon_df_wide)
    else if(input$selected_category == "Circumstances") dt_function(data = reason_df_wide)
  })
  
  # Download CSV
  output$downloadData <- downloadHandler(
    filename = function() { 'shr_data.csv' }, content = function(file) {
      # Filter data table based on radio button input
      if(input$selected_demog %in% 'Race') {
        filtered_data <- main_df_wide[,c('State','Year', trend_inp_vars_race)]
      }
      else if(input$selected_demog %in% 'Sex') {
        filtered_data <- main_df_wide[,c('State','Year', trend_inp_vars_sex)]
      }
      else if(input$selected_demog %in% 'Both') {
        filtered_data <- main_df_wide[,c('State','Year', trend_inp_vars_both)]
      }
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
}



