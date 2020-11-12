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
                   href = "https://github.com/hunterrjohnson"),
                 badgeStatus = NULL,
                 icon = icon("info-circle fa-lg"),
                 headerText = "App Information"
    )
  })
  
  #=====TIME SERIES PLOT=====#
  output$trend_demog <- renderPlotly({
    
    # Filter to selected state
    df_trend <- main_df_long %>% filter(State == input$selected_state)
    
    # Filter plot based on variables selected
    if (length(input$selected_vars) == 0) {
      print("Select at least one group")
    }
    else {
      df_trend <- df_trend  %>%
        filter(Variable %in% input$selected_vars)
    }
    
    # If no input is selected, displays blank plot
    validate(
      need(input$selected_vars, "Nothing selected")
    )
    
    # Main ggplot object
    gg_obj <- ggplot(df_trend) +
      geom_line(aes(x = Year, y = Value, color = Variable,
                    group = 1, # Unclear why this is needed; necessary for custom hover text to work correctly
                    text = paste0(
                      "<b>", Variable, "</b>", "<br>",
                      "Year: ", Year, "<br>",
                      "Value: ", scales::comma(Value, 1), "<br>"
                    )
      )) +
      labs(x = '', y = '', title = '', color = '') +
      scale_y_continuous(label = comma) +
      scale_x_continuous(limits = c(1985,2020),
                         breaks = seq(1985,2020,5),
                         minor_breaks = seq(1985,2020,1)) +
      theme_minimal()
    
    # Convert ggplot object to plotly object
    ggplotly(gg_obj, tooltip = c('text')) %>%
      layout(hovermode = 'compare',
             legend = list(
               title = list(text='<b> Groups </b>',
                            font = list(size = 12,
                                        family = 'sans-serif')),
               font = list(
                 family = "sans-serif",
                 size = 12,
                 color = "#000"),
               bgcolor = "#ececec",
               bordercolor = "#FFFFFF",
               borderwidth = 2))
  })
  
  # Render box dynamically so box title changes when state input is changed
  output$trend_box <- renderUI({
    plot_title <- paste0(input$selected_state, ' Homicides, 1985 - 2019')
    box(width = 12, solidHeader = T, status = 'primary',
        title = plot_title,
        footer = 'Note: participation in FBI Supplementary Homicide Reports (SHR) varies by state and year. For example, Florida no longer participates in SHR reporting',
        plotlyOutput('trend_demog')
    )
  })

  #=====US MAP=====#
  output$us_map <- renderLeaflet({
    leaf_map # Defined in global
  })
  
  # Capture the info of the clicked polygon; use to control state select dropdown which controls all uses of state input
  click_state <- reactiveVal()
  observeEvent(input$us_map_shape_click, {
    if(!is.null(click_state()) && click_state() == input$us_map_shape_click$id) {
      click_state(NULL) # Reset filter
      updateSelectInput(session, 'selected_state', selected = 'United States')
    }
    else {
      click_state(input$us_map_shape_click$id)
      updateSelectInput(session, 'selected_state', selected = input$us_map_shape_click$id)
    }
  })
  
  #=====Pie Charts=====#
  output$pie_box <- renderUI({
    # Get correct box title based on state input
    plot_title_pie <- paste0(input$selected_state, ' Demographics, ', input$selected_year)
    box(
      width = 8, collapsible = T,
      title = plot_title_pie,
      solidHeader = T, status = 'primary',
      splitLayout(
        plotlyOutput('pies')
      )
    )
  })
  
  output$pies <- renderPlotly({
    # Filter Census pie chart by state
    census_pie <- census_df %>% filter(State == input$selected_state)
    # Get correct Census year based on year selection
    if (!is.null(input$selected_year)) {
      census_year <- ifelse(input$selected_year %in% c(2010:2020), 2010,
                            ifelse(input$selected_year %in% c(2000:2009), 2000,
                                   ifelse(input$selected_year %in% c(1990:1999), 1990,
                                          ifelse(input$selected_year %in% c(1980:1989), 1980, input$selected_year))))
      census_pie <- census_pie %>% filter(Year == census_year)
    }
    # Filter offender pie chart by state and year
    offender_pie <- main_df_offenders %>% filter(State == input$selected_state,
                                                 Year == input$selected_year)
    # Filter victim pie chart by state and year
    victim_pie <- main_df_victims %>% filter(State == input$selected_state,
                                             Year == input$selected_year)
    # Pie variables react to radio button input
    if(input$selected_demog %in% 'Race') {
      # Census
      customOrder_census <- customOrder_census_race
      census_pie <- census_pie %>% slice(match(customOrder_census, Variable))
      census_pie <- census_pie %>% filter(Variable %in% customOrder_census_race)
      # Offenders
      customOrder_off <- customOrder_off_race
      offender_pie <- offender_pie %>% slice(match(customOrder_off, Variable))
      offender_pie <- offender_pie %>% filter(Variable %in% customOrder_off_race)
      # Victims
      customOrder_vic <- customOrder_vic_race
      victim_pie <- victim_pie %>% slice(match(customOrder_vic, Variable))
      victim_pie <- victim_pie %>% filter(Variable %in% customOrder_vic_race)
    }
    if(input$selected_demog %in% 'Sex') {
      # Census
      customOrder_census <- customOrder_census_sex
      census_pie <- census_pie %>% slice(match(customOrder_census, Variable))
      census_pie <- census_pie %>% filter(Variable %in% customOrder_census_sex)
      # Offenders
      customOrder_off <- customOrder_off_sex
      offender_pie <- offender_pie %>% slice(match(customOrder_off, Variable))
      offender_pie <- offender_pie %>% filter(Variable %in% customOrder_off_sex)
      # Victims
      customOrder_vic <- customOrder_vic_sex
      victim_pie <- victim_pie %>% slice(match(customOrder_vic, Variable))
      victim_pie <- victim_pie %>% filter(Variable %in% customOrder_vic_sex)
    }
    if(input$selected_demog %in% 'Both') {
      # Census
      customOrder_census <- customOrder_census_both
      census_pie <- census_pie %>% slice(match(customOrder_census, Variable))
      census_pie <- census_pie %>% filter(Variable %in% customOrder_census_both)
      # Offenders
      customOrder_off <- customOrder_off_both
      offender_pie <- offender_pie %>% slice(match(customOrder_off, Variable))
      offender_pie <- offender_pie %>% filter(Variable %in% customOrder_off_both)
      # Victims
      customOrder_vic <- customOrder_vic_both
      victim_pie <- victim_pie %>% slice(match(customOrder_vic, Variable))
      victim_pie <- victim_pie %>% filter(Variable %in% customOrder_vic_both)
    }
    # Custom palette colors
    colors_pie_census <- c('#b9e5ec','#ff5353','#375ec5','#ffe98f','#55a157')
    colors_pie <- c('#b9e5ec','#ff5353','#375ec5','#ffe98f','#d2badb')
    # Start with Census pie and add offender/victim pies as traces
    plot_ly(labels = ~Variable, values = ~Value,
            textposition = 'inside', textinfo = 'label+percent',
            insidetextfont = list(color = '#000000'),
            marker = list(line = list(color = '#000000', width = 1))) %>%
      add_pie(data = census_pie,
              marker = list(colors = colors_pie_census),
              title = paste0('Population (',census_year,' Census)'), name = '', domain = list(row = 0, column = 0)) %>%
      add_pie(data = offender_pie, 
              marker = list(colors = colors_pie),
              title = 'Homicide Offenders', domain = list(row = 0, column = 1)) %>%
      add_pie(data = victim_pie,
              marker = list(colors = colors_pie),
              title = 'Homicide Victims', domain = list(row = 0, column = 2)) %>%
      layout(title = '', showlegend = T,
             grid = list(rows = 1, columns = 3),
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             legend = list(y = 0.5,
                           title = list(text='<b> Demographics </b>')),
             hoverlabel = list(align = 'left',
                               color = '#55a157'))
  })
  
  #=====DATA TABLE=====#
  output$shr_table <- DT::renderDataTable({
    # Filter data table based on radio button input
    if(input$selected_demog %in% 'Race') {
      filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_race)]
    }
    if(input$selected_demog %in% 'Sex') {
      filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_sex)]
    }
    if(input$selected_demog %in% 'Both') {
      filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_both)]
    }
    # Table
    DT::datatable(filtered_data,
                  options = list(lengthMenu = c(10, 25, 50, 100),
                                 pageLength = 10,
                                 autowidth = TRUE,
                                 columnDefs = list(list(classVariable = 'dt-center', 
                                                        className = 'dt-center', 
                                                        searchable = TRUE,
                                                        targets = '_all')),
                                 scrollX = TRUE)
    )
  })
  
  #=====Download CSV=====#
  output$downloadData <- downloadHandler(
    filename = function() { 'shr_data.csv' }, content = function(file) {
      # Filter data table based on radio button input
      if(input$selected_demog %in% 'Race') {
        filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_race)]
      }
      if(input$selected_demog %in% 'Sex') {
        filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_sex)]
      }
      if(input$selected_demog %in% 'Both') {
        filtered_data <- main_df_wide[,c('State','Year',plotly_input_vars_both)]
      }
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
}



