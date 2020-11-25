# R Shiny Dashboard for Supplemental Homicide Reports - Functions

#====================================================================================================

# BOX FUNCTIONS
trend_box_function <- function(title) {
  box(width = 12, solidHeader = T, status = 'primary',
      title = title,
      footer = 'Note: participation in FBI Supplementary Homicide Reports (SHR) varies by state and year. For example, Florida no longer participates in SHR reporting',
      plotlyOutput('trend_plot')
  )
}

bar_box_function <- function(title) {
  box(
    width = 12, collapsible = T,
    title = title,
    solidHeader = T, status = 'primary',
    plotlyOutput('bar_output')
  )
}

# TABLE FUNCTION
dt_function <- function(data) {
  DT::datatable(data,
                options = list(lengthMenu = c(10, 25, 50, 100),
                               pageLength = 10,
                               autowidth = TRUE,
                               columnDefs = list(list(classVariable = 'dt-center', 
                                                      className = 'dt-center', 
                                                      searchable = TRUE,
                                                      targets = '_all')),
                               scrollX = TRUE)
  )
}

# PIE FUNCTION
pie_function <- function(census_data, off_data, vic_data, state_inp, year_inp, demog_inp) {
  # Filter Census pie chart by state
  census_pie <- census_data %>% filter(State == state_inp)
  # Get correct Census year based on year selection
  if (!is.null(year_inp)) {
    census_year <- ifelse(year_inp %in% c(2010:2020), 2010,
                          ifelse(year_inp %in% c(2000:2009), 2000,
                                 ifelse(year_inp %in% c(1990:1999), 1990,
                                        ifelse(year_inp %in% c(1980:1989), 1980, year_inp))))
    census_pie <- census_pie %>% filter(Year == census_year)
  }
  # Filter offender pie chart by state and year
  offender_pie <- off_data %>% filter(State == state_inp,
                                      Year == year_inp)
  # Filter victim pie chart by state and year
  victim_pie <- vic_data %>% filter(State == state_inp,
                                    Year == year_inp)
  # Pie variables react to radio button input
  if(demog_inp %in% 'Race') {
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
  if(demog_inp %in% 'Sex') {
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
  if(demog_inp %in% 'Both') {
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
}

# TREND FUNCTION
trend_function <- function(data, state_inp, vars_inp) {
  # Filter to selected state
  df_trend <- data %>% filter(State == state_inp)
  
  # Filter plot based on variables selected
  if (length(vars_inp) == 0) {
    print("Select at least one group")
  }
  else {
    df_trend <- df_trend  %>%
      filter(Variable %in% vars_inp)
  }
  
  # If no input is selected, displays blank plot
  validate(
    need(vars_inp, "Nothing selected")
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
}



