# R Shiny Dashboard for Supplemental Homicide Reports - Global

lapply(c('data.table','dplyr','ggplot2','shiny','shinydashboard',
         'shinythemes','leaflet','DT','plotly','scales'), library, character.only = TRUE)

setwd('/Users/hunterjohnson/Desktop/Dashboards/US Crime/Supplementary Homicide Reports/')

#====================================================================================================
# Read in data

# Main SHR data (long for plotly, wide for datatable)
main_df_long <- data.frame(fread('Input/shr_analysis_data_long.csv'))
main_df_wide <- data.frame(fread('Input/shr_analysis_data_wide.csv'), check.names = FALSE) # Keep invalid names

# Create offender/victim subsets of main_df_long for pie charts
main_df_offenders <- main_df_long[grep('Offenders', main_df_long$Variable),]
main_df_offenders$Variable <- gsub(' Offenders', '', main_df_offenders$Variable)
main_df_offenders$Variable <- gsub('Offenders, ', '', main_df_offenders$Variable)
main_df_victims <- main_df_long[grep('Victims', main_df_long$Variable),]
main_df_victims$Variable <- gsub(' Victims', '', main_df_victims$Variable)
main_df_victims$Variable <- gsub('Victims, ', '', main_df_victims$Variable)

# U.S. states shapefile
states_shp <- sf::read_sf('Input/us_shapefiles/cb_2018_us_state_500k.shp')
# Remove territories
states_shp <- states_shp[-which(states_shp$NAME %in% c('American Samoa','Puerto Rico','United States Virgin Islands',
                                                       'Guam','Commonwealth of the Northern Mariana Islands')), ]

# Census data
census_df <- data.frame(fread('Input/census_data.csv'))

#====================================================================================================
# Create arrays of Variables for use in UI and server

# Order Variables for selectizeInput
plotly_input_vars_both <- c('Total Offenders','Total Victims','Male Offenders','Male Victims','Female Offenders','Female Victims',
                            'Black Offenders','Black Victims','White Offenders','White Victims',
                            'Asian/Pacific Islander Offenders','Asian/Pacific Islander Victims',
                            'Native American Offenders','Native American Victims','Black Male Offenders','Black Male Victims',
                            'Black Female Offenders','Black Female Victims','White Male Offenders','White Male Victims',
                            'White Female Offenders','White Female Victims',
                            'Asian/Pacific Islander Male Offenders','Asian/Pacific Islander Male Victims',
                            'Asian/Pacific Islander Female Offenders','Asian/Pacific Islander Female Victims',
                            'Native American Male Offenders','Native American Male Victims',
                            'Native American Female Offenders','Native American Female Victims')
plotly_input_vars_race <- c('Total Offenders','Total Victims','Black Offenders','Black Victims','White Offenders','White Victims',
                            'Asian/Pacific Islander Offenders','Asian/Pacific Islander Victims',
                            'Native American Offenders','Native American Victims')
plotly_input_vars_sex <- c('Total Offenders','Total Victims','Male Offenders','Male Victims','Female Offenders','Female Victims')

# Ordered Variables for pie chart selections
customOrder_census_race <- c('Black','White','Asian/Pacific Islander','Native American','Other')
customOrder_off_race <- c('Black','White','Asian/Pacific Islander','Native American','Race Unknown')
customOrder_vic_race <- c('Black','White','Asian/Pacific Islander','Native American','Race Unknown')
customOrder_census_sex <- c('Male','Female')
customOrder_off_sex <- c('Male','Female','Sex Unknown')
customOrder_vic_sex <- c('Male','Female','Sex Unknown')
customOrder_census_both <- c('Black Male','Black Female','White Male','White Female','Native American Male',
                             'Native American Female','Asian/Pacific Islander Male','Asian/Pacific Islander Female',
                             'Male, Other','Female, Other')
customOrder_off_both <- c('Black Male','Black Female','White Male','White Female',
                          'Native American Male','Native American Female','Asian/Pacific Islander Male',
                          'Asian/Pacific Islander Female','Male, Race Unknown','Female, Race Unknown')
customOrder_vic_both <- c('Black Male','Black Female','White Male','White Female',
                          'Native American Male','Native American Female','Asian/Pacific Islander Male',
                          'Asian/Pacific Islander Female','Male, Race Unknown','Female, Race Unknown')

#====================================================================================================
# Leaflet map

# Popup labels
labels <- sprintf(
  "<strong>%s</strong>",
  states_shp$NAME
) %>% lapply(htmltools::HTML)

pal <- colorFactor(
  palette = c('red', 'blue', 'green', 'purple', 'orange'),
  domain = states_shp$NAME
)

# Map
leaf_map <- leaflet(states_shp) %>% 
  addProviderTiles(providers$OpenStreetMap) %>%
  addPolygons(layerId = ~NAME,
              weight = 1,
              opacity = 0.6,
              color = 'black',
              fillColor = ~pal(NAME),
              label = labels) %>%
  setView(lat = 39.50, lng = -98.35, zoom = 3) # US Centroid



