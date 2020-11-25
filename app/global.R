# R Shiny Dashboard for Supplemental Homicide Reports - Global

lapply(c('data.table','dplyr','ggplot2','shiny','shinydashboard',
         'shinythemes','leaflet','DT','plotly','scales'), library, character.only = TRUE)

source('R/App/functions.R')

#====================================================================================================
# Read in data

# Offender/victim data (long for plotly, wide for datatable)
main_df_long <- data.frame(fread('Input/shr_analysis_data_long.csv'))
main_df_wide <- data.frame(fread('Input/shr_analysis_data_wide.csv'), check.names = FALSE) # Keep invalid names

# Create offender/victim subsets of main_df_long for pie charts
main_df_offenders <- main_df_long[grep('Offenders', main_df_long$Variable),]
main_df_offenders$Variable <- gsub(' Offenders', '', main_df_offenders$Variable)
main_df_offenders$Variable <- gsub('Offenders, ', '', main_df_offenders$Variable)
main_df_victims <- main_df_long[grep('Victims', main_df_long$Variable),]
main_df_victims$Variable <- gsub(' Victims', '', main_df_victims$Variable)
main_df_victims$Variable <- gsub('Victims, ', '', main_df_victims$Variable)

# Circumstances data
circum_df <- data.frame(fread('Input/shr_circumstances_analysis_data_long.csv'))
circum_df_wide <- data.frame(fread('Input/shr_circumstances_analysis_data_wide.csv'), check.names = FALSE)

# Weapons data
weapon_df <- data.frame(fread('Input/shr_weapon_analysis_data_long.csv'))
weapon_df_wide <- data.frame(fread('Input/shr_weapon_analysis_data_wide.csv'), check.names = FALSE)

# Reasons data
reason_df <- data.frame(fread('Input/shr_reason_analysis_data_long.csv'))
reason_df_wide <- data.frame(fread('Input/shr_reason_analysis_data_wide.csv'), check.names = FALSE)

# Census data
census_df <- data.frame(fread('Input/census_data.csv'))

#====================================================================================================
# Create arrays of Variables for use in UI and server

# Order variables for selectizeInput - demographics tab
trend_inp_vars_demog_both <- c('Total Offenders','Total Victims','Male Offenders','Male Victims','Female Offenders','Female Victims',
                               'Black Offenders','Black Victims','White Offenders','White Victims',
                               'Asian/Pacific Islander Offenders','Asian/Pacific Islander Victims',
                               'Native American Offenders','Native American Victims','Black Male Offenders','Black Male Victims',
                               'Black Female Offenders','Black Female Victims','White Male Offenders','White Male Victims',
                               'White Female Offenders','White Female Victims',
                               'Asian/Pacific Islander Male Offenders','Asian/Pacific Islander Male Victims',
                               'Asian/Pacific Islander Female Offenders','Asian/Pacific Islander Female Victims',
                               'Native American Male Offenders','Native American Male Victims',
                               'Native American Female Offenders','Native American Female Victims')
trend_inp_vars_demog_race <- c('Total Offenders','Total Victims','Black Offenders','Black Victims','White Offenders','White Victims',
                               'Asian/Pacific Islander Offenders','Asian/Pacific Islander Victims',
                               'Native American Offenders','Native American Victims')
trend_inp_vars_demog_sex <- c('Total Offenders','Total Victims','Male Offenders','Male Victims','Female Offenders','Female Victims')

# Order variables for selectizeInput - victim/offender tab
trend_inp_vars_vic_off <- c('Total Offenses','Friend','Acquaintance','Neighbor','Stranger','Employer','Employee',
                            'Girlfriend','Boyfriend','Wife','Husband','Homosexual Partner','Ex-Wife','Ex-Husband',
                            'Common-Law Wife','Common-Law Husband','Daughter','Son','Sister','Brother','Mother','Father',
                            'Stepdaughter','Stepson','Stepmother','Stepfather','In-Law','Other Family','Other','Unknown')
bar_inp_vars_vic_off <- c('Friend','Acquaintance','Neighbor','Stranger','Employer','Employee',
                          'Girlfriend','Boyfriend','Wife','Husband','Homosexual Partner','Ex-Wife','Ex-Husband',
                          'Common-Law Wife','Common-Law Husband','Daughter','Son','Sister','Brother','Mother','Father',
                          'Stepdaughter','Stepson','Stepmother','Stepfather','In-Law','Other Family','Other','Unknown')

# Order variables for selectizeInput - weapon tab
trend_inp_vars_weap <- c('Total Offenses','Handgun','Shotgun','Rifle','Firearm, Other','Firearm, Type Unknown',
                         'Personal Weapons (Includes Beating)','Blunt Object','Strangulation','Drowning',
                         'Pushed/Thrown Out of Window','Knife/Cutting Instrument','Asphyxiation',
                         'Poison','Narcotics/Drugs','Fire','Explosives','Other')
bar_inp_vars_weap_bar <- c('Handgun','Shotgun','Rifle','Firearm, Other','Firearm, Type Unknown',
                           'Personal Weapons (Includes Beating)','Blunt Object','Strangulation','Drowning',
                           'Pushed/Thrown Out of Window','Knife/Cutting Instrument','Asphyxiation',
                           'Poison','Narcotics/Drugs','Fire','Explosives','Other')

# Order variables for selectizeInput - circumstances tab
trend_inp_vars_circ <- c('Total Offenses','Argument Over Money/Property','Robbery','Burglary','Larceny','Motor Vehicle Theft',
                         'Felon Killed by Police','Felon Killed by Private Citizen',"Lovers' Triangle",'Brawl Involving Alcohol','Brawl Involving Narcotics',
                         'Gangland Killing','Juvenile Gang Killing','Rape','Prostitution/Commercialized Vice','Other Sex Offense',
                         'Gambling','Narcotic/Drug Laws','Arson','Institutional Killing','Sniper Attack','Child Killed by Babysitter',
                         'All Suspected Felony Type','Other Felony, Not Specified','Other Non-Felony, Not Specified','Unknown')
bar_inp_vars_circ <- c('Argument Over Money/Property','Robbery','Burglary','Larceny','Motor Vehicle Theft',
                       'Felon Killed by Police','Felon Killed by Private Citizen',"Lovers' Triangle",'Brawl Involving Alcohol','Brawl Involving Narcotics',
                       'Gangland Killing','Juvenile Gang Killing','Rape','Prostitution/Commercialized Vice','Other Sex Offense',
                       'Gambling','Narcotic/Drug Laws','Arson','Institutional Killing','Sniper Attack','Child Killed by Babysitter',
                       'All Suspected Felony Type','Other Felony, Not Specified','Other Non-Felony, Not Specified','Unknown')

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



