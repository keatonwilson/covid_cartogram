# pulling data from NYTimes github repo
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-02

# packages
require(tidyverse)
require(lubridate)

get_nytimes_covid_data = function(){
  # county data
  county_data = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  
  write_csv(county_data, "./data/covid_19_by_county.csv")
  
  # state data
  state_data = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
  write_csv(state_data, "./data/covid_19_by_state.csv")
  
  # printing most recent update
  recent_date = max(county_data$date)
  print(paste("Most recent data is from:", recent_date))
}

