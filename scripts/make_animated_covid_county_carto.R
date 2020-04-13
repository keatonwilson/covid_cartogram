# cartogram mapping function
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-03

# packages
require(tidyverse)
require(sf)
require(usmap)
require(ggmap)
require(Rcartogram)
require(getcartr)
require(sfheaders)
require(maptools)
require(FRK)
require(spdplyr)
require(cartogram)
require(mcla)
require(parallel)
source("./scripts/get_nytimes_covid_data.R")
require(gganimate)
require(purrr)
require(transformr)
require(ggthemes)
require(ggrepel)
require(viridis)


make_animated_county_carto = function(){
  # pulling most recent data
  get_nytimes_covid_data()

  
  # loading covid data and joining to county data
  covid_data = read_csv("./data/covid_19_by_county.csv") %>%
    mutate(week = lubridate::epiweek(date)) %>%
    group_by(week, county, state) %>%
    summarize(num_cases = max(cases), 
              num_deaths = max(deaths)) %>%
    ungroup()
  
  # getting a list of dates
  sf = stamp("January 1, 2020")
  
  ending_days = read_csv("./data/covid_19_by_county.csv") %>%
    mutate(week = lubridate::epiweek(date)) %>%
    dplyr::select(week, date) %>%
    group_by(week) %>%
    summarize(last_day = max(date)) %>%
    mutate(last_sunday = last_day + days(1)) %>%
    mutate(last_sunday = sf(last_sunday)) %>%
    pull(last_sunday)
  
  # splitting to a list separated by week
  covid_split = split(covid_data, covid_data$week)
  
  # counties data
  counties = map_data("county") %>%
    rename(county = subregion, 
           state = region) %>%
    mutate(county = stringr::str_to_title(county), 
           state = stringr::str_to_title(state))
  
  # doing a right join on every item in the list
  joined_list = lapply(covid_split, right_join, counties, by = c("state", "county"))
  
  # generating week vectors
  weeks = names(covid_split)
  
  #replacing values in each item with the week number
  for(i in 1:length(joined_list)){
    joined_list[[i]]$week = weeks[i]
  }
  
  # replacing missing num_cases and num_deaths with 0s
  for(i in 1:length(joined_list)){
    joined_list[[i]]$num_cases = ifelse(is.na(joined_list[[i]]$num_cases), 
                                        0, 
                                        joined_list[[i]]$num_cases)
    joined_list[[i]]$num_deaths = ifelse(is.na(joined_list[[i]]$num_deaths), 
                                        0, 
                                        joined_list[[i]]$num_deaths)
  }
  
 glimpse(joined_list)
  
  # cartograms for each week
  
  # turning weekly dataframes into an sf polygon objects for cartogram processing
  sf_list = lapply(joined_list, 
                   sf_polygon, 
                   x = "long", 
                   y = "lat", 
                   polygon_id = "group", 
                   keep = TRUE)
  
  # using multi-core parallel lapply to generate cartograms for each week
  carto_list = mclapply(sf_list, 
                      cartogram_cont, 
                      weight = "num_cases", 
                      prepare = "remove", 
                      itermax = 5)
  
  # un listing into a big sf
  unlisted_sf = reduce(carto_list, sf:::rbind.sf)
  
  # changing to a factor
  unlisted_sf$week = factor(as.numeric(unlisted_sf$week))
  
  unlisted_sf$week_label = factor(unlisted_sf$week, labels = ending_days)
  unlisted_sf = unlisted_sf %>%
    dplyr::select(-group) 
  

#full
# testing
unlisted_sf$week_label = droplevels(unlisted_sf$week_label)

# Generating summary df for labels
# Centroid coords
coords = unlisted_sf %>%
  st_centroid() %>%
  st_coordinates() %>%
  as_tibble()

# binding and summarizing
summary_df = unlisted_sf %>%
  bind_cols(coords) %>%
  group_by(week_label) %>%
  filter(num_cases != 0) %>%
  top_n(10, num_cases) %>%
  mutate(labels = paste(county, state, sep = ",")) %>%
  ungroup()

# making a unique county_state_id
unlisted_sf = unlisted_sf %>%
  mutate(unique = paste(county, state, sep = "_"))

ggplot(unlisted_sf) +
  geom_sf() +
  facet_wrap(~ week_label)

g1 = ggplot() +
  geom_sf(data = unlisted_sf, 
          aes(fill = as.factor(state), 
              group = as.factor(unique)), 
          size = 0.1) +
  geom_label_repel(data = summary_df, aes(x = X, y = Y, label = labels, 
                                          group =as.factor(county)), 
                   seed = 42, vjust = 20) +
  transition_states(week_label, transition_length = 3, state_length = 1) +
  theme_void() +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.1, size = 22)) +
  labs(title = 'Week ending in {closest_state}') +
  ease_aes('sine-in-out')


anim = animate(plot = g1, height = 1200, width = 1800, duration = 20, detail = 5)

}
carto_list = readRDS("./output/carto_list.rds")


