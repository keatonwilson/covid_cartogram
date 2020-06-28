# cartogram mapping function
# Keaton Wilson
# keatonwilson@me.com
# 2020-04-08

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
require(cartogram)
require(mcla)
require(parallel)
require(gganimate)
require(transformr)
require(purrr)
require(ggthemes)
require(ggrepel)
require(viridis)
require(raster)
require(gifski)

# pulling nytimes data function
source("./scripts/get_nytimes_covid_data.R")


make_animated_state_carto = function(){
  # pulling most recent data
  get_nytimes_covid_data()
  
  
  # loading covid data and joining to state data
  covid_data = read_csv("./data/covid_19_by_state.csv") %>%
    mutate(week = lubridate::epiweek(date)) %>%
    group_by(week, state) %>%
    summarize(num_cases = max(cases), 
              num_deaths = max(deaths)) %>%
    ungroup() %>%
    group_by(week) %>%
      mutate(perc_by_week = num_cases/sum(num_cases))
  
  # getting a list of dates
  sf = stamp("January 1, 2020")
  
  ending_days = read_csv("./data/covid_19_by_state.csv") %>%
    mutate(week = lubridate::epiweek(date)) %>%
    dplyr::select(week, date) %>%
    group_by(week) %>%
    summarize(last_day = max(date)) %>%
    mutate(last_sunday = last_day + days(1)) %>%
    mutate(last_sunday = sf(last_sunday)) %>%
    pull(last_sunday)
  
  # splitting to a list separated by week
  covid_split = split(covid_data, covid_data$week)
  
  usa <- maps::map("state", fill = TRUE)
  IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
  usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  usa <- SpatialPolygonsDataFrame(usa, 
                                  data = data.frame(unique(IDs), 
                                                    row.names = unique(IDs)) )
  usa@data = usa@data %>% transmute(state = stringr::str_to_title(unique.IDs.))
  
  joined_list = list()
  for(i in 1:length(covid_split)){
    joined_list[[i]] = merge(usa, covid_split[[i]], all.x = TRUE, by = "state")
  }
  
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
    joined_list[[i]]$perc_by_week = ifelse(is.na(joined_list[[i]]$perc_by_week), 
                                           0, 
                                           joined_list[[i]]$perc_by_week)
  }
  
 
  #re-projecting
  sp_list = lapply(joined_list, spTransform, CRS("+init=epsg:3857"))
  
  # small addition to all values to get cartogram to plot reasonably
  for(i in 1:length(sp_list)){
    sp_list[[i]]$num_cases = sp_list[[i]]$num_cases + 0.05
  }

  #using multi-core parallel lapply to generate cartograms for each week
  carto_list = mclapply(sp_list,
                      cartogram_cont,
                        weight = "num_cases", 
                        itermax = 15, 
                        prepare = "adjust")

  # merging
  merged = do.call(bind, carto_list)
  merged@data = merged@data %>%
    mutate(num_cases = num_cases - 0.05)
  
  # changing to a factor
  merged$week = (as.numeric(merged$week))
  
  merged$week_label = factor(merged$week, labels = ending_days)
  
  # to sf
  merged_sf = st_as_sf(merged)
  
  # Generating summary df for labels
  # Centroid coords
  coords = merged_sf %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble()
  
states = unique(merged_sf$state)

centroid_list = list()
for(i in seq_along(states)){
  centroid_list[i] = merged_sf %>% 
    filter(state == states[i]) %>%
    st_centroid() %>%
    st_coordinates() %>%
    as_tibble()
}
  
  # binding and summarizing
  summary_df = merged_sf %>%
    bind_cols(coords) %>%
    group_by(week_label) %>%
    filter(num_cases != 0) %>%
    top_n(5, num_cases) %>%
    ungroup() %>%
    mutate(labels = paste(state, ": ", num_cases))
  
  #testing frames out in in a facet
  # ggplot() +
  #   geom_sf(data = merged_sf, aes(fill = log(num_cases)), color = "lightgrey") +
  #   theme_void() +
  #   scale_fill_viridis(option = "magma") +
  #   facet_wrap(~ week_label)
  
g1 = ggplot() +
    geom_sf(data = merged_sf, aes(fill = log(num_cases), group = state), color = "lightgrey") +
    transition_states(week_label, transition_length = 3, state_length = 1) +
    theme_void() +
    theme(legend.position = "none", 
          plot.subtitle = element_text(hjust = 0.1, size = 28),
          plot.title = element_text(hjust = 0.1, size = 32), 
          plot.caption = element_text(size = 18, hjust = 0.8)) +
    ggrepel::geom_label_repel(data = summary_df,
                              size = 8,
                              seed = 42,
                              aes(x = X, y = Y, 
                                  label = paste(state, ":", trunc(num_cases)))) +
    labs(subtitle = 'Week ending in {closest_state}',
         title = 'Cumulative number of cases of Covid-19 in the US by week', 
         caption = 'Data source: New York Times') +
    scale_fill_viridis(option = "magma") +
    ease_aes('sine-in-out')
    
  # what is colorado's deal?
  # ggplot(data = merged_sf %>% filter(state %in% c("Colorado", "California", "New Mexico")), aes(x = week, y = perc_by_week, color = state)) +
  #   geom_line()
  
  
  anim = gganimate::animate(plot = g1, height = 1200, width = 1800, duration = 20, detail = 5, renderer = gifski_renderer())
  save_animation(anim, file = "./output/state_cartogram.gif")
}