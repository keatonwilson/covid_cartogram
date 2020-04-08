# static faceted map by week
#libraries
library(tidyverse)
library(sf)
library(usmap)
library(ggmap)
library(Rcartogram)
library(getcartr)
library(sfheaders)
library(maptools)

# loading covid data and joining to county data
covid_data = read_csv("./data/covid_19_by_county.csv") %>%
  mutate(week = lubridate::week(date)) %>%
  group_by(week, county) %>%
  summarize(num_cases = max(cases), 
            num_deaths = max(deaths))

counties = map_data("county") %>%
  rename(county = subregion, 
         state = region) %>%
  mutate(county = stringr::str_to_title(county), 
         state = stringr::str_to_title(state))

joined = left_join(counties, covid_data, by = "county")
joined %>%
  filter(is.na(num_cases))

# plotting
ggplot(joined, aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = num_cases)) +
  theme_nothing(legend = TRUE) +
  facet_wrap(~ week)

#Big current map
ggplot(joined %>%
         filter(week == 13), aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(aes(fill = num_cases)) +
  theme_nothing(legend = TRUE)

# size mapping - wacky cartogram
# https://trucvietle.me/r/tutorial/2016/12/18/cartogram-plotting-using-r.html
# http://clarkdatalabs.github.io/mapping_R/
# https://www.r-graph-gallery.com/331-basic-cartogram.html

library(FRK)
library(spdplyr)
library(cartogram)
# making a small df to play with
joined_sm = joined %>%
  filter(week == 12)

sf = sf_polygon(joined_sm, 
           x = "long", 
           y = "lat", 
           polygon_id = "group", 
           keep = TRUE)

cart_test = cartogram_cont(sf, weight = "num_cases")
plot(st_geometry(cart_test["num_deaths"]))
cart_test                           
test = cart_test
st_geometry(test) = NULL
test

ggplot() +
  geom_sf(data = cart_test, aes(fill = state))
