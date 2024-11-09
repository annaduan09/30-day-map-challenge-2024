# 30 Day Map Challenge Day 9: AI only
# November 9, 2024
# Author: Anna Duan, annaduan@sas.upenn.edu

#### Setup ####
library(tidyverse)
library(mapview)
library(conflicted)
library(sf)
library(tigris)
library(osmdata)
library(ggimage)
library(extrafont)

font_import()
loadfonts(device = "pdf")

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

#### OSM Data ####
plant_bb <- getbb("Three Mile Island Nuclear Generating Station, PA")

plant_osm <- plant_bb %>%
  opq() %>%
  add_osm_feature(key = "landuse", value = "industrial") %>%
  osmdata_sf()

buildings_osm <- plant_bb %>%
  opq() %>%
  add_osm_feature(key = "building", value = "yes") %>%
  osmdata_sf()

plant <- plant_osm$osm_polygons %>%
  st_transform("EPSG:2272")

buildings <- buildings_osm$osm_polygons %>%
  st_transform("EPSG:2272")

#### Boundaries ####
island <- data.frame(lat = 40.147, lon = -76.723) %>%
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>%
    st_transform("EPSG:2272")

island_x <- as.numeric(st_coordinates(island)[, 1])
island_y <- as.numeric(st_coordinates(island)[, 2])

feet_x <- 16000
feet_y <- 24000

island_bbox <- st_bbox(c(
  xmin = island_x - feet_x / 2,
  xmax = island_x + feet_x / 2,
  ymin = island_y - feet_y / 2,
  ymax = island_y + feet_y / 2
), crs = st_crs(island))

#### Basemap ####
basemap <- counties("42") %>%
  filter(NAME %in% c("Dauphin", "York", "Lancaster")) %>%
  st_transform("EPSG:2272") %>%
  st_crop(island_bbox) %>%
  erase_water(area_threshold = 0.1) 

roads <- tigris::roads("PA", county = c("Dauphin", "York", "Lancaster")) %>%
  st_transform("EPSG:2272") %>%
  st_crop(island_bbox) %>%
  erase_water(area_threshold = 0.1)

pa <- counties("42") %>%
  st_transform("EPSG:2272") 

#### Data Prep ####
# panel references
left = as.numeric(island_bbox[1])
right = as.numeric(island_bbox[3])
bottom = as.numeric(island_bbox[2])
top = as.numeric(island_bbox[4])

x_range = right - left
y_range = top - bottom

# Crop background (river)
background <- data.frame(xmin = left, 
                         xmax = right, 
                         ymin = bottom, 
                         ymax = top)

#### Map ####
# inset
inset <- ggplot() +
  geom_sf(data = st_union(pa), fill = "gray20", color = "transparent") +
  geom_sf_text(data = st_union(pa) %>% st_centroid(), aes(label = "PA"), size = 8, color = "#9fbfba", family = "Arial") +
  geom_sf(data = plant %>% st_buffer(30000), fill = "gold", color = "transparent") +
  theme_void()

# write png of inset 
ggsave("img/inset.png", plot = inset, width = 3, height = 2, dpi = "retina")

map <- 
  ggplot() +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#5dc3b9") +
  geom_sf(data = basemap, color = "transparent", fill = "#242121") +
  geom_sf(data = roads, color = "gray20") +
  geom_sf(data = buildings, color = "transparent", fill = "gold") +
  geom_text(aes(label = str_wrap("Three Mile Island,", 38), x = right - x_range*0.05, y = top - y_range*0.05), size = 11, color = "#ee962f", family = "Big Caslon", hjust = 1) +
  geom_text(aes(label = "site of the worst nuclear meltdown in US history,\n
                will reopen in 2028 for 20-year deal with Microsoft.\n
                Under this agreement, Microsoft will buy all power\n
                generated to fuel its growing data centers.", x = right - x_range*0.05, y = top - y_range*0.11), size = 3.3, color = "#9fbfba", family = "Arial", hjust = 1, lineheight = 0.65) +
  geom_text(aes(label = "Opened in 1974, the two-unit plant closed\n
Unit 2 after its 1979 meltdown. The\n
plant closed officially for economic\n
reasons in 2019.", x = right - x_range*0.4, y = bottom + y_range*0.51), size = 3.3, color = "#9fbfba", family = "Arial", hjust = 0, lineheight = 0.65) +
  geom_text(aes(label = "Constellation Energy, the plant's owner,\n
will revive Unit 1 and two cooling towers\n
by 2028, supplying Microsoft enough\n
power for 800k houses per year.", x = right - x_range*0.05, y = bottom + y_range*0.19), size = 3.3, color = "#9fbfba", family = "Arial", hjust = 1, lineheight = 0.65) +
  geom_text(aes(label = "Anna Duan", x = left + x_range*0.05, y = bottom + y_range*0.08), size = 4, color = "#ee962f", family = "Arial", hjust = 0, alpha = 0.8) +
  geom_text(aes(label = "R Libraries: tigris, osmdata, tidyverse, sf, ggimage, extrafont", x = left + x_range*0.05, y = bottom + y_range*0.065), size = 3, color = "#9fbfba", family = "Arial", hjust = 0, alpha = 0.8) +
  geom_text(aes(label = "Sources: openstreetmap.org, Reuters, Constellation Energy", x = left + x_range*0.05, y = bottom + y_range*0.05), size = 3, color = "#9fbfba", family = "Arial", hjust = 0, alpha = 0.8) +
  
  geom_text(aes(label = "Susquehanna River", x = left + x_range*0.5, y = bottom + y_range*0.17), size = 5, color = "darkcyan", family = "Big Caslon", hjust = 1, angle = -35) +
  geom_text(aes(label = "Three\nMile\nIsland", x = left + x_range*0.475, y = bottom + y_range*0.51), size = 3, color = "gold", family = "Big Caslon", hjust = 0.5) +
  geom_text(aes(label = "HILL\nISLAND", x = left + x_range*0.17, y = bottom + y_range*0.85), size = 3, color = "#9fbfba", family = "Arial Narrow",  hjust = 0.5, alpha = 0.3) +
  geom_text(aes(label = "SHELLEY\nISLAND", x = left + x_range*0.29, y = bottom + y_range*0.6), size = 3, color = "#9fbfba", family = "Arial Narrow", hjust = 0.5, alpha = 0.3) +
  geom_text(aes(label = "YORK HAVEN, PA", x = left + x_range*0.25, y = bottom + y_range*0.12), size = 3, color = "#9fbfba", family = "Arial Narrow", hjust = 0.5, alpha = 0.3) +
  geom_text(aes(label = "MIDDLETOWN, PA", x = left + x_range*0.7, y = bottom + y_range*0.7), size = 3, color = "#9fbfba", family = "Arial Narrow", hjust = 0.5, alpha = 0.3) +
  geom_image(aes(x = right - x_range*0.18, y = bottom + y_range*0.08, image = "img/inset.png"), size = 0.15) +
  theme_void()

# write png of map 
ggsave("map.png", plot = map, width = 8, height = 12, dpi = "retina")
