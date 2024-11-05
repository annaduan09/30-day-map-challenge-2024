# 30 Day Map Challenge Day 5: a journey
# November 5, 2024
# Author: Anna Duan, annaduan@sas.upenn.edu

#### Setup ####
library(tidyverse)
library(mapview)
library(conflicted)
library(sf)
library(ggimage)
library(extrafont)

font_import()
loadfonts(device = "pdf")

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")

#### Load Data ####
# Admin 0 - Countries https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
world <- st_read("shapefiles/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
  st_transform("EPSG:3857")

# Admin 1 - States, Provinces https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
china <- st_read("shapefiles/ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp") %>%
  filter(iso_a2 == "CN") %>%
  st_transform("EPSG:3857")

#### Data Prep ####
# Crop basemap to China
china_bbox <- st_bbox(c(xmin = 7693366, xmax = 15502800, ymin = 1278728, ymax = 7589026), crs = st_crs(world))
basemap <- world %>%
  st_make_valid() %>%
  st_crop(china_bbox)

# Crop background (ocean) to China
background <- data.frame(xmin = 7693366, xmax = 15502800, ymin = 1278728, ymax = 7589026)

# Province labels
province_labels <- china %>% 
  st_centroid() %>%
  filter(name %in% c("Guangxi", "Fujian", "Hunan", "Liaoning")) %>%
  select(name) %>%
  mutate(name = toupper(name))

# Province centroid coordinates
hunan_x <- 12433978
hunan_y <- 3203293
fujian_x <- 13132729
fujian_y <- 3010387
liaoning_x <- 13649613
liaoning_y <- 5059462
guangxi_x <- 12109209
guangxi_y <- 2734639

# Lifter info
lifters <- data.frame(lifter = c("Li Fabin", 
                                 "Shi Zhiyong", 
                                 "Liu Huanhua", 
                                 "Hou Zhihui", 
                                 "Luo Shifang", 
                                 "Li Wenwen"),
                      class = c("Men's 61kg",
                                "Men's 73kg",
                                "Men's 102kg",
                                "Women's 49kg",
                                "Women's 59kg",
                                "Women's +81kg"),
                      hometown = c("Quanzhou, Fujian",
                                   "Guilin, Guangxi",
                                   "Chenzhou, Hunan",
                                   "Chenzhou, Hunan",
                                   "Chenzhou, Hunan",
                                   "Anshan, Liaoning"),
                      lat = c(fujian_y - 350000, #fujian
                              guangxi_y - 350000, #guangxi
                              hunan_y + 700000, #hunan
                              hunan_y + 700000, #hunan
                              hunan_y + 700000, #hunan
                              liaoning_y + 700000), #liaoning
                      lon = c(fujian_x, #fujian
                              guangxi_x, #guangxi
                              hunan_x, #hunan
                              hunan_x - 800000, #hunan
                              hunan_x + 800000, #hunan
                              liaoning_x)) %>% #liaoning
  st_as_sf(coords = c("lon", "lat"), crs = "EPSG:3857")

#### Map ####
# panel references
left = 7693366
right = 15502800
bottom = 1278728
top = 7589026

map <- ggplot() +
  geom_rect(data = background, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "#2e5d71") +
  geom_sf(data = basemap, color = "gray22", fill = "gray15") +
  geom_sf(data = china, color = "gray15", fill = "gray22") +
  geom_sf_text(data = province_labels, aes(label = name), size = 4, color = "gray70", family = "Futura") +
  geom_sf_text(data = lifters, aes(label = lifter), size = 3.5, color = "goldenrod1", nudge_y = -300000, family = "Arial", fontface = "bold") +
  geom_sf_text(data = lifters, aes(label = hometown), size = 3, color = "goldenrod4", nudge_y = -400000, family = "Arial") +
  geom_sf_text(data = lifters, aes(label = class), size = 3, color = "goldenrod4", nudge_y = -500000, family = "Arial") +
  geom_text(aes(label = str_wrap("The Chinese Weightlifting Team: Hometown Edition", 38), x = left + 300000, y = 7000000), size = 10, color = "salmon", family = "Andale Mono", hjust = 0) +
  geom_text(aes(label = "China's weightlifting team won 5 gold medals at the Paris Olympics.\n3 of the medalists are from the same city in Hunan Province!", x = left + 310000, y = top - 1200000), size = 4, color = "gray60", family = "Arial", hjust = 0) +
  geom_text(aes(label = "Anna Duan | linkedin.com/in/annaduan", x = left + 150000, y = bottom + 420000), size = 3, color = "gray60", family = "Arial", hjust = 0) +
  geom_text(aes(label = "R Libraries: tidyverse, sf, ggimage, extrafont", x = left + 150000, y = bottom + 310000), size = 3, color = "gray60", family = "Arial", hjust = 0) +
  geom_text(aes(label = "Source: International Olympic Committee, Wikipedia, Natural Earth", x = left + 150000, y = bottom + 200000), size = 3, color = "gray60", family = "Arial", hjust = 0) +
  geom_image(aes(image = "img/li_f.png", x = fujian_x, y = fujian_y - 350000), size = 0.06) +
  geom_image(aes(image = "img/shi.png", x = guangxi_x, y = guangxi_y - 350000), size = 0.06) + 
  geom_image(aes(image = "img/liu.png", x = hunan_x, y = hunan_y + 700000), size = 0.06) + 
  geom_image(aes(image = "img/hou.png", x = hunan_x - 800000, y = hunan_y + 700000), size = 0.06) + 
  geom_image(aes(image = "img/luo.png", x = hunan_x + 800000, y = hunan_y + 700000), size = 0.06) + 
  geom_image(aes(image = "img/li_w.png", x = liaoning_x, y = liaoning_y + 700000), size = 0.06) + 
  geom_image(aes(image = "img/olympics.png", x = left + 1000000, y = bottom + 700000), size = 0.06) +
  geom_image(aes(image = "img/china.png", x = left + 350000, y = bottom + 700000), size = 0.06) +
  theme_void() 

# write png of map 
ggsave("map.png", plot = map, width = 12, height = 10, dpi = "retina")
