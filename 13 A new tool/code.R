#### SETUP ####
# libraries
library(tidyverse)
library(tigris)
library(tidycensus)
library(sf)
library(mapview)
library(viridis)

# census api call
census_api_key("d9ebfd04caa0138647fbacd94c657cdecbf705e9", install = TRUE, overwrite = TRUE)
vars <- load_variables(2017, "acs5", cache = TRUE)

# total households surveyed B28002_001
# any kind of internet subscription B28002_002
# 
#### 2023 ####
states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

get_dat <- function(year){
  dat <- get_acs(geography = "county", state = states,
                  variables = c("hh_total" = "B28002_001E",
                                "hh_internet" = "B28002_002E"), 
                  year = year,
                  geometry = TRUE, survey = "acs5", output = "wide") %>%
  mutate(internet_pct = ifelse(hh_total > 0, round(100*hh_internet/hh_total), 0)) %>%
  dplyr::select(internet_pct, hh_internet) %>%
  shift_geometry()
  
  return(dat)
}
 
# get data for 2013-2023
for (i in c(2017, 2022)){
    dat <- get_dat(i)
    assign(paste0("internet_", i), dat)
}


# centroid
centroid_22 <-  internet_2022 %>% 
  st_centroid() %>%
  mutate(internet_pct = round(internet_pct*100)) %>%
  arrange(internet_pct)


#### MAP ####
ggplot() +
  geom_sf(data = internet_2022 %>% st_centroid(), aes(size = internet_pct), color = "azure1") +
  # scale_color_distiller(palette = "Blues",
  #                      direction = -1,
  #                    guide = "legend") +
  scale_size(range = c(0.1, 1),
             guide = "legend") +
  theme_void() + theme(legend.position = c(0.9, 0.2),
                       panel.background = element_rect(fill = "black"),
                       legend.text = element_text(color = "beige"))



