library(sf)
library(tidyverse)


## Write Great Meadows polygon filtering function
filter_to_geobounds <- function(dat, lat, long) {
  
  # Prevent default
  sf::sf_use_s2(FALSE)
  
  # Read in polygon of interest, in this case file path to great meadow
  geo.bounds <- sf::read_sf("data/raw_data/biodiversity_data/GreatMeadowBoundaries/GreatMeadowBoundaries.shp") %>%
    st_transform(4326)

  # Format data for filtering
  dat2 <- dat %>% 
    rename(x = paste(long), y = paste(lat)) %>% 
    mutate(longitude.keep = x,
           latitude.keep = y) %>% 
    sf::st_as_sf(., coords = c("x","y"), crs = sf::st_crs(geo.bounds))
  
  # Filter to polygon using st_join and clean
  output <- sf::st_join(dat2, geo.bounds, left = F) %>% 
    st_set_geometry(., NULL) %>% 
    select(everything(), latitude = latitude.keep, longitude = longitude.keep)
  
  return(output)
  
}



## Read in your raw data that is from a larger geographic area than the polygon of interest
inat_raw <- read.csv("data/raw_data/biodiversity_data/great_meadow_citsci_data_20260324/inat_observations-700177.csv")
ebird_raw <- read.delim("data/raw_data/biodiversity_data/great_meadow_citsci_data_20260324/ebd_US-ME-009_smp_relFeb-2026.txt", header = T, quote = "")


## Filter the data to the geographic bounds using the custom function
inat_gm <- filter_to_geobounds(inat_raw, lat = "latitude", long = "longitude")
ebird_gm <- filter_to_geobounds(ebird_raw, lat = "LATITUDE", long = "LONGITUDE")


## Export
write.csv(inat_gm, "data/raw_data/biodiversity_data/inat_greatmeadow_20260326.csv", row.names = F)
write.csv(ebird_gm, "data/raw_data/biodiversity_data/ebird_greatmeadow_20260324.csv", row.names = F)



