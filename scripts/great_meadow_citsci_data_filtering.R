library(sf)
library(tidyverse)


## Write Great Meadows polygon filtering function
filter_to_geobounds <- function(dat, lat, long) {
  
  # Prevent default
  sf::sf_use_s2(FALSE)
  
  # Read in polygon of interest, in this case file path to great meadow
  geo.bounds <- sf::read_sf("/Users/kylelima/Desktop/GreatMeadowBoundaries/GreatMeadowBoundaries.shp") %>%
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
inat_raw <- read.csv("/Users/kylelima/Desktop/inaturalist_acad_obs_20250822.csv")
ebird_raw <- read.delim("/Users/kylelima/Desktop/ebd_US-ME_relJul-2025.txt", header = T, quote = "")


## Filter the data to the geogrpahic bounds using the custom function
inat_gm <- filter_to_geobounds(inat_raw, lat = "latitude", long = "longitude")
ebird_gm <- filter_to_geobounds(ebird_raw, lat = "LATITUDE", long = "LONGITUDE")


## Export
write.csv(inat_gm, "/Users/kylelima/Desktop/inat_greatmeadow_20250825.csv", row.names = F)
write.csv(ebird_gm, "/Users/kylelima/Desktop/ebird_greatmeadow_20250825.csv", row.names = F)



