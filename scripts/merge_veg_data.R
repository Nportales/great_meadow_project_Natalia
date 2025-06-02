## Merging Great Meadow Veg Data ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(stringr)
library(sf)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## 2015-2024 Glen Veg data ##

species_by_strata <- read.csv("data/raw_data/Glen_veg_data/species_by_strata_2015_2024.csv") %>%
  as_tibble()

species_list <- read.csv("data/raw_data/Glen_veg_data/species_list_2015_2024.csv") %>%
  as_tibble()

locations <- read.csv("data/raw_data/Glen_veg_data/locations_2015_2024.csv") %>%
  as_tibble()

visits <- read.csv("data/raw_data/Glen_veg_data/visits_2015_2024.csv") %>%
  as_tibble()

RAM_stressors <- read.csv("data/raw_data/Glen_veg_data/RAM_stressors_2015_2024.csv") %>%
  as_tibble()

vertical_complexity <- read.csv("data/raw_data/Glen_veg_data/vertical_complexity_2015_2024.csv") %>%
  as_tibble()

VMMI_2015_2024 <- read.csv("data/processed_data/VMMI_2015_2024.csv") %>%
  as_tibble()

## Kate Veg data ##

ram_ssplist <- read.csv("data/processed_data/Kate_NETN_veg_data/ram_spplist_2012_to_2022.csv") %>%
  as_tibble()

ram_veg_cover <- read.csv("data/processed_data/Kate_NETN_veg_data/ram_veg_cover_2012_to_2022.csv") %>%
  as_tibble()

sen_ram_species <- read.csv("data/processed_data/Kate_NETN_veg_data/sen_ram_species_data_2011_2022.csv") %>%
  as_tibble()

sen_veg_cover <- read.csv("data/processed_data/Kate_NETN_veg_data/sen_veg_cover_2011_2016_2021.csv") %>%
  as_tibble()

VMMI_ram_sen <- read.csv("data/processed_data/Kate_NETN_veg_data/vegMMI_2011_to_2023.csv") %>%
  as_tibble()


#-----------------------#
####    Data Manip   #### 
#-----------------------#

## load functions

# Convert UTM to lat/lon
utm_to_latlon <- function(df, x_col = "xcoord", y_col = "ycoord", epsg = 32619) {
  sf_obj <- st_as_sf(df, coords = c(x_col, y_col), crs = epsg)
  latlon <- st_transform(sf_obj, crs = 4326)
  coords <- st_coordinates(latlon)
  
  # Remove original UTM columns
  df[[x_col]] <- NULL
  df[[y_col]] <- NULL
  
  # Add renamed latitude and longitude columns
  df$longitude <- coords[, "X"]
  df$latitude  <- coords[, "Y"]
  
  df
}

## merge Glen VMMI data with NETN VMMI data ------------------------------------

# first edit Glen VMMI data columns to match NETN VMMI data columns
new_VMMI_2015_2024 <- VMMI_2015_2024 %>% 
  
  mutate(
    
    # add a site type column
    site.type = 
      case_when(
        grepl("GRME01", Code) ~ "Intensive",
        grepl("GRME02", Code) ~ "Intensive",
        grepl("GRME03", Code) ~ "Intensive",
        grepl("GRME04", Code) ~ "Intensive",
        grepl("GRME05", Code) ~ "Intensive",
        grepl("GRME06", Code) ~ "Intensive",
        grepl("GRME07", Code) ~ "RAM",
        grepl("GRME08", Code) ~ "RAM",
        grepl("GRME09", Code) ~ "RAM",
        grepl("GRME10", Code) ~ "RAM",
        grepl("GIME", Code) ~ "RAM",
        TRUE ~ NA_character_),
    
    wetland = 
      case_when(
        grepl("GRME", Code) ~ "Great Meadow",
        grepl("GIME", Code) ~ "Gilmore Meadow",
        TRUE ~ NA_character_),
    
    Location_ID = as.character(Location_ID)
    
    ) %>% 

    # Remove unneeded columns and rename
    select(site.name = Code,
           local.id = Location_ID,
           site.type,
           year = Year,
           xcoord = xCoordinate,
           ycoord = yCoordinate,
           mean.coc = meanC,
           inv.cov = Invasive_Cover,
           bryo.cov = Bryophyte_Cover,
           strtol.cov = Cover_Tolerant,
           vmmi,
           vmmi.rating = vmmi_rating,
           wetland)

# then select appropriate sites from NETN VMMI dataset
new_VMMI_ram_sen <- VMMI_ram_sen %>% 
  mutate(
    
    # fix site.name codes
    site.name =
      case_when(
        site.name %in% c("NWCA11-R304", "NWCA16-R304", "NWC21-ME-HP304") ~ "NWCA-R304",
        TRUE ~ site.name),
    
    # standardize wetland names in notes column
    notes = 
      case_when(
        site.name %in% c("RAM-13", "RAM-04", "RAM-19") ~ "Great Meadow",
        site.name %in% c("RAM-31", "NWCA-R304") ~ "Gilmore Meadow",
        TRUE ~ NA_character_)) %>% 
  
  filter(site.name %in% c("RAM-31", "RAM-13", "RAM-04", "RAM-19", "NWCA-R304")) %>% 
  rename(wetland = notes)

# then merge datasets
VMMI_Glen_NETN <- bind_rows(new_VMMI_2015_2024, new_VMMI_ram_sen) %>% 
  utm_to_latlon() %>% 
  select(site.name, local.id, site.type, year, latitude, longitude, everything())

# Save outputs as CSV
# write.csv(VMMI_Glen_NETN, "data/processed_data/VMMI_Glen_NETN_2011_2024.csv", row.names = FALSE)


#### GRAVEYARD ####-------------------------------------------------------------

# ## merge 2025-2023 and 2024 Glen veg data ------------------------------------
# 
# # Remove one column
# VMMI_2024 <- VMMI_2024 %>% select(-X)
# 
# # Add site column to 2015-2023 data
# VMMI_2015_2023 <- VMMI_2015_2023 %>% 
#   mutate(Site = "Great Meadow")
# 
# # merge 2015-2023 data and 2024 data
# VMMI_2015_2024 <- bind_rows(VMMI_2015_2023, VMMI_2024)



