## Script for merging new veg data with old ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(stringr)
library(sf)
library(gt)
library(purrr)
library(ggplot2)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

new_spplist <- read.csv("data/processed_data/ACAD_Wetland_Species_List_20251215.csv") %>%
  as_tibble()

old_spplist <- read.csv("data/processed_data/FOA_NETN_species_list_2011_2024.csv")

new_vmmi <- read.csv("data/processed_data/ACAD_Wetland_VegMMI_20251215.csv") %>%
  as_tibble()

old_vmmi <- read.csv("data/processed_data/FOA_NETN_VMMI_2011_2024.csv")


species_list <- read.csv("data/processed_data/species_list_arcgis.csv") %>%
  as_tibble()


#---------------------------#
####    Load Functions   #### 
#---------------------------#

# Convert UTM to lat/lon
utm_to_latlon <- function(df, x_col = "xCoordinate", y_col = "yCoordinate", epsg = 32619) {
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

# QA function to check spatial coordinates
qa_check_coords <- function(df, sites_df) {
  df %>%
    left_join(sites_df, by = "site.name", suffix = c(".data", ".expected")) %>%
    mutate(
      lat_diff = abs(latitude.data - latitude.expected),
      lon_diff = abs(longitude.data - longitude.expected),
      coord_match = lat_diff < 0.00001 & lon_diff < 0.00001
    )
}


#-----------------------#
####    Data Manip   #### 
#-----------------------#

## species list data -----------------------------------------------------------

# FOA new spplist data 
# format new data to merge with existing data
new_spplist_clean <- new_spplist %>% 
  
  # filter for only new year of data
  filter(Year == 2025) %>% 
  
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
    
    # add wetland column
    wetland = 
      case_when(
        grepl("GRME", Code) ~ "Great Meadow",
        grepl("GIME", Code) ~ "Gilmore Meadow",
        TRUE ~ NA_character_),
    
    # add a source column
    source = "FOA",
    
    # correct column class
    Location_ID = as.character(Location_ID)
    
  ) %>% 
  
  # convert coordinates to lat and long  
  utm_to_latlon() %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = Location_ID,
         site.type,
         year = Year,
         latitude,
         longitude,
         tsn = TSN,
         plant.code = PLANTS_Code,
         latin.name = Latin_Name,
         common.name = Common,
         quad.freq = quad_freq,
         invasive = Invasive,
         protected = Protected_species,
         coc = CoC_ME_ACAD,
         coc.wetness = Coef_wetness,
         wetland,
         source)


# merge new and existing spplist datasets
spplist_new_old <- bind_rows(new_spplist_clean, old_spplist) %>% 
select(site.name, local.id, site.type, year, latitude, longitude, everything())

# write.csv(spplist_corrected, "data/processed_data/FOA_NETN_species_list_2011_2025.csv", row.names = FALSE)

## species list table for pop-up on arcgis map

spplist_arcgis <- spplist_new_old %>%
  group_by(site.name, latin.name, common.name, invasive) %>%
  summarize(
    years.observed = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    invasive = ifelse(invasive, "Yes", "No")
  )

# export for joining to spatial data
# write.csv(spplist_arcgis, "data/processed_data/species_list_arcgis_2011_2025.csv", row.names = FALSE)


## VMMI data -------------------------------------------------------------------

## FOA VMMI data 
new_vmmi_clean <- new_vmmi %>% 
  
  # filter for only new year of data
  filter(Year == 2025) %>% 
  
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
    
    # add wetland column
    wetland = 
      case_when(
        grepl("GRME", Code) ~ "Great Meadow",
        grepl("GIME", Code) ~ "Gilmore Meadow",
        TRUE ~ NA_character_),
    
    # add a source column
    source = "FOA",
    
    # correct column class
    Location_ID = as.character(Location_ID)
    
  ) %>% 
  
  # convert coordinates to lat and long  
  utm_to_latlon() %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = Location_ID,
         site.type,
         year = Year,
         latitude,
         longitude,
         mean.coc = meanC,
         inv.cov = Invasive_Cover,
         bryo.cov = Bryophyte_Cover,
         strtol.cov = Cover_Tolerant,
         vmmi,
         vmmi.rating = vmmi_rating,
         wetland,
         source)

# merge new and existing VMMI datasets
vmmi_new_old <- bind_rows(new_vmmi_clean, old_vmmi) %>% 
  select(site.name, local.id, site.type, year, latitude, longitude, everything())

# Save outputs as CSV
# write.csv(vmmi_corrected, "data/processed_data/FOA_NETN_VMMI_2011_2025.csv", row.names = FALSE)




