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

VMMI_FOA <- read.csv("data/processed_data/FOA_VMMI_2015_2024.csv") %>%
  as_tibble()

spplist_FOA <- read.csv("data/processed_data/FOA_species_list_2015_2024.csv") %>%
  as_tibble()

## Kate Veg data ##

VMMI_NETN <- read.csv("data/processed_data/NETN_vegMMI_allsites_2011-2024.csv") %>%
  as_tibble()

spplist_NETN <- read.csv("data/processed_data/NETN_spplist_allsites_2011-2024_public.csv") %>% 
  as_tibble()

#---------------------------#
####    Load Functions   #### 
#---------------------------#

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

#-----------------------#
####    Data Manip   #### 
#-----------------------#

## merge FOA VMMI data with NETN VMMI data ------------------------------------

## FOA VMMI data 
new_VMMI_FOA <- VMMI_FOA %>% 
  
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

    # Remove unneeded columns and rename to standardize 
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
           wetland,
           source)


# NETN VMMI data
new_VMMI_NETN <- VMMI_NETN %>% 
  
  # first filter for great meadow and gilmore meadow sites
  filter(Code %in% c("RAM-31", "RAM-13", "RAM-04", "RAM-19") | str_detect(Code, "304")) %>% 
  
  mutate(
    
    # fix code names
    Code = 
      case_when(
      str_detect(Code, "304") ~ "NWCA-R304",
      TRUE ~ Code),
    
    # standardize wetland names in notes column
    Note = 
      case_when(
        Code %in% c("RAM-13", "RAM-04", "RAM-19") ~ "Great Meadow",
        Code %in% c("RAM-31", "NWCA-R304") ~ "Gilmore Meadow",
        TRUE ~ NA_character_),
    
    # add a source column
    source = "NETN"
    
  ) %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = local_id,
         site.type = site_type,
         year = Year,
         xcoord = xCoordinate,
         ycoord = yCoordinate,
         mean.coc = meanC,
         inv.cov = Invasive_Cover,
         bryo.cov = Bryophyte_Cover,
         strtol.cov = Cover_Tolerant,
         vmmi,
         vmmi.rating = vmmi_rating,
         wetland = Note,
         source)

# merge FOA and NETN VMMI datasets
VMMI_FOA_NETN <- bind_rows(new_VMMI_FOA, new_VMMI_NETN) %>% 
  utm_to_latlon() %>% 
  select(site.name, local.id, site.type, year, latitude, longitude, everything())

# Save outputs as CSV
# write.csv(VMMI_FOA_NETN, "data/processed_data/FOA_NETN_VMMI_2011_2024.csv", row.names = FALSE)


## merge FOA spplist data with NETN spplist data ------------------------------------

# FOA spplist data 
new_spplist_FOA <- spplist_FOA %>% 
  
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
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = Location_ID,
         site.type,
         year = Year,
         latitude = Latitude,
         longitude = Longitude,
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


# NETN spplist data
new_spplist_NETN <- spplist_NETN %>% 
  
  # first filter for great meadow and gilmore meadow sites
  filter(Code %in% c("RAM-31", "RAM-13", "RAM-04", "RAM-19") | str_detect(Code, "304")) %>% 
  
  mutate(
    
    # fix code names
    Code = 
      case_when(
        str_detect(Code, "304") ~ "NWCA-R304",
        TRUE ~ Code),
    
    # standardize wetland names in notes column
    wetland = 
      case_when(
        Code %in% c("RAM-13", "RAM-04", "RAM-19") ~ "Great Meadow",
        Code %in% c("RAM-31", "NWCA-R304") ~ "Gilmore Meadow",
        TRUE ~ NA_character_),
    
    # add a source column
    source = "NETN"
    
  ) %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = local_id,
         site.type = site_type,
         year = Year,
         xcoord = xCoordinate,
         ycoord = yCoordinate,
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
         source) %>% 
 
  # convert coordinates to lat and long  
  utm_to_latlon()
    
# merge FOA and NETN VMMI datasets
spplist_FOA_NETN <- bind_rows(new_spplist_FOA, new_spplist_NETN) %>% 
  select(site.name, local.id, site.type, year, latitude, longitude, everything())

# filter out NAs and error values

clean_spplist_FOA_NETN <- spplist_FOA_NETN[
  !is.na(spplist_FOA_NETN$tsn) & !grepl("^\\-999", as.character(spplist_FOA_NETN$tsn)),
]

# Save outputs as CSV
# write.csv(clean_spplist_FOA_NETN, "data/processed_data/FOA_NETN_species_list_2011_2024.csv", row.names = FALSE)



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



# # then select appropriate sites from NETN VMMI dataset
# new_VMMI_ram_sen <- VMMI_ram_sen %>% 
#   mutate(
#     
#     # fix site.name codes
#     site.name =
#       case_when(
#         site.name %in% c("NWCA11-R304", "NWCA16-R304", "NWC21-ME-HP304") ~ "NWCA-R304",
#         TRUE ~ site.name),
#     
#     # standardize wetland names in notes column
#     notes = 
#       case_when(
#         site.name %in% c("RAM-13", "RAM-04", "RAM-19") ~ "Great Meadow",
#         site.name %in% c("RAM-31", "NWCA-R304") ~ "Gilmore Meadow",
#         TRUE ~ NA_character_),
#     
#     # add a source column
#     source = "NETN"
#     
#   ) %>% 
#   
#   filter(site.name %in% c("RAM-31", "RAM-13", "RAM-04", "RAM-19", "NWCA-R304")) %>% 
#   rename(wetland = notes)
# 
# # then merge datasets
# VMMI_FOA_NETN <- bind_rows(new_VMMI_2015_2024, new_VMMI_ram_sen) %>% 
#   utm_to_latlon() %>% 
#   select(site.name, local.id, site.type, year, latitude, longitude, everything())
# 



