## Great Meadow data processing ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)

#-----------------------#
####    Read Data    ####
#-----------------------#

#Reading in CSVs as a tibble

## 2015-2023 Glen Veg data ##

species_by_strata_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_by_strata_2015_2023.csv") %>%
  as_tibble()

species_list_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_list_2015_2023.csv") %>%
  as_tibble()

tlu_Plant <- read.csv("data/raw_data/Glen_veg_data/tlu_Plant.csv") %>%
  as_tibble()

locations_2015_2023 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/locations_2015_2023.csv") %>%
  as_tibble()

visits_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/visits_2015_2023.csv") %>%
  as_tibble()

vertical_complexity_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/vertical_complexity_2015_2023.csv") %>%
  as_tibble()

RAM_stressors_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/RAM_stressors_2015_2023.csv") %>%
  as_tibble()

AA_char_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/AA_char_2015_2023.csv")


## 2024 Glen Veg data ##

species_by_strata_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_by_strata_2024.csv") %>%
  as_tibble()

species_list_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_list_2024.csv") %>%
  as_tibble()

locations_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/locations_2024.csv") %>%
  as_tibble()

visits_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/visits_2024.csv") %>%
  as_tibble()

vertical_complexity_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/vertical_complexity_2024.csv") %>%
  as_tibble()

RAM_stressors_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/RAM_stressors_2024.csv") %>%
  as_tibble()

AA_char_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/AA_char_2024.csv")

#-----------------------#
####    Data Manip   ####
#-----------------------#

## 2013-2015 Glen Veg data ## --------------------------------------------------

## combine tlu_Plant to species.list by common columns

#first find latin names that are in species_list and not in tlu_Plant
latin_mismatch_list <- anti_join(species_list_2015, tlu_Plant, by = "Latin_Name")

#search tlu for mismatch latin names
search_tlu <- filter(tlu_Plant, Accepted_Latin_Name == "Rhamnus frangula")

#rename latin name species using case_when
species_list_new <- species_list_2015 %>% 
  
  mutate(
    Latin_Name =
      case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
                Latin_Name == "Lysimachia borealis" ~ "Trientalis borealis",
                Latin_Name == "Glyceria laxa" ~ "Glyceria canadensis",
                Latin_Name == "Viola pallens" ~ "Viola macloskeyi",
                Latin_Name == "Rhodora" ~ "Rhododendron canadense",
                Latin_Name == "Malus sieboldii" ~ "Malus toringo",
                Latin_Name == "Rubus idaeus spp strigosus" ~ "Rubus idaeus",
                Latin_Name == "Juncas pylaei" ~ "Juncus effusus",
                Latin_Name == "Dulichium" ~ "Dulichium arundinaceum",
                Latin_Name == "Unknown Agrostis" ~ "Agrostis",
                Latin_Name == "Brachyelytrum" ~ "Brachyelytrum aristosum",
                Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
                Latin_Name == "Potamogeton berchtoldii" ~ "Potamogeton pusillus",
                Latin_Name == "Brachyelytum aristosum" ~ "Brachyelytrum aristosum",
                Latin_Name == "Juncus pylaei" ~ "Juncus effusus",
                Latin_Name == "Utricularia vulgaris" ~ "Utricularua vulgaris",
                TRUE ~ Latin_Name),
    
    # fill in panel column with -1
    Panel = as.numeric(Panel),
    Panel = replace_na(Panel, -1)
    
    )

#check renaming worked
check_latin_mismatch_list <- anti_join(species_list_new, tlu_Plant, by = "Latin_Name")

#combine datasets
# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols <- intersect(setdiff(names(species_list_new), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))

# Step 3: Join and update values
species_list_tlu <- species_list_new %>%
  left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1
setdiff(names(species_list_new), names(species_list_tlu))

#fix latin name again
species_list_tlu <- species_list_tlu %>% 
mutate(
  Latin_Name =
    case_when(Latin_Name == "Utricularua vulgaris" ~ "Utricularia vulgaris",
              TRUE ~ Latin_Name))          

# Save outputs as CSV
# write.csv(species_list_tlu, "data/processed_data/species_list_tlu_2015_2023.csv", row.names = FALSE)





## combine tlu_Plant to species_by_strata by common columns---------------------

#first find latin names that are in species_by_strata and not in tlu_Plant
strata_latin_mismatch <- anti_join(species_by_strata_2015, tlu_Plant, by = "Latin_Name")

#rename latin name species using case_when
species_by_strata_new <- species_by_strata_2015 %>% 
  
  mutate(
    Latin_Name =
      case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
                Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
                TRUE ~ Latin_Name),
    
    # fill in panel column with -1
    Panel = as.numeric(Panel),
    Panel = replace_na(Panel, -1))

#combine datasets
# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols_2 <- intersect(setdiff(names(species_by_strata_new), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub_2 <- tlu_Plant %>% select(all_of(c(key_col, common_cols_2)))

# Step 3: Join and update values
species_by_strata_tlu <- species_by_strata_new %>%
  left_join(tlu_Plant_sub_2, by = key_col, suffix = c("", "_new")) %>%
  mutate(across(all_of(common_cols_2), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
  select(-ends_with("_new"))

#check that the same columns were kept in dataset 1
setdiff(names(species_by_strata_new), names(species_by_strata_tlu))

# Save outputs as CSV
# write.csv(species_by_strata_tlu, "data/processed_data/species_by_strata_tlu_2015_2023.csv", row.names = FALSE)




#### combine 2024 data with 2015-2023 data ####---------------------------------

#---------------------------#
####    Load Functions   ####
#---------------------------#

# Format a date column consistently
format_dates <- function(df, date_col) {
  df %>%
    mutate({{ date_col }} := parse_date_time({{ date_col }}, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY")),
           {{ date_col }} := as.Date({{ date_col }}),
           {{ date_col }} := format({{ date_col }}, "%Y-%m-%d"))
}

# Bind two datasets after checking identical column names
safe_bind_rows <- function(df1, df2) {
  stopifnot(identical(names(df1), names(df2)))
  bind_rows(df1, df2)
}

# Clean panel column to numeric with NA replaced by -1
fix_panel <- function(df) {
  df %>%
    mutate(Panel = as.numeric(Panel),
           Panel = replace_na(Panel, -1))
}

# Fix coordinate for a specific code/year
fix_coordinates <- function(df, code, new_x, new_y, new_lat, new_lon) {
  df %>%
    mutate(
      xCoordinate = if_else(Code == code, new_x, xCoordinate),
      yCoordinate = if_else(Code == code, new_y, yCoordinate),
      Latitude    = if_else(Code == code, new_lat, Latitude),
      Longitude   = if_else(Code == code, new_lon, Longitude)
    )
}

# Convert UTM to lat/lon
utm_to_latlon <- function(df, x_col = "xCoordinate", y_col = "yCoordinate", epsg = 32619) {
  sf_obj <- st_as_sf(df, coords = c(x_col, y_col), crs = epsg)
  latlon <- st_transform(sf_obj, crs = 4326)
  coords <- st_coordinates(latlon)
  df$Longitude <- coords[, "X"]
  df$Latitude  <- coords[, "Y"]
  df
}


#----------------------------#
####    Apply Functions   ####
#----------------------------#

# Species by strata
species_by_strata_2015_2024 <- safe_bind_rows(species_by_strata_tlu, species_by_strata_2024) %>%
  format_dates(Date)

# Species list
species_list_2015_2024 <- bind_rows(
  species_list_tlu %>% mutate(Collected = as.integer(Collected)),
  species_list_2024 %>% mutate(Collected = as.integer(Collected))
) %>% format_dates(Date)

# Locations
locations_2015_2023 <- locations_2015_2023 %>% fix_panel() %>% utm_to_latlon()
locations_2024_clean <- locations_2024 %>% fix_coordinates("GRME02", 563071, 4913162, 44.36899, -68.20840)
locations_2015_2024 <- safe_bind_rows(locations_2015_2023, locations_2024_clean) %>%
  format_dates(Date_Established)

# Visits
visits_2015 <- visits_2015 %>%
  mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer)) %>%
  fix_panel()
visits_2024 <- visits_2024 %>%
  mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer))
visits_2015_2024 <- safe_bind_rows(visits_2015, visits_2024) %>% format_dates(Date)

# RAM stressors
RAM_stressors_2015 <- RAM_stressors_2015 %>% fix_panel()
RAM_stressors_2015_2024 <- safe_bind_rows(RAM_stressors_2015, RAM_stressors_2024) %>% format_dates(Date)

# Vertical complexity
vertical_complexity_2015 <- vertical_complexity_2015 %>% fix_panel()
vertical_complexity_2015_2024 <- safe_bind_rows(vertical_complexity_2015, vertical_complexity_2024) %>%
  format_dates(Date)

# AA_char
AA_char_2015 <- AA_char_2015 %>% fix_panel()
AA_char_2015_2024 <- safe_bind_rows(AA_char_2015, AA_char_2024) %>%
  format_dates(Date)


# Save outputs as CSV
# write.csv(species_by_strata_2015_2024, "data/raw_data/Glen_veg_data/species_by_strata_2015_2024.csv", row.names = FALSE)
# write.csv(species_list_2015_2024, "data/raw_data/Glen_veg_data/species_list_2015_2024.csv", row.names = FALSE)
# write.csv(locations_2015_2024, "data/raw_data/Glen_veg_data/locations_2015_2024.csv", row.names = FALSE)
# write.csv(locations_2024_clean, "data/raw_data/Glen_veg_data/locations_2024_clean.csv", row.names = FALSE)
# write.csv(visits_2015_2024, "data/raw_data/Glen_veg_data/visits_2015_2024.csv", row.names = FALSE)
# write.csv(RAM_stressors_2015_2024, "data/raw_data/Glen_veg_data/RAM_stressors_2015_2024.csv", row.names = FALSE)
# write.csv(vertical_complexity_2015_2024, "data/raw_data/Glen_veg_data/vertical_complexity_2015_2024.csv", row.names = FALSE)
# write.csv(AA_char_2015_2024, "data/raw_data/Glen_veg_data/AA_char_2015_2024.csv", row.names = FALSE)







#### GRAVEYARD ####-------------------------------------------------------------



# #### combine 2024 data with 2015-2023 data ####-------------------------------
# 
# ## merge species_by_strata datasets
# # first check all column names are the same
# identical(names(species_by_strata_tlu), names(species_by_strata_2024))
# 
# # merge
# species_by_strata_2015_2024 <- bind_rows(species_by_strata_tlu, species_by_strata_2024)
# 
# # fix date column format
# species_by_strata_2015_2024$Date <- parse_date_time(species_by_strata_2015_2024$Date, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# species_by_strata_2015_2024$Date <- as.Date(species_by_strata_2015_2024$Date)
# species_by_strata_2015_2024$Date <- format(species_by_strata_2015_2024$Date, "%Y-%m-%d")
# 
# 
# 
# 
# ## merge species_list datasets
# # first check all column names are the same
# identical(names(species_list_tlu), names(species_list_2024))
# 
# # convert collected column to same class = integer
# species_list_tlu$Collected <- as.integer(species_list_tlu$Collected)
# species_list_2024$Collected <- as.integer(species_list_2024$Collected)
# 
# # merge
# species_list_2015_2024 <- bind_rows(species_list_tlu, species_list_2024)
# 
# # fix date column format
# species_list_2015_2024$Date <- parse_date_time(species_list_2015_2024$Date, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# species_list_2015_2024$Date <- as.Date(species_list_2015_2024$Date)
# species_list_2015_2024$Date <- format(species_list_2015_2024$Date, "%Y-%m-%d")
# 
# 
# 
# 
# ## merge locations datasets
# # first check all column names are the same
# identical(names(locations_2015_2023), names(locations_2024))
# 
# # fill in panel column with -1
# locations_2015_2023 <- locations_2015_2023 %>% 
#   mutate(Panel = as.numeric(Panel),
#          Panel = replace_na(Panel, -1))
# 
# # fix coordinates mistake in locations_2024
# locations_2024_clean <- locations_2024 %>% 
#   mutate(
#     xCoordinate = case_when(
#       Code == "GRME02" ~ 563071,
#       TRUE ~ xCoordinate
#     ),
#     yCoordinate = case_when(
#       Code == "GRME02" ~ 4913162,
#       TRUE ~ yCoordinate
#     ),
#     Latitude = case_when(
#       Code == "GRME02" ~ 44.36899,
#       TRUE ~ Latitude
#     ),
#     Longitude = case_when(
#       Code == "GRME02" ~ -68.20840,
#       TRUE ~ Longitude
#     )
#   )
# 
# 
# # Add full EPSG code for UTM zone 19N = EPSG:32619
# locations_2015_2023_sf <- st_as_sf(locations_2015_2023, coords = c("xCoordinate", "yCoordinate"), crs = 32619)
# 
# # Transform to latitude/longitude (WGS 84)
# locations_2015_2023_latlon <- st_transform(locations_2015_2023_sf, crs = 4326)
# 
# # View converted coordinates
# coords <- st_coordinates(locations_2015_2023_latlon)
# locations_2015_2023$Longitude <- coords[, "X"]
# locations_2015_2023$Latitude  <- coords[, "Y"]
# view(locations_2015_2023)
# 
# # merge
# locations_2015_2024 <- bind_rows(locations_2015_2023, locations_2024_clean)
# 
# # fix date column format
# locations_2015_2024$Date_Established <- parse_date_time(locations_2015_2024$Date_Established, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# locations_2015_2024$Date_Established <- as.Date(locations_2015_2024$Date_Established)
# locations_2015_2024$Date_Established <- format(locations_2015_2024$Date_Established, "%Y-%m-%d")
# 
# 
# 
# 
# 
# ## merge visits datasets
# # first check all column names are the same
# identical(names(visits_2015), names(visits_2024))
# 
# # convert visits column to same class = integer
# visits_2015 <- visits_2015 %>%
#   mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer),
#          
#          # fill in panel column with -1
#          Panel = as.numeric(Panel),
#          Panel = replace_na(Panel, -1)
#          
#          )
# 
# visits_2024 <- visits_2024 %>%
#   mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer))
# 
# # merge
# visits_2015_2024 <- bind_rows(visits_2015, visits_2024)
# 
# # fix date column format
# visits_2015_2024$Date <- parse_date_time(visits_2015_2024$Date, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# visits_2015_2024$Date <- as.Date(visits_2015_2024$Date)
# visits_2015_2024$Date <- format(visits_2015_2024$Date, "%Y-%m-%d")
# 
# 
# 
# 
# 
# ## merge RAM_stressors datasets
# # first check all column names are the same
# identical(names(RAM_stressors_2015), names(RAM_stressors_2024))
# 
# # fill in panel column with -1
# RAM_stressors_2015 <- RAM_stressors_2015 %>% 
#   mutate(Panel = as.numeric(Panel),
#          Panel = replace_na(Panel, -1))
# 
# # merge
# RAM_stressors_2015_2024 <- bind_rows(RAM_stressors_2015, RAM_stressors_2024)
# 
# # fix date column format
# RAM_stressors_2015_2024$Date <- parse_date_time(RAM_stressors_2015_2024$Date, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# RAM_stressors_2015_2024$Date <- as.Date(RAM_stressors_2015_2024$Date)
# RAM_stressors_2015_2024$Date <- format(RAM_stressors_2015_2024$Date, "%Y-%m-%d")
# 
# 
# 
# 
# ## merge vertical_complexity datasets 
# # first check all column names are the same
# identical(names(vertical_complexity_2015), names(vertical_complexity_2024))
# 
# # first fill in panel column with -1
# vertical_complexity_2015 <- vertical_complexity_2015 %>% 
#   mutate(Panel = as.numeric(Panel),
#          Panel = replace_na(Panel, -1))
# 
# # merge
# vertical_complexity_2015_2024 <- bind_rows(vertical_complexity_2015, vertical_complexity_2024)
# 
# # fix date column format
# vertical_complexity_2015_2024$Date <- parse_date_time(vertical_complexity_2015_2024$Date, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY"))
# vertical_complexity_2015_2024$Date <- as.Date(vertical_complexity_2015_2024$Date)
# vertical_complexity_2015_2024$Date <- format(vertical_complexity_2015_2024$Date, "%Y-%m-%d")
# 
# 
# 
# # Save outputs as CSV
# # write.csv(species_by_strata_2015_2024, "data/raw_data/Glen_veg_data/species_by_strata_2015_2024.csv", row.names = FALSE)
# # write.csv(species_list_2015_2024, "data/raw_data/Glen_veg_data/species_list_2015_2024.csv", row.names = FALSE)
# # write.csv(locations_2015_2024, "data/raw_data/Glen_veg_data/locations_2015_2024.csv", row.names = FALSE)
# # write.csv(locations_2024_clean, "data/raw_data/Glen_veg_data/locations_2024_clean.csv", row.names = FALSE)
# # write.csv(visits_2015_2024, "data/raw_data/Glen_veg_data/visits_2015_2024.csv", row.names = FALSE)
# # write.csv(RAM_stressors_2015_2024, "data/raw_data/Glen_veg_data/RAM_stressors_2015_2024.csv", row.names = FALSE)
# # write.csv(vertical_complexity_2015_2024, "data/raw_data/Glen_veg_data/vertical_complexity_2015_2024.csv", row.names = FALSE)
# 




