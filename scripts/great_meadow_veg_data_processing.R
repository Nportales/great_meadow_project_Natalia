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

## 2015-2023 FOA/Glen veg data ##

species_by_strata_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/species_by_strata_2015_2023.csv") %>%
  as_tibble()

species_list_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/species_list_2015_2023.csv") %>%
  as_tibble()

locations_2015_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/locations_2015_2023.csv") %>%
  as_tibble()

visits_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/visits_2015_2023.csv") %>%
  as_tibble()

vertical_complexity_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/vertical_complexity_2015_2023.csv") %>%
  as_tibble()

RAM_stressors_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/RAM_stressors_2015_2023.csv") %>%
  as_tibble()

AA_char_2015 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/AA_char_2015_2023.csv")

tlu_Plant <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/tlu_Plant.csv") %>%
  as_tibble()

## 2024 FOA/Glen veg data ##

species_by_strata_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/species_by_strata.csv") %>%
  as_tibble()

species_list_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/species_list.csv") %>%
  as_tibble()

locations <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/locations.csv") %>%
  as_tibble()

visits_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/visits.csv") %>%
  as_tibble()

vertical_complexity_new<- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/vertical_complexity.csv") %>%
  as_tibble()

RAM_stressors_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/RAM_stressors.csv") %>%
  as_tibble()

AA_char_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/AA_char.csv")

tlu_Plant_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/tlu_Plant.csv")


## site data ##

sites <- read.csv("data/raw_data/monitoring_sites_GRME_GIME_metadata.csv")


#-----------------------#
####    Data Manip   ####
#-----------------------#

# ##latin name check
# tlu_latin_mismatch <- anti_join(
#   tlu_Plant_new %>% distinct(Latin_Name),
#   tlu_Plant %>% distinct(Latin_Name),
#   by = "Latin_Name"
# )
# 
# #search tlu for mismatch latin names
# search_tlu <- tlu_Plant_new %>%
#   filter(Latin_Name %in% c(
#     "Acer saccharum",
#     "Juncus bufonius",
#     "Moehringia lateriflora",
#     "Panicum capillare",
#     "Polypodium appalachianum",
#     "Frangula alnus var. asplenifolia",
#     "Salix sericea"
#   ))


#### 2013-2015 Glen Veg data ####

## combine tlu_Plant with species_list by common columns------------------------

#first find latin names that are in species_list and not in tlu_Plant
latin_mismatch_list <- anti_join(species_list_2015, tlu_Plant, by = "Latin_Name")

#search tlu for mismatch latin names
search_tlu <- filter(tlu_Plant, Latin_Name == "Utricularua vulgaris")

#rename latin name species using case_when
species_list_clean <- species_list_2015 %>% 
  
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
check_latin_mismatch_list <- anti_join(species_list_clean, tlu_Plant, by = "Latin_Name")

#combine datasets
# Define the key column
key_col <- "Latin_Name"

# Step 1: Find common columns (excluding the key)
common_cols <- intersect(setdiff(names(species_list_clean), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))

# Step 3: Join and update values
species_list_tlu <- species_list_clean %>%
  left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_clean")) %>%
  mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_clean")), .x))) %>%
  select(-ends_with("_clean"))

#check that the same columns were kept in dataset 1
setdiff(names(species_list_clean), names(species_list_tlu))

# Save outputs as CSV
# write.csv(species_list_tlu, "data/processed_data/species_list_tlu_2015_2023.csv", row.names = FALSE)




## combine tlu_Plant with species_by_strata by common columns-------------------

#first find latin names that are in species_by_strata and not in tlu_Plant
strata_latin_mismatch <- anti_join(species_by_strata_2015, tlu_Plant, by = "Latin_Name")

#rename latin name species using case_when
species_by_strata_clean <- species_by_strata_2015 %>% 
  
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
common_cols_2 <- intersect(setdiff(names(species_by_strata_clean), key_col), names(tlu_Plant))

# Step 2: Join only on key and shared columns
tlu_Plant_sub_2 <- tlu_Plant %>% select(all_of(c(key_col, common_cols_2)))

# Step 3: Join and update values
species_by_strata_tlu <- species_by_strata_clean %>%
  left_join(tlu_Plant_sub_2, by = key_col, suffix = c("", "_clean")) %>%
  mutate(across(all_of(common_cols_2), ~ coalesce(get(paste0(cur_column(), "_clean")), .x))) %>%
  select(-ends_with("_clean"))

#check that the same columns were kept in dataset 1
setdiff(names(species_by_strata_clean), names(species_by_strata_tlu))

# Save outputs as CSV
# write.csv(species_by_strata_tlu, "data/processed_data/species_by_strata_tlu_2015_2023.csv", row.names = FALSE)



# Fix bryophyte cover column in visits dataset--------------------------------- 

visits_2015_clean <- visits_2015 %>%
  # isolate 2015 and 2020 data
  left_join(
    visits_2015 %>% filter(Year == 2020) %>% select(Code, bryo_2020 = Bryophyte_Cover),
    by = "Code"
  ) %>%
  # Replace missing 2015 values with 2020 values
  mutate(Bryophyte_Cover = ifelse(Year == 2015 & is.na(Bryophyte_Cover), bryo_2020, Bryophyte_Cover)) %>%
  # Drop helper column
  select(-bryo_2020)




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
species_by_strata_2015_2025 <- safe_bind_rows(species_by_strata_tlu, species_by_strata_new) %>%
  format_dates(Date)

# Species list
species_list_2015_2025 <- bind_rows(
  species_list_tlu %>% mutate(Collected = as.integer(Collected)),
  species_list_new %>% mutate(Collected = as.integer(Collected))
) %>% format_dates(Date)

# Locations
locations_new_clean <- locations %>% 
  fix_coordinates("GRME02", 563071, 4913162, 44.36899, -68.20840) %>% 
  fix_coordinates("GRME08", 563371, 4912986, 44.36738, -68.20466) %>%
  format_dates(Date_Established)

# Visits
visits_2015_clean_2 <- visits_2015_clean %>%
  mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer)) %>%
  fix_panel()
visits_new_2 <- visits_new %>%
  mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer))
visits_2015_2025 <- safe_bind_rows(visits_2015_clean_2, visits_new_2) %>% format_dates(Date)

# RAM stressors
RAM_stressors_2015 <- RAM_stressors_2015 %>% fix_panel()
RAM_stressors_2015_2025 <- safe_bind_rows(RAM_stressors_2015, RAM_stressors_new) %>% format_dates(Date)

# Vertical complexity
vertical_complexity_2015 <- vertical_complexity_2015 %>% fix_panel()
vertical_complexity_2015_2025 <- safe_bind_rows(vertical_complexity_2015, vertical_complexity_new) %>%
  format_dates(Date)

# AA_char
AA_char_2015 <- AA_char_2015 %>% fix_panel()
AA_char_2015_2025 <- safe_bind_rows(AA_char_2015, AA_char_new) %>%
  format_dates(Date)

# Save outputs as CSV
# write.csv(species_by_strata_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/species_by_strata_2015_2025.csv", row.names = FALSE)
# write.csv(species_list_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/species_list_2015_2025.csv", row.names = FALSE)
# write.csv(locations, "data/raw_data/vegetation_data/FOA_veg_data/locations.csv", row.names = FALSE)
# write.csv(visits_2015_2025, "data/raw_data//vegetation_data/FOA_veg_data/visits_2015_2025.csv", row.names = FALSE)
# write.csv(RAM_stressors_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/RAM_stressors_2015_2025.csv", row.names = FALSE)
# write.csv(vertical_complexity_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/vertical_complexity_2015_2025.csv", row.names = FALSE)
# write.csv(AA_char_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/AA_char_2015_2025.csv", row.names = FALSE)


#### change coordinates for site data ####--------------------------------------

# Convert UTM to lat/lon
convert_latlon <- function(df, x_col = "xcoord", y_col = "ycoord", epsg = 32619) {
  sf_obj <- st_as_sf(df, coords = c(x_col, y_col), crs = epsg)
  latlon <- st_transform(sf_obj, crs = 4326)
  coords <- st_coordinates(latlon)
  df$longitude <- coords[, "X"]
  df$latitude  <- coords[, "Y"]
  df
}

sites.clean <- sites %>% 
  convert_latlon() %>% 
  select(site.name,
         latitude,
         longitude,
         site.type,
         wetland.name,
         source
         )

# Save outputs as CSV
# write.csv(sites.clean, "data/processed_data/monitoring_sites.csv", row.names = FALSE)


## Quality control check-point code ## -----------------------------------------

# check latin names are up to date and consistent

check_tsn_accepted <- function(data, tlu,
                               tsn_col,
                               tsn_accepted_col,
                               latin_col,
                               dataset_name) {
  
  data %>%
    select(
      TSN = {{ tsn_col }},
      TSN_Accepted_data = {{ tsn_accepted_col }},
      Latin_Name_data = {{ latin_col }}
    ) %>%
    distinct() %>%
    left_join(
      tlu %>%
        select(
          TSN_Accepted = TSN,
          Accepted_Latin_Name
        ),
      by = c("TSN_Accepted_data" = "TSN_Accepted")
    ) %>%
    mutate(
      tsn_accepted_missing_in_tlu = is.na(Accepted_Latin_Name),
      latin_matches_accepted =
        !is.na(Accepted_Latin_Name) &
        Latin_Name_data == Accepted_Latin_Name
    ) %>%
    filter(
      tsn_accepted_missing_in_tlu | !latin_matches_accepted
    ) %>%
    mutate(dataset = dataset_name)
}


# check species_by_strata

latin_check_strata <- check_tsn_accepted(
  data = species_by_strata_2015_2025,
  tlu = tlu_Plant_new,
  tsn_col = TSN,
  tsn_accepted_col = TSN_Accepted,
  latin_col = Latin_Name,
  dataset_name = "species_by_strata_2015_2025"
)

# check species_list

latin_check_lists <- check_tsn_accepted(
  data = species_list_2015_2025,
  tlu = tlu_Plant_new,
  tsn_col = TSN,
  tsn_accepted_col = TSN_Accepted,
  latin_col = Latin_Name,
  dataset_name = "species_list_2015_2025"
)


# combine and review
latin_name_issues <- bind_rows(
  latin_check_strata,
  latin_check_lists
)

#search tlu for mismatch latin names
search_tlu <- filter(tlu_Plant_new, Latin_Name == "Rubus canadensis")
search_tlu_tsn <- filter(tlu_Plant_new, TSN_Accepted == "28562")

#search tlu for mismatch latin names
search_list <- filter(species_list_2015_2025, Accepted_Latin_Name == "Utricularia vulgaris")


# TSN is NA or missing
find_bad_tsn <- function(data, tsn_col, latin_col, dataset_name) {
  
  data %>%
    mutate(TSN_check = {{ tsn_col }}) %>%
    filter(
      is.na(TSN_check) |
        startsWith(as.character(TSN_check), "-9999")
    ) %>%
    transmute(
      dataset = dataset_name,
      TSN = TSN_check,
      Latin_Name = {{ latin_col }}
    )
}

bad_tsn_strata <- find_bad_tsn(
  species_by_strata_2015_2025,
  TSN,
  Latin_Name,
  "species_by_strata_2015_2025"
)

bad_tsn_list <- find_bad_tsn(
  species_list_2015_2025,
  TSN,
  Latin_Name,
  "species_list_2015_2025"
)

bad_tsn_summary <- bind_rows(
  bad_tsn_strata,
  bad_tsn_list
)




#### 2024 script copy #### -----------------------------------------------------



# ## Great Meadow data processing ##
# 
# #---------------------------------------------#
# ####        Load Required Packages         ####
# #---------------------------------------------#
# 
# library(tidyverse)
# library(dplyr)
# library(lubridate)
# library(sf)
# 
# #-----------------------#
# ####    Read Data    ####
# #-----------------------#
# 
# #Reading in CSVs as a tibble
# 
# ## 2015-2023 FOA/Glen veg data ##
# 
# species_by_strata_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_by_strata_2015_2023.csv") %>%
#   as_tibble()
# 
# species_list_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_list_2015_2023.csv") %>%
#   as_tibble()
# 
# tlu_Plant <- read.csv("data/raw_data/Glen_veg_data/tlu_Plant.csv") %>%
#   as_tibble()
# 
# locations_2015_2023 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/locations_2015_2023.csv") %>%
#   as_tibble()
# 
# visits_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/visits_2015_2023.csv") %>%
#   as_tibble()
# 
# vertical_complexity_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/vertical_complexity_2015_2023.csv") %>%
#   as_tibble()
# 
# RAM_stressors_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/RAM_stressors_2015_2023.csv") %>%
#   as_tibble()
# 
# AA_char_2015 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/AA_char_2015_2023.csv")
# 
# 
# ## 2024 FOA/Glen veg data ##
# 
# species_by_strata_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_by_strata_2024.csv") %>%
#   as_tibble()
# 
# species_list_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/species_list_2024.csv") %>%
#   as_tibble()
# 
# locations_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/locations_2024.csv") %>%
#   as_tibble()
# 
# visits_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/visits_2024.csv") %>%
#   as_tibble()
# 
# vertical_complexity_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/vertical_complexity_2024.csv") %>%
#   as_tibble()
# 
# RAM_stressors_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/RAM_stressors_2024.csv") %>%
#   as_tibble()
# 
# AA_char_2024 <- read.csv("data/raw_data/Glen_veg_data/old_raw_veg_data/AA_char_2024.csv")
# 
# 
# ## site data ##
# 
# sites <- read.csv("data/raw_data/monitoring_sites_GRME_GIME_metadata.csv")
# 
# 
# #-----------------------#
# ####    Data Manip   ####
# #-----------------------#
# 
# #### 2013-2015 Glen Veg data ####
# 
# ## combine tlu_Plant with species_list by common columns------------------------
# 
# #first find latin names that are in species_list and not in tlu_Plant
# latin_mismatch_list <- anti_join(species_list_2015, tlu_Plant, by = "Latin_Name")
# 
# #search tlu for mismatch latin names
# search_tlu <- filter(tlu_Plant, Latin_Name == "Utricularua vulgaris")
# 
# #rename latin name species using case_when
# species_list_new <- species_list_2015 %>% 
#   
#   mutate(
#     Latin_Name =
#       case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
#                 Latin_Name == "Lysimachia borealis" ~ "Trientalis borealis",
#                 Latin_Name == "Glyceria laxa" ~ "Glyceria canadensis",
#                 Latin_Name == "Viola pallens" ~ "Viola macloskeyi",
#                 Latin_Name == "Rhodora" ~ "Rhododendron canadense",
#                 Latin_Name == "Malus sieboldii" ~ "Malus toringo",
#                 Latin_Name == "Rubus idaeus spp strigosus" ~ "Rubus idaeus",
#                 Latin_Name == "Juncas pylaei" ~ "Juncus effusus",
#                 Latin_Name == "Dulichium" ~ "Dulichium arundinaceum",
#                 Latin_Name == "Unknown Agrostis" ~ "Agrostis",
#                 Latin_Name == "Brachyelytrum" ~ "Brachyelytrum aristosum",
#                 Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
#                 Latin_Name == "Potamogeton berchtoldii" ~ "Potamogeton pusillus",
#                 Latin_Name == "Brachyelytum aristosum" ~ "Brachyelytrum aristosum",
#                 Latin_Name == "Juncus pylaei" ~ "Juncus effusus",
#                 Latin_Name == "Utricularia vulgaris" ~ "Utricularua vulgaris",
#                 TRUE ~ Latin_Name),
#     
#     # fill in panel column with -1
#     Panel = as.numeric(Panel),
#     Panel = replace_na(Panel, -1)
#     
#   )
# 
# #check renaming worked
# check_latin_mismatch_list <- anti_join(species_list_new, tlu_Plant, by = "Latin_Name")
# 
# #combine datasets
# # Define the key column
# key_col <- "Latin_Name"
# 
# # Step 1: Find common columns (excluding the key)
# common_cols <- intersect(setdiff(names(species_list_new), key_col), names(tlu_Plant))
# 
# # Step 2: Join only on key and shared columns
# tlu_Plant_sub <- tlu_Plant %>% select(all_of(c(key_col, common_cols)))
# 
# # Step 3: Join and update values
# species_list_tlu <- species_list_new %>%
#   left_join(tlu_Plant_sub, by = key_col, suffix = c("", "_new")) %>%
#   mutate(across(all_of(common_cols), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
#   select(-ends_with("_new"))
# 
# #check that the same columns were kept in dataset 1
# setdiff(names(species_list_new), names(species_list_tlu))
# 
# # Save outputs as CSV
# # write.csv(species_list_tlu, "data/processed_data/species_list_tlu_2015_2023.csv", row.names = FALSE)
# 
# 
# 
# 
# ## combine tlu_Plant with species_by_strata by common columns-------------------
# 
# #first find latin names that are in species_by_strata and not in tlu_Plant
# strata_latin_mismatch <- anti_join(species_by_strata_2015, tlu_Plant, by = "Latin_Name")
# 
# #rename latin name species using case_when
# species_by_strata_new <- species_by_strata_2015 %>% 
#   
#   mutate(
#     Latin_Name =
#       case_when(Latin_Name == "Frangula alnus" ~ "Rhamnus frangula",
#                 Latin_Name == "Alnus incana ssp. rugosa" ~ "Alnus incana",
#                 TRUE ~ Latin_Name),
#     
#     # fill in panel column with -1
#     Panel = as.numeric(Panel),
#     Panel = replace_na(Panel, -1))
# 
# #combine datasets
# # Define the key column
# key_col <- "Latin_Name"
# 
# # Step 1: Find common columns (excluding the key)
# common_cols_2 <- intersect(setdiff(names(species_by_strata_new), key_col), names(tlu_Plant))
# 
# # Step 2: Join only on key and shared columns
# tlu_Plant_sub_2 <- tlu_Plant %>% select(all_of(c(key_col, common_cols_2)))
# 
# # Step 3: Join and update values
# species_by_strata_tlu <- species_by_strata_new %>%
#   left_join(tlu_Plant_sub_2, by = key_col, suffix = c("", "_new")) %>%
#   mutate(across(all_of(common_cols_2), ~ coalesce(get(paste0(cur_column(), "_new")), .x))) %>%
#   select(-ends_with("_new"))
# 
# #check that the same columns were kept in dataset 1
# setdiff(names(species_by_strata_new), names(species_by_strata_tlu))
# 
# # Save outputs as CSV
# # write.csv(species_by_strata_tlu, "data/processed_data/species_by_strata_tlu_2015_2023.csv", row.names = FALSE)
# 
# 
# 
# # Fix bryophyte cover column in visits dataset--------------------------------- 
# 
# visits_2015_clean <- visits_2015 %>%
#   # isolate 2015 and 2020 data
#   left_join(
#     visits_2015 %>% filter(Year == 2020) %>% select(Code, bryo_2020 = Bryophyte_Cover),
#     by = "Code"
#   ) %>%
#   # Replace missing 2015 values with 2020 values
#   mutate(Bryophyte_Cover = ifelse(Year == 2015 & is.na(Bryophyte_Cover), bryo_2020, Bryophyte_Cover)) %>%
#   # Drop helper column
#   select(-bryo_2020)
# 
# 
# 
# 
# #### combine 2024 data with 2015-2023 data ####---------------------------------
# 
# #---------------------------#
# ####    Load Functions   ####
# #---------------------------#
# 
# # Format a date column consistently
# format_dates <- function(df, date_col) {
#   df %>%
#     mutate({{ date_col }} := parse_date_time({{ date_col }}, orders = c("ymd", "mdy", "dmy", "Ymd HMS", "mdY", "BdY")),
#            {{ date_col }} := as.Date({{ date_col }}),
#            {{ date_col }} := format({{ date_col }}, "%Y-%m-%d"))
# }
# 
# # Bind two datasets after checking identical column names
# safe_bind_rows <- function(df1, df2) {
#   stopifnot(identical(names(df1), names(df2)))
#   bind_rows(df1, df2)
# }
# 
# # Clean panel column to numeric with NA replaced by -1
# fix_panel <- function(df) {
#   df %>%
#     mutate(Panel = as.numeric(Panel),
#            Panel = replace_na(Panel, -1))
# }
# 
# # Fix coordinate for a specific code/year
# fix_coordinates <- function(df, code, new_x, new_y, new_lat, new_lon) {
#   df %>%
#     mutate(
#       xCoordinate = if_else(Code == code, new_x, xCoordinate),
#       yCoordinate = if_else(Code == code, new_y, yCoordinate),
#       Latitude    = if_else(Code == code, new_lat, Latitude),
#       Longitude   = if_else(Code == code, new_lon, Longitude)
#     )
# }
# 
# # Convert UTM to lat/lon
# utm_to_latlon <- function(df, x_col = "xCoordinate", y_col = "yCoordinate", epsg = 32619) {
#   sf_obj <- st_as_sf(df, coords = c(x_col, y_col), crs = epsg)
#   latlon <- st_transform(sf_obj, crs = 4326)
#   coords <- st_coordinates(latlon)
#   df$Longitude <- coords[, "X"]
#   df$Latitude  <- coords[, "Y"]
#   df
# }
# 
# 
# #----------------------------#
# ####    Apply Functions   ####
# #----------------------------#
# 
# # Species by strata
# species_by_strata_2015_2024 <- safe_bind_rows(species_by_strata_tlu, species_by_strata_2024) %>%
#   format_dates(Date)
# 
# # Species list
# species_list_2015_2024 <- bind_rows(
#   species_list_tlu %>% mutate(Collected = as.integer(Collected)),
#   species_list_2024 %>% mutate(Collected = as.integer(Collected))
# ) %>% format_dates(Date)
# 
# # Locations
# locations_2015_2023 <- locations_2015_2023 %>% fix_panel() %>% utm_to_latlon()
# locations_2024_clean <- locations_2024 %>% 
#   fix_coordinates("GRME02", 563071, 4913162, 44.36899, -68.20840) %>% 
#   fix_coordinates("GRME08", 563371, 4912986, 44.36738, -68.20466)
# locations_2015_2024 <- safe_bind_rows(locations_2015_2023, locations_2024_clean) %>%
#   format_dates(Date_Established)
# 
# # Visits
# visits_2015_clean <- visits_2015_clean %>%
#   mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer)) %>%
#   fix_panel()
# visits_2024 <- visits_2024 %>%
#   mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer))
# visits_2015_2024 <- safe_bind_rows(visits_2015_clean, visits_2024) %>% format_dates(Date)
# 
# # RAM stressors
# RAM_stressors_2015 <- RAM_stressors_2015 %>% fix_panel()
# RAM_stressors_2015_2024 <- safe_bind_rows(RAM_stressors_2015, RAM_stressors_2024) %>% format_dates(Date)
# 
# # Vertical complexity
# vertical_complexity_2015 <- vertical_complexity_2015 %>% fix_panel()
# vertical_complexity_2015_2024 <- safe_bind_rows(vertical_complexity_2015, vertical_complexity_2024) %>%
#   format_dates(Date)
# 
# # AA_char
# AA_char_2015 <- AA_char_2015 %>% fix_panel()
# AA_char_2015_2024 <- safe_bind_rows(AA_char_2015, AA_char_2024) %>%
#   format_dates(Date)
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
# # write.csv(AA_char_2015_2024, "data/raw_data/Glen_veg_data/AA_char_2015_2024.csv", row.names = FALSE)
# 
# 
# 
# #### change coordinates for site data ####--------------------------------------
# 
# # Convert UTM to lat/lon
# convert_latlon <- function(df, x_col = "xcoord", y_col = "ycoord", epsg = 32619) {
#   sf_obj <- st_as_sf(df, coords = c(x_col, y_col), crs = epsg)
#   latlon <- st_transform(sf_obj, crs = 4326)
#   coords <- st_coordinates(latlon)
#   df$longitude <- coords[, "X"]
#   df$latitude  <- coords[, "Y"]
#   df
# }
# 
# sites.clean <- sites %>% 
#   convert_latlon() %>% 
#   select(site.name,
#          latitude,
#          longitude,
#          site.type,
#          wetland.name,
#          source
#   )
# 
# # Save outputs as CSV
# # write.csv(sites.clean, "data/processed_data/monitoring_sites.csv", row.names = FALSE)










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




