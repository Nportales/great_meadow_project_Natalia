#### FOA GRME/GIME Vegetation Data Cleaning and Merging Script ####

## This script takes the old 2015-2023 FOA veg data and combines it with new FOA veg data (2024-new) for later processing

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)

#-----------------------#
####    Read Data    ####
#-----------------------#

## 2015-2023 FOA veg data ##

species_by_strata_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/species_by_strata_2015_2023.csv") %>%
  as_tibble()

species_list_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/species_list_2015_2023.csv") %>%
  as_tibble()

locations_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/locations.csv") %>%
  as_tibble()

visits_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/visits_2015_2023.csv") %>%
  as_tibble()

vertical_complexity_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/vertical_complexity_2015_2023.csv") %>%
  as_tibble()

RAM_stressors_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/RAM_stressors_2015_2023.csv") %>%
  as_tibble()

AA_char_2023 <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2023/AA_char_2015_2023.csv") %>%
  as_tibble()

tlu_Plant <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/tlu_Plant.csv") %>%
  as_tibble()


## 2024-new FOA veg data ##

species_by_strata_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/species_by_strata.csv") %>%
  as_tibble()

species_list_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/species_list.csv") %>%
  as_tibble()

locations_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/locations.csv") %>%
  as_tibble()

visits_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/visits.csv") %>%
  as_tibble()

vertical_complexity_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/vertical_complexity.csv") %>%
  as_tibble()

RAM_stressors_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/RAM_stressors.csv") %>%
  as_tibble()

AA_char_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/AA_char.csv")

tlu_Plant_new <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/ACAD_Wetland_RAM_Data_20260129_public/tlu_Plant.csv")

## other veg data resources ##

maine_coc <- read.csv("data/raw_data/vegetation_data/Maine_CoC_20260217.csv")

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


#---------------------------------------#
####    Data Cleaning & Processing   ####
#---------------------------------------#

# Species by strata
species_by_strata_2015_2025 <- safe_bind_rows(species_by_strata_2023, species_by_strata_new) %>%
  format_dates(Date)

# Species list
species_list_2015_2025 <- bind_rows(
  species_list_2023 %>%
    mutate(Collected = readr::parse_integer(Collected, na = c("", "NA", "19%"))),
  species_list_new %>%
    mutate(Collected = as.integer(Collected))) %>%
  format_dates(Date)

# Locations
locations_clean <- locations_2023 %>% 
  fix_coordinates("GRME02", 563071, 4913162, 44.36899, -68.20840) %>% 
  fix_coordinates("GRME08", 563371, 4912986, 44.36738, -68.20466) %>%
  format_dates(Date_Established)

# Visits
visits_new_2 <- visits_new %>%
  mutate(across(c(Depth_3, DrySeasonWaterTable, FiddlerCrabBurrows, ShallowAquitard), as.integer))
visits_2015_2025 <- safe_bind_rows(visits_2023, visits_new_2) %>% format_dates(Date)

# RAM stressors
RAM_stressors_2015_2025 <- safe_bind_rows(RAM_stressors_2023, RAM_stressors_new) %>% format_dates(Date)

# Vertical complexity
vertical_complexity_2015_2025 <- safe_bind_rows(vertical_complexity_2023, vertical_complexity_new) %>%
  format_dates(Date)

# AA_char
AA_char_2015_2025 <- safe_bind_rows(AA_char_2023, AA_char_new) %>%
  format_dates(Date)


#---------------------------#
####    QA/QC Checking   ####
#---------------------------#

## function for identifying TSN-latin name mismatches (between new tlu_Plant dataset and veg datasets) for taxonomic consistency
check_tsn_latin_mismatch <- function(reference_df,
                                     compare_dfs,
                                     tsn_col = TSN,
                                     latin_col = Latin_Name,
                                     reference_label = "tlu_Plant_new",
                                     compare_labels,
                                     drop_invalid_tsn = TRUE) {
  
  tsn_col   <- rlang::enquo(tsn_col)
  latin_col <- rlang::enquo(latin_col)
  
  # Helper to standardize each dataset
  prep_df <- function(df, label) {
    df %>%
      select(!!tsn_col, !!latin_col) %>%
      distinct() %>%
      mutate(dataset = label)
  }
  
  # Reference dataset
  ref <- prep_df(reference_df, reference_label)
  
  # Comparison datasets
  comps <- purrr::map2_dfr(compare_dfs, compare_labels, prep_df)
  
  all_data <- bind_rows(ref, comps)
  
  # Optional TSN cleaning
  if (drop_invalid_tsn) {
    all_data <- all_data %>%
      filter(!is.na(!!tsn_col) & !!tsn_col > 0)
  }
  
  # Identify TSNs with multiple Latin names
  mismatches_long <- all_data %>%
    group_by(!!tsn_col) %>%
    filter(n_distinct(!!latin_col) > 1) %>%
    ungroup()
  
  # Wide view for easy inspection
  mismatches_wide <- mismatches_long %>%
    pivot_wider(
      names_from = dataset,
      values_from = !!latin_col
    )
  
  list(
    long = mismatches_long,
    wide = mismatches_wide
  )
}

## run functions
tsn_latin_results <- check_tsn_latin_mismatch(
  reference_df  = tlu_Plant_new,
  compare_dfs   = list(species_by_strata_2015_2025, species_list_2015_2025),
  compare_labels = c("species_by_strata_2015_2025", "species_list_2015_2025"),
  tsn_col       = TSN,
  latin_col     = Latin_Name
)

view(tsn_latin_results$long)   # long-format mismatches
view(tsn_latin_results$wide)   # wide-format QA table


## function for identifying species in veg datasets but not in tlu_Plant by TSN (potential species that need to be added to tlu_Plant)

veg_tsn <- bind_rows(
  species_by_strata_2015_2025 %>%
    select(TSN, Latin_Name) %>%
    distinct() %>%
    mutate(source_dataset = "species_by_strata_2015_2025"),
  
  species_list_2015_2025 %>%
    select(TSN, Latin_Name) %>%
    distinct() %>%
    mutate(source_dataset = "species_list_2015_2025")
)

veg_not_in_tlu <- veg_tsn %>%
  anti_join(
    tlu_Plant_new %>% select(TSN) %>% distinct(),
    by = "TSN"
  )

## code to help search for the species rows in the datasets
# search_list <- filter(species_list_2015_2025, Latin_Name == "Rhamnus alnifolia")
# search_strata <- filter(species_by_strata_2015_2025, Latin_Name == "Solidago altissima")
# search_tlu <- filter(tlu_Plant_new, Latin_Name == "Solidago altissima")
# search_tlu_old <- filter(tlu_Plant, Latin_Name == "Solidago altissima")

## add missing valid species to tlu_Plant_new if new species are identified





