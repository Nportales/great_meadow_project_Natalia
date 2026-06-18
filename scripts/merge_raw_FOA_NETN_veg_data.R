## Merging raw vegetation data from FOA and NETN RAM sites ##

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)
library(tibble)

#---------------------------#
####    Load Functions   ####
#---------------------------#

filter_fix_sites <- function(df) {
  
  df %>%
    
    # filter for sites
    filter(
      Code %in% c("R-31", "R-13", "R-04", "R-19") |
        stringr::str_detect(Code, "304")
    )
}

# Bind two datasets after checking identical column names
safe_bind_rows <- function(df1, df2) {
  stopifnot(identical(names(df1), names(df2)))
  bind_rows(df1, df2)
}

#-----------------------#
####    Read Data    ####
#-----------------------#

## 2015-new FOA veg data ##

species_by_strata_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/species_by_strata_2015_2025.csv") %>%
  as_tibble()

species_list_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/species_list_2015_2025.csv") %>%
  as_tibble()

locations_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/locations_2015_2025.csv") %>%
  as_tibble()

visits_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/visits_2015_2025.csv") %>%
  as_tibble()

vertical_complexity_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/vertical_complexity_2015_2025.csv") %>%
  as_tibble()

RAM_stressors_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/RAM_stressors_2015_2025.csv") %>%
  as_tibble()

AA_char_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/AA_char_2015_2025.csv")

tlu_Plant_FOA <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/tlu_Plant_2015_2025.csv")


## 2011-new NETN veg data ##

species_by_strata_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/species_by_strata.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

species_list_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/species_list.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

locations_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/locations.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

visits_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/visits.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

vertical_complexity_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/vertical_complexity.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

RAM_stressors_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/RAM_stressors.csv") %>%
  filter_fix_sites() %>% 
  as_tibble()

AA_char_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/AA_char.csv") %>% 
  filter_fix_sites()

tlu_Plant_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/tlu_Plant.csv")


## sites data ##

monitoring_sites <- read.csv("data/processed_data/monitoring_sites.csv") %>% as_tibble()


#---------------------------------------#
####    Data Cleaning & Processing   ####
#---------------------------------------#

# Species by strata
species_by_strata_all <- safe_bind_rows(species_by_strata_FOA, species_by_strata_NETN)

# Species list
species_list_all <- safe_bind_rows(species_list_FOA, species_list_NETN)

# Locations
locations_all <- safe_bind_rows(locations_FOA, locations_NETN)

# Visits
visits_all <- safe_bind_rows(visits_FOA, visits_NETN)

# RAM stressors
RAM_stressors_all <- safe_bind_rows(RAM_stressors_FOA, RAM_stressors_NETN)

# Vertical complexity
vertical_complexity_all <- safe_bind_rows(vertical_complexity_FOA, vertical_complexity_NETN)

# AA_char
AA_char_all <- safe_bind_rows(AA_char_FOA, AA_char_NETN)


#---------------------------#
####    QA/QC Checking   ####
#---------------------------#

## 1: check for species missing from tlu_Plant ## ------------------------------

## function for identifying species/taxa in veg datasets but not in new tlu_Plant dataset by TSN (potential species that need to be added to tlu_Plant)

veg_tsn <- bind_rows(
  species_by_strata_all %>%
    select(TSN, Latin_Name) %>%
    distinct() %>%
    mutate(source_dataset = "species_by_strata_all"),
  
  species_list_all %>%
    select(TSN, Latin_Name) %>%
    distinct() %>%
    mutate(source_dataset = "species_list_all")
)

# produce summary table of missing species
veg_not_in_tlu <- veg_tsn %>%
  anti_join(
    tlu_Plant_FOA %>% select(TSN) %>% distinct(),
    by = "TSN"
  )

## If missing species are found, code needs to be added to add missing species/taxa (additions were not needed in the past hence no code). 
## Check 1 is more like a double check there aren't species in the veg datasets not in tlu_Plant now that NETN data has been incorporated.

## 2: comprehensive check ## ---------------------------------------------------

## this function checks for any differences between veg data and the new tlu_Plant dataset and fixes differences by overwriting veg data with tlu_Plant data

sync_with_tlu <- function(data, tlu, key = "TSN", verbose = TRUE) {
  
  shared_cols <- intersect(names(data), names(tlu))
  compare_cols <- setdiff(shared_cols, key)
  
  if (length(compare_cols) == 0) {
    stop("No shared columns to compare.")
  }
  
  comparison_df <- data %>%
    dplyr::inner_join(
      tlu,
      by = key,
      suffix = c("_data", "_tlu")
    )
  
  mismatch_matrix <- sapply(compare_cols, function(col) {
    
    data_col <- comparison_df[[paste0(col, "_data")]]
    tlu_col  <- comparison_df[[paste0(col, "_tlu")]]
    
    !(data_col == tlu_col |
        (is.na(data_col) & is.na(tlu_col)))
    
  }, simplify = "matrix")
  
  mismatches <- comparison_df[rowSums(mismatch_matrix) > 0, , drop = FALSE]
  
  diff_counts <- colSums(mismatch_matrix)
  cols_that_differ <- names(diff_counts[diff_counts > 0])
  
  mismatch_report <- NULL
  
  if (nrow(mismatches) > 0) {
    
    mismatch_report <- mismatches %>%
      dplyr::select(
        all_of(key),
        paste0(cols_that_differ, "_data"),
        paste0(cols_that_differ, "_tlu")
      )
  }
  
  keys_to_update <- mismatches[[key]]
  
  updated_data <- data %>%
    dplyr::rows_update(
      tlu %>%
        dplyr::filter(.data[[key]] %in% keys_to_update) %>%
        dplyr::select(all_of(c(key, compare_cols))),
      by = key
    )
  
  if (verbose) {
    message("Rows updated: ", nrow(mismatches))
    message(
      "Columns changed: ",
      ifelse(
        length(cols_that_differ) == 0,
        "None",
        paste(cols_that_differ, collapse = ", ")
      )
    )
  }
  
  list(
    updated_data = updated_data,
    mismatches = mismatch_report,
    changed_columns = cols_that_differ
  )
}

# update species_list
result_species <- sync_with_tlu(
  species_list_all,
  tlu_Plant_FOA
)

species_list_all_updated <- result_species$updated_data
species_mismatches <- result_species$mismatches


# update species_by_strata
result_strata <- sync_with_tlu(
  species_by_strata_all,
  tlu_Plant_FOA
)

species_by_strata_all_updated <- result_strata$updated_data
strata_mismatches <- result_strata$mismatches


# QA function to check spatial coordinates
qa_check_coords <- function(
    df,
    sites_df,
    df_key = "Code",
    sites_key = "site.name",
    df_lat = "Latitude",
    df_lon = "Longitude",
    sites_lat = "latitude",
    sites_lon = "longitude",
    tol = 0.00001,
    return_only_mismatch = TRUE,
    verbose = TRUE
) {
  
  needed_df <- c(df_key, df_lat, df_lon)
  needed_sites <- c(sites_key, sites_lat, sites_lon)
  
  if (!all(needed_df %in% names(df))) {
    stop(
      "Missing columns in df: ",
      paste(setdiff(needed_df, names(df)), collapse = ", ")
    )
  }
  
  if (!all(needed_sites %in% names(sites_df))) {
    stop(
      "Missing columns in sites_df: ",
      paste(setdiff(needed_sites, names(sites_df)), collapse = ", ")
    )
  }
  
  joined <- dplyr::left_join(
    df,
    sites_df,
    by = setNames(sites_key, df_key),
    suffix = c(".data", ".expected")
  )
  
  result <- joined %>%
    dplyr::mutate(
      lat_diff = abs(.data[[df_lat]] - .data[[sites_lat]]),
      lon_diff = abs(.data[[df_lon]] - .data[[sites_lon]]),
      coord_match = lat_diff < tol & lon_diff < tol
    )
  
  # ---- summary ----
  if (verbose) {
    
    n_total <- nrow(result)
    n_match <- sum(result$coord_match, na.rm = TRUE)
    n_mismatch <- sum(!result$coord_match, na.rm = TRUE)
    
    message("Total rows: ", n_total)
    message("Matching coords: ", n_match)
    message("Mismatched coords: ", n_mismatch)
  }
  
  # ---- return only mismatches if requested ----
  if (return_only_mismatch) {
    result <- dplyr::filter(result, !coord_match | is.na(coord_match))
  }
  
  return(result)
}

# call QA check for coords
qa_coords <- qa_check_coords(
  locations_all,
  monitoring_sites,
  df_key = "Code",
  sites_key = "site.name",
  df_lat = "Latitude",
  df_lon = "Longitude",
  sites_lat = "latitude",
  sites_lon = "longitude",
  return_only_mismatch = TRUE
)




# Save outputs as CSV
# write.csv(tlu_Plant_FOA, "data/raw_data/vegetation_data/all_veg_data/tlu_Plant_all_2012_2025.csv", row.names = FALSE)
# write.csv(species_by_strata_all_updated, "data/raw_data/vegetation_data/all_veg_data/species_by_strata_all_2012_2025.csv", row.names = FALSE)
# write.csv(species_list_all_updated, "data/raw_data/vegetation_data/all_veg_data/species_list_all_2012_2025.csv", row.names = FALSE)
# write.csv(locations_all, "data/raw_data/vegetation_data/all_veg_data/locations_all_2012_2025.csv", row.names = FALSE)
# write.csv(visits_all, "data/raw_data/vegetation_data/all_veg_data/visits_all_2012_2025.csv", row.names = FALSE)
# write.csv(RAM_stressors_all, "data/raw_data/vegetation_data/all_veg_data/RAM_stressors_all_2012_2025.csv", row.names = FALSE)
# write.csv(vertical_complexity_all, "data/raw_data/vegetation_data/all_veg_data/vertical_complexity_all_2012_2025.csv", row.names = FALSE)
# write.csv(AA_char_all, "data/raw_data/vegetation_data/all_veg_data/AA_char_all_2012_2025.csv", row.names = FALSE)



