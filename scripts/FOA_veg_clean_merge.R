#### FOA GRME/GIME Vegetation Data Cleaning and Merging Script ####

## This script takes the old 2015-2023 FOA veg data and combines it with new FOA veg data (2024-new) for later processing and goes through a series of QA/QC checks and cleaning to ensure data consistency

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(dplyr)
library(lubridate)
library(sf)
library(stringr)
library(tibble)

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
  # Calculate quadrat frequency in cases it is missing
  mutate(
    quad_freq = if_else(
      is.na(quad_freq),
      rowSums(
        across(c(Quadrat_NE, Quadrat_SE, Quadrat_SW, Quadrat_NW)),
        na.rm = TRUE
      ) * 25,
      quad_freq
    )
  ) %>%
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

## TSN-latin name mismatch ## --------------------------------------------------

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

# view(tsn_latin_results$long)   # long-format mismatches
# view(tsn_latin_results$wide)   # wide-format QA table


## Identify species/taxa missing from tlu_Plant ## -----------------------------

## function for identifying species/taxa in veg datasets but not in new tlu_Plant dataset by TSN (potential species that need to be added to tlu_Plant)

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

# produce summary table of missing species
veg_not_in_tlu <- veg_tsn %>%
  anti_join(
    tlu_Plant_new %>% select(TSN) %>% distinct(),
    by = "TSN"
  )

## code to help search for the species rows in the datasets
# search_list <- filter(species_list_2015_2025, Latin_Name == "Rhamnus alnifolia")
# search_strata <- filter(species_by_strata_2015_2025, Latin_Name == "Rhamnus alnifolia")
# search_tlu <- filter(tlu_Plant_new, Latin_Name == "Solidago altissima")
# search_tlu <- filter(tlu_Plant_new, Genus == "Thalictrum")
# search_tlu_old <- filter(tlu_Plant, Latin_Name == "Solidago altissima")


## Add missing species/taxa ## -------------------------------------------------

## add missing valid species/taxa to tlu_Plant_new if new species/taxa are identified
## Sources for gathering plant data include: 
## - Maine Floristic Quality Assessment - COC: https://www.maine.gov/dacf/mnap/features/coc.htm
## - ITIS: https://www.itis.gov/
## - USDA PLANTS Database: https://plants.usda.gov/
## - Maine Rare Plant List: https://www.maine.gov/dacf/mnap/features/rare_plants/plantlist.htm
## species level data is pulled from the maine_coc dataset and manually added from the other sources. Genus level data is all added manually.

## species 
# pull species from maine_coc
species_to_pull <- c(
  "Carex lenticularis",
  "Rubus canadensis",
  "Solidago altissima"
  # add more as needed
)

# filter maine_coc to just those species
maine_subset <- maine_coc %>%
  filter(Scientific.Name %in% species_to_pull)

# convert Nativity -> Exotic (TRUE / FALSE)
maine_subset <- maine_subset %>%
  mutate(
    Exotic = case_when(
      str_to_lower(nativity) %in% c("non-native", "introduced", "exotic") ~ TRUE,
      str_to_lower(nativity) == "native" ~ FALSE,
      TRUE ~ NA
    )
  )

# convert physiognomy -> Boolean growth-form traits
maine_subset <- maine_subset %>%
  mutate(
    Tree        = physiognomy == "tree",
    Shrub       = physiognomy == "shrub",
    Vine        = physiognomy == "vine",
    Fern_Ally   = physiognomy == "fern",
    Graminoid  = physiognomy %in% c("grass", "sedge", "rush"),
    Herbaceous = physiognomy %in% c("forb", "grass", "sedge", "rush")
  )

# create table with manually added species info not included in maine_coc
manual_species_info <- tibble::tribble(
  ~Latin_Name,              ~Genus,     ~Order,        ~Species,        ~Subspecies, ~Rank_Name, ~Synonym,
  ~TSN,     ~TSN_Accepted, ~Accepted_Found, ~Invasive, ~Author,  ~Canopy_Exclusion, ~ACAD_ED, ~Protected_species, ~Aquatic, ~Moss_Lichen, 
  # ---- Carex lenticularis ----
  "Carex lenticularis", "Carex", "Poales", "lenticularis", NA, "Species", NA, 39665, 39665, NA, FALSE, "Michx.", NA, FALSE, FALSE, FALSE, FALSE,
  # ---- Rubus canadensis ----
  "Rubus canadensis", "Rubus", "Rosales", "canadensis", NA, "Species", NA, 504842, 504842, NA, FALSE, "L.", NA, FALSE, FALSE, FALSE, FALSE,
  # ---- Solidago altissima ----
  "Solidago altissima", "Solidago", "Asterales", "altissima", NA, "Species", NA, 36228, 36228, NA, FALSE, "L.",  NA, FALSE, FALSE, FALSE, FALSE
)

maine_subset <- maine_subset %>%
  left_join(
    manual_species_info,
    by = c("Scientific.Name" = "Latin_Name")
  )

# rename & reshape to match tlu_Plant_new
species_to_add <- maine_subset %>%
  transmute(
    Accepted_Latin_Name = Scientific.Name,
    Common              = common.name,
    Family              = Family,
    Genus               = Genus,
    Latin_Name          = Scientific.Name,
    Order               = Order,
    PLANTS_Code         = PLANTS_Accepted.Symbol,
    Rank_Name           = Rank_Name,
    Species             = Species,
    Subspecies          = Subspecies,
    Synonym             = Synonym,
    ACAD_ED             = ACAD_ED,
    Accepted_Found      = Accepted_Found,
    Aquatic             = Aquatic,
    Exotic              = Exotic,
    Fern_Ally           = Fern_Ally,
    Graminoid           = Graminoid,
    Herbaceous          = Herbaceous,
    Moss_Lichen         = Moss_Lichen,
    Shrub               = Shrub,
    Tree                = Tree,
    TSN                 = TSN,
    TSN_Accepted        = TSN_Accepted,
    Vine                = Vine,
    CoC_ME_ACAD         = ecoreg_82_COC,
    Created_Date        = as.character(Sys.Date()),
    Updated_Date        = as.character(Sys.Date()),
    Invasive            = Invasive,
    Author              = Author,
    Canopy_Exclusion    = Canopy_Exclusion,
    Protected_species   = Protected_species,
    Coef_wetness        = coefficient.of.wetness
  )

## genera 
# pull species of genera that need to be added to tlu_Plant from maine_coc
genus_species_to_pull <- c(
  "Nuphar lutea ssp. advena",
  "Nuphar lutea ssp. variegata",
  "Thalictrum polygamum",
  "Thalictrum pubescens"
  # add more as needed
)

# filter maine_coc to just those species
genus_maine_subset <- maine_coc %>%
  filter(Scientific.Name %in% genus_species_to_pull)

# calculate genus-level summaries
genus_summary <- genus_maine_subset %>%
  mutate(Genus = word(Scientific.Name, 1)) %>%
  group_by(Genus) %>%
  summarise(
    mean_COC = round(mean(ecoreg_82_COC, na.rm = TRUE)),
    n_species = n(),
    
    # keep wetness value only if all species share the same one
    coef_wetness = if(n_distinct(coefficient.of.wetness, na.rm = TRUE) == 1)
      first(coefficient.of.wetness)
    else
      NA
  )

# Create a table for any genus that needs to be manually added to tlu_Plant
manual_genus_info <- tibble::tribble(
  ~Accepted_Latin_Name, ~Common, ~Family,          ~Genus,         ~Latin_Name,   ~Order,
  ~PLANTS_Code, ~Rank_Name, ~Species, ~Subspecies, ~Synonym,
  ~ACAD_ED, ~Accepted_Found, ~Aquatic, ~Exotic,
  ~Fern_Ally, ~Graminoid, ~Herbaceous, ~Moss_Lichen, ~Shrub, ~Tree,
  ~TSN,     ~TSN_Accepted, ~Vine,
  ~CoC_ME_ACAD, ~Created_Date,            ~Updated_Date,
  ~Invasive, ~Author, ~Canopy_Exclusion, ~Protected_species, ~Coef_wetness,
  
  # ---- Nuphar (genus) ----
  "Nuphar", "pond-lily", "Nymphaeaceae", "Nuphar", "Nuphar", "Nymphaeales",
  "NUPHA", "Genus", NA, NA, NA,
  NA, NA, TRUE, NA,
  FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
  18371, 18371, FALSE,
  NA, as.character(Sys.Date()), as.character(Sys.Date()),
  NA, "Sm.", NA, FALSE, NA,
  
  # ---- Thalictrum (genus) ----
  "Thalictrum", "meadow-rue", "Ranunculaceae", "Thalictrum", "Thalictrum", "Ranunculales",
  "THALI2", "Genus", NA, NA, NA,
  NA, NA, NA, NA,
  FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
  18658, 18658, FALSE,
  NA, as.character(Sys.Date()), as.character(Sys.Date()),
  NA, "L.", NA, FALSE, NA
)

# fill in COC and COW values from maine.coc dataset
manual_genus_info_filled <- manual_genus_info %>%
  left_join(
    genus_summary %>%
      select(Genus, mean_COC, coef_wetness),
    by = "Genus"
  ) %>%
  mutate(
    CoC_ME_ACAD = coalesce(CoC_ME_ACAD, mean_COC),
    Coef_wetness = coalesce(Coef_wetness, coef_wetness)
  ) %>%
  select(-mean_COC, -coef_wetness)

# append to tlu_Plant_new
tlu_Plant_new_updated <- bind_rows(
  tlu_Plant_new,
  species_to_add,
  manual_genus_info_filled
)

## add new gathered data into veg datasets ##-----------------------------------

## add missing species information to species_lists and species_by_strata datasets
fill_from_tlu <- function(df, join_col_df, join_col_tlu) {
  
  df_joined <- df %>%
    left_join(
      tlu_Plant_new_updated,
      by = setNames(join_col_tlu, join_col_df),
      suffix = c("", ".tlu")
    )
  
  tlu_cols <- names(df_joined)[endsWith(names(df_joined), ".tlu")]
  
  for (tlu_col in tlu_cols) {
    
    base_col <- sub("\\.tlu$", "", tlu_col)
    
    if (base_col %in% names(df)) {
      
      df_joined[[base_col]] <- dplyr::coalesce(
        df_joined[[base_col]],
        df_joined[[tlu_col]]
      )
    }
  }
  
  df_joined %>%
    select(all_of(names(df)))
}

# fill-in species_lists
species_lists_filled <- fill_from_tlu(
  species_list_2015_2025,
  join_col_df  = "Latin_Name",
  join_col_tlu = "Latin_Name"
)

# fill-in species_by_strata
species_by_strata_filled <- fill_from_tlu(
  species_by_strata_2015_2025,
  join_col_df  = "Latin_Name",
  join_col_tlu = "Latin_Name"
)

# double check that the function worked correctly by isolating which rows were changed
check_filled <- anti_join(
  species_lists_filled,
  species_list_2015_2025,
  by = colnames(species_list_2015_2025)
)


## optional check - find rows where TSN is NA or missing (should now only be bryophytes)
species_list_missing_tsn <- species_lists_filled  %>%
  filter(is.na(TSN))

species_by_strata_missing_tsn <- species_by_strata_filled %>%
  filter(is.na(TSN))


## comprehensive check ## ------------------------------------------------------

## this function checks for any differences between FOA veg data and the new tlu_Plant dataset and fixes differences by overwriting FOA data with tlu_Plant data

sync_with_tlu <- function(data, tlu, key = "TSN", verbose = TRUE) {
  
  
  # Identify shared columns
  shared_cols <- intersect(names(data), names(tlu))
  compare_cols <- setdiff(shared_cols, key)
  
  if (length(compare_cols) == 0) {
    stop("No shared columns to compare.")
  }
  
  # Join for comparison
  comparison_df <- data %>%
    dplyr::inner_join(tlu,
                      by = key,
                      suffix = c("_data", "_tlu"))
  
  # Identify mismatches
  mismatch_matrix <- sapply(compare_cols, function(col) {
    
    data_col <- comparison_df[[paste0(col, "_data")]]
    tlu_col  <- comparison_df[[paste0(col, "_tlu")]]
    
    !(data_col == tlu_col |
        (is.na(data_col) & is.na(tlu_col)))
  })
  
  mismatches <- comparison_df[rowSums(mismatch_matrix) > 0, ]
  
  # Identify which columns differ
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
  
  # Update data
  updated_data <- data %>%
    dplyr::rows_update(
      tlu %>%
        dplyr::filter(.data[[key]] %in% mismatches[[key]]) %>%
        dplyr::select(all_of(c(key, compare_cols))),
      by = key
    )
  
  # produce written summary of changes
  if (verbose) {
    message("Rows updated: ", nrow(mismatches))
    message("Columns changed: ",
            ifelse(length(cols_that_differ) == 0,
                   "None",
                   paste(cols_that_differ, collapse = ", ")))
  }
  
  return(list(
    updated_data = updated_data,
    mismatches = mismatch_report,
    changed_columns = cols_that_differ
  ))
}

# update species_list
result_species <- sync_with_tlu(
  species_lists_filled,
  tlu_Plant_new_updated
)

  species_lists_filled_updated <- result_species$updated_data
  species_mismatches <- result_species$mismatches


# update species_by_strata
result_strata <- sync_with_tlu(
  species_by_strata_filled,
  tlu_Plant_new_updated
)

  species_by_strata_updated <- result_strata$updated_data
  strata_mismatches <- result_strata$mismatches

  

# Save outputs as CSV
# write.csv(tlu_Plant_new_updated, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/tlu_Plant.csv", row.names = FALSE)
# write.csv(species_by_strata_updated, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/species_by_strata_2015_2025.csv", row.names = FALSE)
# write.csv(species_lists_filled_updated, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/species_list_2015_2025.csv", row.names = FALSE)
# write.csv(locations_clean, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/locations.csv", row.names = FALSE)
# write.csv(visits_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/visits_2015_2025.csv", row.names = FALSE)
# write.csv(RAM_stressors_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/RAM_stressors_2015_2025.csv", row.names = FALSE)
# write.csv(vertical_complexity_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/vertical_complexity_2015_2025.csv", row.names = FALSE)
# write.csv(AA_char_2015_2025, "data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/AA_char_2015_2025.csv", row.names = FALSE)

