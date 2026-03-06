## investigating tlu datasets ##

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

tlu_Plant <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/old_raw_veg_data/tlu_Plant.csv") %>%
  as_tibble()

## 2024-new FOA/Glen veg data ##

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


## NETN veg data tlu ##

tlu_Plant_NETN <- read.csv("data/raw_data/vegetation_data/NETN_veg_data/NETN_Wetland_RAM_Data_20260129_public/tlu_Plant.csv")


## site data ##

sites <- read.csv("data/raw_data/monitoring_sites_GRME_GIME_metadata.csv")


#-----------------------#
####    Data Manip   ####
#-----------------------#

##latin name check
tlu_latin_mismatch <- anti_join(
  tlu_Plant_new %>% distinct(Latin_Name),
  tlu_Plant %>% distinct(Latin_Name),
  by = "Latin_Name"
)

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

# build a table to view latin name mismatches ----------------------------------
tlu_latin_mismatch <- bind_rows(
  tlu_Plant_new %>%
    distinct(Latin_Name) %>%
    mutate(source_dataset = "tlu_Plant_new"),
  
  tlu_Plant %>%
    distinct(Latin_Name) %>%
    mutate(source_dataset = "tlu_Plant")
) %>%
  group_by(Latin_Name) %>%
  filter(n() == 1) %>%     # only present in one dataset
  ungroup()

new_only_rows <- tlu_Plant_new %>%
  anti_join(tlu_Plant %>% distinct(Latin_Name),
            by = "Latin_Name") %>%
  mutate(source_dataset = "tlu_Plant_new")

old_only_rows <- tlu_Plant %>%
  anti_join(tlu_Plant_new %>% distinct(Latin_Name),
            by = "Latin_Name") %>%
  mutate(source_dataset = "tlu_Plant")

latin_mismatch_rows <- bind_rows(new_only_rows, old_only_rows)


#search tlu for latin names
search_tlu <- filter(tlu_Plant_new, Latin_Name == "Bidens")


# build a table to view tsn mismatches -----------------------------------------
mismatched_tsn <- bind_rows(
  tlu_Plant_new %>% distinct(TSN) %>% mutate(source = "new"),
  tlu_Plant %>% distinct(TSN) %>% mutate(source = "old")
) %>%
  group_by(TSN) %>%
  filter(n() == 1) %>%
  ungroup()

TSN_mismatch_rows <- bind_rows(
  tlu_Plant_new %>%
    semi_join(mismatched_tsn, by = "TSN") %>%
    mutate(source_dataset = "tlu_Plant_new"),
  
  tlu_Plant %>%
    semi_join(mismatched_tsn, by = "TSN") %>%
    mutate(source_dataset = "tlu_Plant")
)

## tsn mismatches function -----------------------------------------------------

find_tsn_mismatches <- function(df_new,
                                df_old,
                                tsn_col = TSN,
                                new_name = "new",
                                old_name = "old",
                                drop_invalid = TRUE) {
  
  tsn_col <- enquo(tsn_col)
  
  # # Optional TSN cleaning
  # clean <- function(df) {
  #   if (drop_invalid) {
  #     df %>%
  #       filter(!is.na(!!tsn_col) & !!tsn_col > 0)
  #   } else {
  #     df
  #   }
  # }
  # 
  # df_new <- clean(df_new)
  # df_old <- clean(df_old)
  
  # Identify TSNs that occur in only one dataset
  mismatched_tsn <- bind_rows(
    df_new %>% distinct(!!tsn_col) %>% mutate(source = new_name),
    df_old %>% distinct(!!tsn_col) %>% mutate(source = old_name)
  ) %>%
    group_by(!!tsn_col) %>%
    filter(n() == 1) %>%
    ungroup()
  
  # Pull full rows for those TSNs
  bind_rows(
    df_new %>%
      semi_join(mismatched_tsn, by = rlang::as_name(tsn_col)) %>%
      mutate(source_dataset = new_name),
    
    df_old %>%
      semi_join(mismatched_tsn, by = rlang::as_name(tsn_col)) %>%
      mutate(source_dataset = old_name)
  )
}

FOA_TSN_mismatch_rows <- find_tsn_mismatches(
  df_new = tlu_Plant_new,
  df_old = tlu_Plant,
  tsn_col = TSN,
  new_name = "tlu_Plant_new",
  old_name = "tlu_Plant_NETN"
)

NETN_TSN_mismatch_rows <- find_tsn_mismatches(
  df_new = tlu_Plant_new,
  df_old = tlu_Plant,
  tsn_col = TSN,
  new_name = "tlu_Plant_new",
  old_name = "tlu_Plant_NETN"
)


# what tsns are only in one dataset and not the other
tsn_origin <- bind_rows(
  tlu_Plant_new %>%
    distinct(TSN) %>%
    mutate(origin_dataset = "tlu_Plant_new"),
  
  tlu_Plant_NETN %>%
    distinct(TSN) %>%
    mutate(origin_dataset = "tlu_Plant_NETN")
) %>%
  group_by(TSN) %>%
  filter(n() == 1) %>%   # TSN appears in only one dataset
  ungroup()

tsn_mismatch_rows <- bind_rows(
  tlu_Plant_new %>%
    semi_join(tsn_origin, by = "TSN") %>%
    mutate(origin_dataset = "tlu_Plant_new"),
  
  tlu_Plant_NETN %>%
    semi_join(tsn_origin, by = "TSN") %>%
    mutate(origin_dataset = "tlu_Plant_NETN")
)


## tsn function ##--------------------------------------------------------------
find_tsn_origin_rows <- function(df_a,
                                 df_b,
                                 tsn_col = TSN,
                                 label_a,
                                 label_b,
                                 drop_invalid = TRUE) {
  
  tsn_col <- rlang::enquo(tsn_col)
  
  # Optional TSN cleaning
  if (drop_invalid) {
    df_a <- df_a %>% filter(!is.na(!!tsn_col) & !!tsn_col > 0)
    df_b <- df_b %>% filter(!is.na(!!tsn_col) & !!tsn_col > 0)
  }
  
  # Step 1: Identify TSNs that occur in only one dataset
  tsn_origin <- bind_rows(
    df_a %>%
      distinct(!!tsn_col) %>%
      mutate(origin_dataset = label_a),
    
    df_b %>%
      distinct(!!tsn_col) %>%
      mutate(origin_dataset = label_b)
  ) %>%
    group_by(!!tsn_col) %>%
    filter(n() == 1) %>%
    ungroup()
  
  # Step 2: Pull full rows associated with those TSNs
  tsn_rows <- bind_rows(
    df_a %>%
      semi_join(tsn_origin, by = rlang::as_name(tsn_col)) %>%
      mutate(origin_dataset = label_a),
    
    df_b %>%
      semi_join(tsn_origin, by = rlang::as_name(tsn_col)) %>%
      mutate(origin_dataset = label_b)
  )
  
  list(
    tsn_origin = tsn_origin,
    rows = tsn_rows
  )
}

tsn_results <- find_tsn_origin_rows(
  df_a = tlu_Plant_new,
  df_b = tlu_Plant_NETN,
  tsn_col = TSN,
  label_a = "tlu_Plant_new",
  label_b = "tlu_Plant_NETN"
)

tsn_results_FOA <- find_tsn_origin_rows(
  df_a = tlu_Plant_new,
  df_b = tlu_Plant,
  tsn_col = TSN,
  label_a = "tlu_Plant_new",
  label_b = "tlu_Plant"
)

tsn_results_old_FOA_NETN <- find_tsn_origin_rows(
  df_a = tlu_Plant,
  df_b = tlu_Plant_NETN,
  tsn_col = TSN,
  label_a = "tlu_Plant",
  label_b = "tlu_Plant_NETN"
)

tsn_results$tsn_origin   # TSN → which dataset it comes from
tsn_results$rows         # full rows for those TSNs

tsn_results_FOA$tsn_origin   
tsn_results_FOA$rows

tsn_results_old_FOA_NETN$tsn_origin   
tsn_results_old_FOA_NETN$rows

#search tlu for tsn
search_tlu_new <- filter(tlu_Plant_new, TSN == "29062")
search_tlu_NETN <- filter(tlu_Plant_NETN, TSN == "29062")
search_tlu <- filter(tlu_Plant, TSN == "29062")









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
search_tlu <- filter(tlu_Plant_new, Genus == "Nuphar")
search_tlu_tsn <- filter(tlu_Plant_new, TSN_Accepted == "39665")
search_tlu <- tlu_Plant_new %>%
  filter(str_detect(Latin_Name, "Nuphar"))
search_data <- filter(species_list_2015_2025, Latin_Name == "Viburnum dilatatum")

#search tlu for mismatch latin names
search_list <- filter(species_list_2015_2025, Latin_Name == "Rhamnus alnifolia")
search_list_2 <- filter(species_list_2015_2025, Latin_Name == "Rhamnus frangula")
search_strata <- filter(species_by_strata_2015_2025, Latin_Name == "Solidago altissima")


# TSN is NA or missing
find_bad_tsn <- function(data, tsn_col, latin_col, year_col, dataset_name) {
  
  data %>%
    mutate(TSN_check = {{ tsn_col }}) %>%
    filter(
      is.na(TSN_check) |
        startsWith(as.character(TSN_check), "-9999")
    ) %>%
    transmute(
      dataset = dataset_name,
      Year = {{ year_col }},
      TSN = TSN_check,
      Latin_Name = {{ latin_col }}
    )
}

bad_tsn_strata <- find_bad_tsn(
  species_by_strata_2015_2025,
  TSN,
  Latin_Name,
  Year,
  "species_by_strata_2015_2025"
)

bad_tsn_list <- find_bad_tsn(
  species_list_2015_2025,
  TSN,
  Latin_Name,
  Year,
  "species_list_2015_2025"
)

bad_tsn_summary <- bind_rows(
  bad_tsn_strata,
  bad_tsn_list
)


## investigate 

