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
search_tlu <- filter(tlu_Plant_new, Latin_Name == "Solidago canadensis")
search_tlu_tsn <- filter(tlu_Plant_new, TSN_Accepted == "39665")
search_tlu <- tlu_Plant_new %>%
  filter(str_detect(Latin_Name, "Viburnum"))
search_data <- filter(species_list_2015_2025, Latin_Name == "Viburnum dilatatum")

#search tlu for mismatch latin names
search_list <- filter(species_list_2015_2025, Accepted_Latin_Name == "Utricularia vulgaris")


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




