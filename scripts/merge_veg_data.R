## Merging Great Meadow Veg Data ##

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

# Reading in CSVs as a tibble

## FOA/Glen Veg data ##

VMMI_FOA <- read.csv("data/processed_data/vegetation_data/FOA_VMMI_2015_2025_20260310.csv") %>%
  as_tibble()

spplist_FOA <- read.csv("data/processed_data/vegetation_data/FOA_species_list_2015_2025_20260310.csv") %>%
  as_tibble()

## NETN/Kate Veg data ##

VMMI_NETN <- read.csv("data/processed_data/vegetation_data/NETN_vegMMI_allsites_2011-2025.csv") %>%
  as_tibble()

spplist_NETN <- read.csv("data/processed_data/vegetation_data/NETN_spplist_allsites_2011-2025_public.csv") %>% 
  as_tibble()

## tlu_Plant ##

tlu_Plant <- read.csv("data/raw_data/vegetation_data/FOA_veg_data/FOA_veg_data_2025/tlu_Plant.csv")

## sites data ##

monitoring_sites <- read.csv("data/processed_data/monitoring_sites.csv") %>% as_tibble()

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

# call QA check for coords
qa_vmmi <- qa_check_coords(VMMI_FOA_NETN, monitoring_sites)

  # fix any coords that mismatch
  # join and replace mismatched coordinates
  vmmi_corrected <- VMMI_FOA_NETN %>%
    left_join(monitoring_sites, by = "site.name", suffix = c(".data", ".fix")) %>%
    mutate(
      latitude = if_else(abs(latitude.data - latitude.fix) > 0.00001, latitude.fix, latitude.data),
      longitude = if_else(abs(longitude.data - longitude.fix) > 0.00001, longitude.fix, longitude.data)
    ) %>%
    select(-ends_with(".fix"), -latitude.data, -longitude.data) %>% 
    select(site.name,
           local.id,
           site.type = site.type.data,
           year,
           latitude,
           longitude,
           mean.coc,
           inv.cov,
           bryo.cov,
           strtol.cov,
           vmmi,
           vmmi.rating,
           wetland,
           source = source.data)



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

# call QA check for coords
qa_spplist <- qa_check_coords(clean_spplist_FOA_NETN, monitoring_sites)

  # fix any coords that mismatch
  # join and replace mismatched coordinates
  spplist_corrected <- clean_spplist_FOA_NETN %>%
    left_join(monitoring_sites, by = "site.name", suffix = c(".data", ".fix")) %>%
    mutate(
      latitude = if_else(abs(latitude.data - latitude.fix) > 0.00001, latitude.fix, latitude.data),
      longitude = if_else(abs(longitude.data - longitude.fix) > 0.00001, longitude.fix, longitude.data)
    ) %>%
    select(-ends_with(".fix"), -latitude.data, -longitude.data) %>% 
    select(site.name,
           local.id,
           site.type = site.type.data,
           year,
           latitude,
           longitude,
           tsn,
           plant.code,
           latin.name,
           common.name,
           quad.freq,
           invasive,
           protected,
           coc,
           coc.wetness,
           wetland,
           source = source.data)


#---------------------------#
####    QA/QC Checking   ####
#---------------------------#
  
## 1: Taxonomic agreement across sources ##----------------------------------
  
## function for identifying TSN-latin name mismatches (between new tlu_Plant dataset and spplist dataset) for taxonomic consistency across data sources
check_tsn_latin_mismatch <- function(reference_df,
                                       compare_dfs,
                                       tsn_ref,
                                       latin_ref,
                                       tsn_comp,
                                       latin_comp,
                                       reference_label = "tlu_Plant",
                                       compare_labels,
                                       drop_invalid_tsn = TRUE) {
    
    tsn_ref   <- rlang::enquo(tsn_ref)
    latin_ref <- rlang::enquo(latin_ref)
    tsn_comp  <- rlang::enquo(tsn_comp)
    latin_comp<- rlang::enquo(latin_comp)
    
    prep_ref <- function(df) {
      df %>%
        select(TSN = !!tsn_ref, Latin_Name = !!latin_ref) %>%
        distinct() %>%
        mutate(dataset = reference_label)
    }
    
    prep_comp <- function(df, label) {
      df %>%
        select(TSN = !!tsn_comp, Latin_Name = !!latin_comp) %>%
        distinct() %>%
        mutate(dataset = label)
    }
    
    ref <- prep_ref(reference_df)
    comps <- purrr::map2_dfr(compare_dfs, compare_labels, prep_comp)
    
    all_data <- bind_rows(ref, comps)
    
    if (drop_invalid_tsn) {
      all_data <- all_data %>%
        filter(!is.na(TSN) & TSN > 0)
    }
    
    mismatches_long <- all_data %>%
      group_by(TSN) %>%
      filter(n_distinct(Latin_Name) > 1) %>%
      ungroup()
    
    mismatches_wide <- mismatches_long %>%
      tidyr::pivot_wider(
        names_from = dataset,
        values_from = Latin_Name
      )
    
    list(
      long = mismatches_long,
      wide = mismatches_wide
    )
  }
  
## run functions
tsn_latin_results <- check_tsn_latin_mismatch(
  reference_df  = tlu_Plant,
  compare_dfs   = list(spplist_corrected),
  compare_labels = "spplist_corrected",

  tsn_ref   = TSN,
  latin_ref = Latin_Name,

  tsn_comp  = tsn,
  latin_comp = latin.name
)

# view(tsn_latin_results$long)   # long-format mismatches
# view(tsn_latin_results$wide)   # wide-format QA table


## 2: check for species missing from tlu_Plant ## ---------------------------

## function for checking if there are any species in veg dataset but not in tlu_Plant by TSN 
find_veg_not_in_tlu <- function(veg_df,
                                tlu_df,
                                veg_tsn_col,
                                veg_latin_col,
                                tlu_tsn_col) {
  
  veg_tsn_col   <- rlang::enquo(veg_tsn_col)
  veg_latin_col <- rlang::enquo(veg_latin_col)
  tlu_tsn_col   <- rlang::enquo(tlu_tsn_col)
  
  veg_df %>%
    select(
      TSN = !!veg_tsn_col,
      Latin_Name = !!veg_latin_col
    ) %>%
    distinct() %>%
    anti_join(
      tlu_df %>%
        select(TSN = !!tlu_tsn_col) %>%
        distinct(),
      by = "TSN"
    )
}

veg_not_in_tlu <- find_veg_not_in_tlu(
  veg_df        = spplist_corrected,
  tlu_df        = tlu_Plant,
  veg_tsn_col   = tsn,
  veg_latin_col = latin.name,
  tlu_tsn_col   = TSN
)

## check for data consistency with in dataset--------------------------------  
## find any inconsistencies in merged data, summarize them, and correct them using tlu_Plant as authoritative table

species_QA <- function(df, reference = NULL, fix = FALSE) {
  
  species_cols <- c(
    "latin.name",
    "common.name",
    "plant.code",
    "invasive",
    "protected",
    "coc",
    "coc.wetness"
  )
  
  # ---- Step 1: identify inconsistencies ----
  qa_results <- purrr::map_dfr(species_cols, function(col) {
    
    df %>%
      dplyr::group_by(tsn) %>%
      dplyr::filter(dplyr::n_distinct(.data[[col]], na.rm = TRUE) > 1) %>%
      dplyr::mutate(problem_column = col) %>%
      dplyr::ungroup()
    
  })
  
  # ---- Step 2: make corrections ----
  if (fix && !is.null(reference)) {
    
    ref_clean <- reference %>%
      dplyr::select(
        tsn = TSN,
        latin.name = Latin_Name,
        common.name = Common,
        plant.code = PLANTS_Code,
        invasive = Invasive,
        protected = Protected_species,
        coc = CoC_ME_ACAD,
        coc.wetness = Coef_wetness
      )
    
    corrected_df <- df %>%
      dplyr::select(-dplyr::all_of(species_cols)) %>%
      dplyr::left_join(ref_clean, by = "tsn")
    
    return(list(
      qa_table = qa_results,
      corrected_data = corrected_df
    ))
    
  } else {
    
    return(list(
      qa_table = qa_results,
      corrected_data = df
    ))
    
  }
}

results_species_QA <- species_QA(
  spplist_corrected,
  reference = tlu_Plant,
  fix = TRUE
)

# Step 1: identify inconsistencies
QA_results <- results_species_QA$qa_table
# Step 2: make corrections
clean_spplist_corrected <- results_species_QA$corrected_data


# Save outputs as CSV
# write.csv(spplist_corrected, "data/processed_data/FOA_NETN_species_list_2011_2025.csv", row.names = FALSE)
# write.csv(vmmi_corrected, "data/processed_data/FOA_NETN_VMMI_2011_2025.csv", row.names = FALSE)


  
## species list table for pop-up on arcgis map ---------------------------------

spplist_arcgis <- spplist_corrected %>%
  group_by(site.name, latin.name, common.name, invasive) %>%
  summarize(
    years.observed = paste(sort(unique(year)), collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    invasive = ifelse(invasive, "Yes", "No")
  )

# export for joining to spatial data
# write.csv(spplist_arcgis, "data/processed_data/species_list_arcgis.csv", row.names = FALSE)




#### GRAVEYARD ####-------------------------------------------------------------

# ## species list table for pop-up with lat and long
# spplist_arcgis_2 <- spplist_corrected %>%
#   group_by(site.name, latin.name, common.name, invasive) %>%
#   summarize(
#     years.observed = paste(sort(unique(year)), collapse = ", "),
#     site.type = first(site.type),
#     lat_vals  = n_distinct(latitude, na.rm = TRUE),
#     long_vals = n_distinct(longitude, na.rm = TRUE),
#     latitude  = first(latitude),
#     longitude = first(longitude),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     invasive = if_else(invasive, "Yes", "No"),
#     coord_warning = lat_vals > 1 | long_vals > 1) %>%
#   select(-lat_vals, -long_vals
#          ,-coord_warning)  # drop checks if you don't need them
# 
# # export for joining to spatial data
# # write.csv(spplist_arcgis_2, "data/processed_data/species_list_arcgis_latlong.csv", row.names = FALSE)
# 
# 
# #### merge vmmi data with monitoring sites data
# 
# # get most recent vmmi rating for each site
# vmmi_recent <- vmmi_corrected %>%
#   group_by(site.name) %>%
#   filter(year == max(year)) %>%
#   ungroup() %>%
#   select(site.name, vmmi.rating)
# 
# # join to sites dataset
# sites_vmmi <- monitoring_sites %>%
#   left_join(vmmi_recent, by = "site.name")
# 
# # Save outputs as CSV
# # write.csv(sites_vmmi, "data/processed_data/monitoring_sites_vmmi.csv", row.names = FALSE)




# # get summary table
# # --- 2. Run t-tests for selected variables ---
# vars_to_test <- c("vmmi", "mean.coc", "inv.cov", "bryo.cov", "strtol.cov")
# 
# t_results <- map_dfr(vars_to_test, function(v) {
#   f <- as.formula(paste(v, "~ wetland"))
#   t_res <- t.test(f, data = vmmi_corrected %>%
#                     filter(wetland %in% c("Great Meadow", "Gilmore Meadow")))
#   tibble(variable = v,
#          p_value = t_res$p.value)
# })
# 
# # --- 3. Merge results ---
# summary_with_p <- summary_vmmi_wetland %>%
#   pivot_longer(-wetland, names_to = "variable", values_to = "value") %>%
#   left_join(t_results, by = "variable")
# 
# # --- 4. Create table with highlighting ---
# highlighted_table <- summary_with_p %>%
#   gt(groupname_col = "wetland") %>%
#   data_color(
#     columns = vars(value),
#     rows = p_value < 0.05,
#     colors = scales::col_factor(c("yellow", "white"), domain = c(TRUE, FALSE))(TRUE)
#   ) %>%
#   fmt_number(columns = value, decimals = 2) %>%
#   tab_header(title = "Wetland Summary with Significant Differences Highlighted")
# 
# # --- 5. Save output ---
# gtsave(highlighted_table, "outputs/summary_table.pdf")



## build pop-up tables for arcgis maps -----------------------------------------

# build HTML table for pop-up per site
# VMMI_popup <- VMMI_FOA_NETN %>%
#   arrange(year) %>%
#   mutate(row = paste0(
#     "<tr><td>", year, "</td><td>", mean.coc, "</td><td>", inv.cov, "</td><td>",
#     bryo.cov, "</td><td>", strtol.cov, "</td><td>", vmmi, "</td><td>", vmmi.rating, "</td></tr>"
#   )) %>%
#   group_by(site.name) %>%
#   summarise(
#     vmmi_popup = paste0(
#       "<b>VMMI Summary</b><br><table border='1'><tr><th>Year</th><th>mean.coc</th><th>inv.cov</th><th>bryo.cov</th><th>strtol.cov</th><th>vmmi</th><th>rating</th></tr>",
#       paste(row, collapse = ""),
#       "</table>"
#     ),
#     .groups = "drop"
#   )

# export for joining to spatial data
# write.csv(VMMI_popup, "data/processed_data/vmmi_popup_per_site.csv", row.names = FALSE)


## species lists format for pop-up on arcgis map

# species_lists_popup <- clean_spplist_FOA_NETN %>%
#   group_by(site.name, latin.name, common.name, invasive) %>%
#   summarize(years_found = paste(sort(unique(year)), collapse = ", "), .groups = "drop") %>%
#   group_by(site.name) %>%
#   summarize(
#     species_popup = paste0(
#       "<table border='1'><tr><th>Latin Name</th><th>Common Name</th><th>Invasive?</th><th>Years Found</th></tr>",
#       paste(
#         "<tr><td>", latin.name, "</td><td>", common.name, "</td><td>", ifelse(invasive, "Yes", "No"), "</td><td>", years_found, "</td></tr>",
#         collapse = ""),
#       "</table>"
#     )
#   )

# export for joining to spatial data
# write.csv(species_lists_popup, "data/processed_data/species_list_popup_per_site.csv", row.names = FALSE)



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



