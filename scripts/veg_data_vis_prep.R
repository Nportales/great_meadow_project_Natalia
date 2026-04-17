## Clean and prep veg data for visualization ##

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

## FOA/NETN veg data ##

vmmi <- read.csv("data/processed_data/vegetation_data/FOA_NETN_VMMI_2011_2025_20260324.csv") %>%
  as_tibble()

spplist <- read.csv("data/processed_data/vegetation_data/FOA_NETN_spplist_2011_2025_20260324.csv") %>%
  as_tibble()

monitoring_sites <- read.csv("data/processed_data/monitoring_sites.csv") %>%
  as_tibble()

#-----------------------#
####    Data Manip   #### 
#-----------------------#

## prep VMMI data ## -----------------------------------------------------------

new_vmmi <- vmmi%>% 
  
  mutate(
    
    # fix code names
    Code = 
      case_when(
        str_detect(Code, "304") ~ "NWCA-R304",
        TRUE ~ Code),
    
    # add a site type column
    site.type =
      case_when(
        grepl("GRME0[1-6]", Code) ~ "Intensive",
        grepl("GRME0[7-9]|GRME10", Code) ~ "RAM",
        grepl("GIME|R-04|R-13|R-19|R-31", Code) ~ "RAM",
        grepl("NWCA-R304", Code) ~ "SEN",
        TRUE ~ NA_character_
      ),
    
    # add wetland column
    wetland =
      case_when(
        grepl("GRME|R-04|R-13|R-19", Code) ~ "Great Meadow",
        grepl("GIME|R-31|NWCA-R304", Code) ~ "Gilmore Meadow",
        TRUE ~ NA_character_
      ),
    
    # add a source column
    source = case_when(
      grepl("GRME0[1-9]|GRME10|GIME", Code) ~ "FOA",
      grepl("R-04|R-13|R-19|R-31|NWCA-R304", Code) ~ "NETN",
      TRUE ~ NA_character_
    )
    
  ) %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = Location_ID,
         site.type,
         year = Year,
         xcoord = xCoordinate,
         ycoord = yCoordinate,
         utm_zone = UTM_Zone,
         latitude = Latitude,
         longitude = Longitude,
         mean.coc = meanC,
         inv.cov = Invasive_Cover,
         bryo.cov = Bryophyte_Cover,
         strtol.cov = Cover_Tolerant,
         vmmi,
         vmmi.rating = vmmi_rating,
         vmmi.rating.orig = vmmi_rating_orig,
         wetland,
         source)

## prep spplist data ## --------------------------------------------------------

new_spplist <- spplist %>% 
  
  mutate(
    
    # fix code names
    Code = 
      case_when(
        str_detect(Code, "304") ~ "NWCA-R304",
        TRUE ~ Code),
    
    # add a site type column
    site.type =
      case_when(
        grepl("GRME0[1-6]", Code) ~ "Intensive",
        grepl("GRME0[7-9]|GRME10", Code) ~ "RAM",
        grepl("GIME|R-04|R-13|R-19|R-31", Code) ~ "RAM",
        grepl("NWCA-R304", Code) ~ "SEN",
        TRUE ~ NA_character_
      ),
    
    # add wetland column
    wetland =
      case_when(
        grepl("GRME|R-04|R-13|R-19", Code) ~ "Great Meadow",
        grepl("GIME|R-31|NWCA-R304", Code) ~ "Gilmore Meadow",
        TRUE ~ NA_character_
      ),
    
    # add a source column
    source = case_when(
      grepl("GRME0[1-9]|GRME10|GIME", Code) ~ "FOA",
      grepl("R-04|R-13|R-19|R-31|NWCA-R304", Code) ~ "NETN",
      TRUE ~ NA_character_
    )
    
  ) %>% 
  
  # Remove unneeded columns and rename to standardize 
  select(site.name = Code,
         local.id = Location_ID,
         site.type,
         year = Year,
         xcoord = xCoordinate,
         ycoord = yCoordinate,
         utm_zone = UTM_Zone,
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

# ##  QA check coordinates consistency ## --------------------------------------------
# check_coords_consistency <- function(df,
#                                      site_col = "site.name",
#                                      x_col = "xcoord",
#                                      y_col = "ycoord") {
#   
#   df %>%
#     group_by(.data[[site_col]]) %>%
#     filter(
#       dplyr::n_distinct(.data[[x_col]]) > 1 |
#         dplyr::n_distinct(.data[[y_col]]) > 1
#     ) %>%
#     ungroup()
# }
# 
# check_coords_consistency(new_vmmi)
# 
# check_coords_consistency(new_spplist)


# Save outputs as CSV
# write.csv(new_vmmi, "data/processed_data/vegetation_data/vis_FOA_NETN_VMMI_2011_2025_20260324.csv", row.names = FALSE)
# write.csv(new_spplist, "data/processed_data/vegetation_data/vis_FOA_NETN_spplist_2011_2025_20260324.csv", row.names = FALSE)


## VMMI dataset for arcgis map ## ----------------------------------------------

# summarise across all years by site
vmmi_summary <- new_vmmi %>%
  group_by(site.name) %>%
  summarise(
    
    # keep identifying info
    local.id = first(local.id),
    site.type = first(site.type),
    wetland = first(wetland),
    source = first(source),
    
    # coordinates
    xcoord = mean(xcoord, na.rm = TRUE),
    ycoord = mean(ycoord, na.rm = TRUE),
    utm_zone = first(utm_zone),
    latitude = mean(latitude, na.rm = TRUE),
    longitude = mean(longitude, na.rm = TRUE),
    
    # year range instead of single year
    year = paste0(min(year, na.rm = TRUE), "–", max(year, na.rm = TRUE)),
    
    # vmmi metrics (mean across years)
    mean.coc = round(mean(mean.coc, na.rm = TRUE), 2),
    inv.cov = round(mean(inv.cov, na.rm = TRUE), 2),
    bryo.cov = round(mean(bryo.cov, na.rm = TRUE), 2),
    strtol.cov = round(mean(strtol.cov, na.rm = TRUE), 2),
    vmmi = round(mean(vmmi, na.rm = TRUE), 2),
    
    # assign rating based on averaged VMMI 
    vmmi.rating = case_when(
      vmmi > 60.94853 ~ "Good",
      vmmi < 41.48136 ~ "Poor",
      TRUE ~ "Fair"
    ),
    
    # assign rating based on averaged VMMI (original rating thresholds)
    vmmi.rating.orig = case_when(
      vmmi > 65.22746 ~ "Good",
      vmmi < 52.785 ~ "Poor",
      TRUE ~ "Fair"
    ),
    
    .groups = "drop"
  ) %>% 
  
  left_join(
    monitoring_sites %>%
      select(site.name, display.site.name),
    by = "site.name"
  )

# view result
print(vmmi_summary)

# write to CSV
# write.csv(vmmi_summary, "data/processed_data/vegetation_data/vis_arcgis_VMMI_2011_2025_20260417.csv", row.names = FALSE)

