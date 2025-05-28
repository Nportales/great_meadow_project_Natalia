#------------------------------------------------------------------------------
# Compile ACAD Great Meadow and Gilmore Meadow wetland data collected in 2024
#------------------------------------------------------------------------------

# Uncomment code below to install wetlandACAD R package from github. Must also have
# devtools package installed.
devtools::install_github("katemmiller/wetlandACAD")

library(wetlandACAD)
library(tidyverse)

# Set path for importing and exporting results
#path = "L:/0409 Vegetation/Veg Observations Inventory & Monitoring/Veg Inventories & Monitoring/Wetland Monitoring/2024_Great_and_Gilmore_Meadow_Data/Data_DRAFT/"
path = "data/raw_data/Kate_NETN_veg_data/"

# Note: Must have MS Access installed on computer, along with ODBC driver for MS Access. If you don't have MS Access, skip to line 33.
# Import ACAD data from Great Meadow and Gilmore Meadow from Access database. Scrubbing protected species from export.
importRAM(type = "dbfile", filepath = paste0(path, "ACAD_RAM_20241216.accdb"),
          export_protected = F, export_data = T, zip = T, export_path = path)

# Import ACAD data from Great Meadow and Gilmore Meadow from Access database. Exporting protected species, and
# using full dataset for vegMMI and species list.
importRAM(type = "dbfile", filepath = paste0(path, "ACAD_RAM_20241216.accdb"),
          export_protected = T, export_data = T, zip = T, export_path = path)

# Renaming default exported file to be ACAD instead of NETN.
file.rename(paste0(path, "NETN_Wetland_RAM_Data_20250523_NPSonly.zip"),
            paste0(path, "ACAD_Wetland_RAM_Data_20250523_NPSonly.zip"))

file.rename(paste0(path, "NETN_Wetland_RAM_Data_20250523_public.zip"),
            paste0(path, "ACAD_Wetland_RAM_Data_20250523_public.zip"))

# If you're not able to run the above code due to issues with ODBC drivers for accdb files, you can just import the zip files.
importRAM(type = "zip", filepath = paste0(path, "ACAD_Wetland_RAM_Data_20250523_NPSonly.zip"))

# Calculate vegetation MMI
vegmmi <- sumVegMMI(panel = -1) |> # used -1 as panel for non-NETN sites
  mutate(Site = ifelse(grepl("GIME", Code), "Gilmore Meadow", "Great Meadow"))

# Generate site-level species list.
spplist <- sumSpeciesList(panel = -1) |> # used -1 as panel for non-NETN sites
  mutate(Site = ifelse(grepl("GIME", Code), "Gilmore Meadow", "Great Meadow"))

# Write df to file
write.csv(vegmmi, paste0(path, "ACAD_Wetland_VegMMI_20241216.csv"))
write.csv(spplist, paste0(path, "ACAD_Wetland_Species_List_20241216.csv"))

table(spplist$Protected_species) # no protected species found, so species list can be public.

# Convert photopoints from iPad from HEIC to jpg
# Only needed to run through this code once, and completed done for 2024 photopoints.
library(magick)
str(magick_config())

path_out <- paste0(path, "photopoints/")
path_HEIC = paste0(path_out, "HEIC/")

img_list <- list.files(path_HEIC, pattern = "HEIC")
img_num <- gsub("IMG_|.HEIC", "", img_list)

conv_to_jpg <- function(path_HEIC, path_out, img, x){
  img1 <- image_read(paste0(path_HEIC, "/", img))
  img2 <- image_convert(img1, format = "jpeg")
  image_write(img2, path = paste0(path_out, "/", "ACAD_RAM_", img_num[x], ".jpg"), format = "jpeg", quality = 100)
}

lapply(seq_along(img_list), function(x) conv_to_jpg(path_HEIC, path_out, img_list[x], x))

# Named photopoints by hand b/c several missing photos and out of order IDs



