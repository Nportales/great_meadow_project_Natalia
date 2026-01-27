#### Cleaning Great Meadow and the Gilmore Meadow Well Data ####

#------------------------------------------------#
####                Packages                  ####
#------------------------------------------------#
library(tidyverse)
library(lubridate)


#################  GREAT MEADOW  #################

#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

## Read in the raw sheet from the data file

raw <- readxl::read_excel("data/raw_data/hydrology_data/GREAT MEADOW Full Data & New Graphs 2025 October.xlsx", sheet = "Full Data to Oct 2025")

# raw <- readxl::read_excel("data/raw_data/hydrology_data/GREAT MEADOW Full Data & New Graphs 2024 May.xlsx", sheet = "Full Data to May 2024")

## Pair down the clutter and fix timestamps

slim <- raw %>% 
  select(-c(1:2, 6:11, 17)) %>%
  rename(timestamp = `Date Time, GMT-04:00`, abs.pres = `Abs Pres, kPa (from AirLogger in Great Meadow)`, temp = `Temp, °F (from AirLogger in Great Meadow)`, year = `Year {=TEXT(r37490,"yyyy")}`,
         precip.cm = `Precip.cm`, daily.precip = `Precip Daily  (Calculated in pivot and vlookup back in on date & 23:00 time)`,
         data.complete = DataComplete) %>%
  mutate(timestamp = coalesce(timestamp, `Plot3 Date Time`),
         date = as.Date(str_extract(timestamp, "^\\d*\\-\\d*\\-\\d\\d"))) %>%
  select(1, 50, everything()) %>%
  filter(!is.na(timestamp)) # ensure that you aren't removing data here! Just meant to clean up extra rows


# slim <- raw %>% 
#   select(-c(1:3, 7:12, 18)) %>%
#   rename(timestamp = `Date Time, GMT-04:00`, abs.pres = `Abs Pres, kPa (from AirLogger in Great Meadow)`, temp = `Temp, °F (from AirLogger in Great Meadow)`, year = `Year {=TEXT(r37490,"yyyy")}`,
#          precip.cm = `Precip.cm`, daily.precip = `Precip Daily  (Calculated in pivot and vlookup back in on date & 23:00 time)`,
#          data.complete = DataComplete) %>%
#   mutate(timestamp = coalesce(timestamp, `Plot3 Date Time`),
#          date = as.Date(str_extract(timestamp, "^\\d*\\-\\d*\\-\\d\\d"))) %>%
#   select(1, 50, everything()) %>%
#   filter(!is.na(timestamp)) # ensure that you aren't removing data here! Just meant to clean up extra rows
# 

#-------------------------------------------------#
####  Cleaning and converting to long format   ####
#-------------------------------------------------#

### First create separate data frames to row bind later
## Plot 1
plot1 <- slim %>% 
  select(1:8, 10:15) %>% 
  mutate(plot.num = 1) %>% 
  rename(logger.pressure = `Plot1 Logger Pressure`, logger.temp = `Plot1 logger temp`, 
         baro.pressure = `Plot1 BaroPress`, cor.logger.depth = `Plot1 Original Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot1 FINAL Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....23`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


## Plot 2
plot2 <- slim %>% 
  select(1:8, 17:22) %>% 
  mutate(plot.num = 2) %>% 
  rename(logger.pressure = `Plot2 Logger Pressure`, logger.temp = `Plot2 logger temp`, 
         baro.pressure = `Plot2 BaroPress`, cor.logger.depth = `Plot2 Original Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot2 Filtered Corrected & Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....30`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


## Plot 3
plot3 <- slim %>% 
  select(1:8, 24:29) %>% 
  mutate(plot.num = 3) %>% 
  rename(logger.pressure = `Plot3 Logger Pressure`, logger.temp = `Plot3 logger temp`, 
         baro.pressure = `Plot3 BaroPress`, cor.logger.depth = `Plot3 Original Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot3 Filtered Corrected & Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....37`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


## Plot 4
plot4 <- slim %>% 
  select(1:8, 31:36) %>% 
  mutate(plot.num = 4) %>% 
  rename(logger.pressure = `Plot4 Logger Pressure`, logger.temp = `Plot4 logger temp`, 
         baro.pressure = `Plot4 BaroPress`, cor.logger.depth = `Plot4 Original Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot4 Filtered Corrected & Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....44`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


## Plot 5
plot5 <- slim %>% 
  select(1:8, 38:43) %>% 
  mutate(plot.num = 5) %>% 
  rename(logger.pressure = `Plot5 Logger Pressure`, logger.temp = `Plot5 logger temp`, 
         baro.pressure = `Plot5 BaroPress`, cor.logger.depth = `Plot5 Original Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot5 Filtered Corrected & Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....51`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


## Plot 6
plot6 <- slim %>% 
  select(1:8, 45:50) %>% 
  mutate(plot.num = 6) %>% 
  rename(logger.pressure = `Plot6 Logger Pressure`, logger.temp = `Plot6 logger temp`, 
         baro.pressure = `Plot6 BaroPress`, cor.logger.depth = `Plot6 Corrected Logger Depth`,
         filt.cor.logger.depth = `Plot6 Filtered Corrected Logger Depth`,
         error.correct = `Removed data for erroneous points....58`) %>% 
  select(1:8, 15, everything()) %>% 
  mutate(error.correct = as.character(error.correct))


### Combine all the plot data by row binding for long format
combined <- bind_rows(plot1, plot2, plot3, plot4, plot5, plot6)



#------------------------------------------------#
####     Fixing the daily precip values       ####
#------------------------------------------------#

## Create the correct sum of each day's precip
day.precip <- slim %>% 
  select(date, precip.cm) %>% 
  group_by(date) %>% 
  summarise(precip.day.total = sum(precip.cm, na.rm = T))

## Join this to our combined data
clean <- combined %>% 
  left_join(., day.precip, by = "date") %>% 
  mutate(water.depth = cor.logger.depth*100,
         doy = yday(date),
         hr = hour(timestamp),
         hr = as.character(hr),
         hr = ifelse(nchar(hr) < 2, paste0("0", hr), hr),
         doy_h = paste0(doy, ".", hr),
         lag.precip = lag(precip.cm),
         lag.precip = ifelse(is.na(lag.precip), 0, lag.precip)) %>% 
  select(1:6, 21, 16, 17, 8:15, 18:20)


## Write out the clean data
# write.csv(clean, "data/processed_data/hydrology_data/gm_well_data_2025_20260127.csv", row.names = F)



################  GILMORE MEADOW  ################

#------------------------------------------------#
####             Read and Clean               ####
#------------------------------------------------#

gilm <- tibble(read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2025.csv")) %>% 
  select(1,2,3,4,5,8,14,15,16) %>% 
  rename_with(tolower) %>% 
  mutate(timestamp = parse_date_time(timestamp, orders = c("mdy HM", "mdy", "ymd HMS", "ymd")),
         date = as.Date(parse_date_time(date, orders = c("ymd", "mdy", "dmy")))
  ) %>% 
  select(1,2,4,3,8,5,7,6) %>% 
  rename(precip.cm = precip_cm, water.depth = gilm_wl)


## Write out the clean data
write.csv(gilm, "data/processed_data/hydrology_data/gl_well_data_2025_20260127.csv", row.names = F)






## GRAVEYARD ##

## old gilmore meadow wl data processing
# gilm <- tibble(read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2025.csv")) %>% 
#   select(1,2,3,4,5,8,14,15,16) %>% 
#   rename_with(tolower) %>% 
#   mutate(timestamp = parse_date_time(timestamp, orders = c("mdy HM", "mdy")),
#          date = mdy(date)) %>% 
#   select(1,2,4,3,8,5,7,6) %>% 
#   rename(precip.cm = precip_cm, water.depth = gilm_wl)

