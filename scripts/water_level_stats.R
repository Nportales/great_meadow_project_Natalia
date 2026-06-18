#### Calculating Water Level Statistics #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(tidyverse)
library(lubridate)

# devtools::install_github("KateMMiller/wetlandACAD")

#-------------------------------------------#
####         Read & Prepare Data         ####
#-------------------------------------------#

## Read in the data and format for running through Kate's function

## Great Meadow 
gmwell <- tibble(read.csv("data/processed_data/hydrology_data/gm_well_data_2025_20260304.csv")) %>% 
  rename(Date = date, Year = year, precip_cm = precip.cm) %>% 
  select(timestamp, Date, doy, Year, precip_cm, water.depth, lag.precip, hr, doy_h, plot.num) %>% 
  mutate(timestamp = as_datetime(timestamp),
         water.depth = ifelse(Year == 2016 & doy_h == 159.12 & plot.num == 3 & 
                                water.depth < -120, NA, water.depth),
         water.depth = ifelse(Year == 2017 & doy_h == 215.02 & plot.num == 6 & 
                                water.depth < -115, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 224 &
                                water.depth > 400, NA, water.depth),
         water.depth = ifelse(Year == 2021 & plot.num == 3 & doy == 225 &
                                water.depth > 400, NA, water.depth))


## Gilmore Meadow
gilm <- tibble(read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2025.csv")) %>% 
  rename(gilmore.meadow = GILM_WL)


#------------------------------------------------#
####                Functions                 ####
#------------------------------------------------#

## Reading in Kate Millers functions manually because they weren't coming through
calc_WL_stats <- function(df, from = 2013, to = 2025) {
  
  EDT<-"America/New_York"
  well_prp <- df %>% mutate(timestamp = as.POSIXct(timestamp, 
                                                   orders = c("mdy HM", "mdy HMS", "ymd HM", 
                                                              "ymd HMS", "dmy HM", "dmy HMS")),
                            month = lubridate::month(timestamp),
                            mon = months(timestamp, abbreviate = T)) %>%
    filter(doy > 134 & doy < 275) %>% droplevels()
  
  well_prp2 <- well_prp %>% group_by(Year) %>%
    mutate(lag.precip = lag(precip_cm, 1)) %>%
    ungroup()
  
  well_prp_yr <- well_prp2 %>% filter(between(Year, from, to)) %>% droplevels()
  
  # May 1 DOY= 121; May 15 = 135; Oct.1 = 274
  well_prp_long <- well_prp_yr %>% gather("site","water_level_cm",
                                          -timestamp, -Date, -doy, -Year, -hr,
                                          -doy_h, -month, -mon, -precip_cm, -lag.precip)
  
  well_prp_long2 <- well_prp_long %>% group_by(Year, site) %>%
    mutate(lag_WL = lag(water_level_cm),
           change_WL = water_level_cm-lag_WL)
  
  # Calculate growing season stats
  well_gs_stats <- well_prp_long2 %>% group_by(Year, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
              WL_sd = sd(water_level_cm, na.rm = TRUE),
              WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
              WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
              max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
              max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
              prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100)
  
  # Calculate change in WL from average Jun to average September
  well_gs_month <- well_prp_long2 %>% group_by(Year, mon, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE)) %>%
    filter(mon %in% c("Jun","Sep")) %>% droplevels() %>% spread(mon, WL_mean) %>%
    mutate(GS_change = Sep - Jun)
  
  
  well_gs_prop1 <- well_prp_long2 %>% mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                             bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                                    !is.na(water_level_cm), 1, 0),
                                             under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                             num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))
  
  well_gs_prop <- well_gs_prop1 %>% group_by(Year, site) %>%
    summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100)
  
  gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) %>%
    reduce(left_join, by = c("Year", "site"))
  
  # Missing water level data from 2017, change to NA
  metrics<-c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )
  
  gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics]<-NA
  # Logger failed in DUCK in 2017
  
  prop_complete_check <- length(gs_WL_stats$prop_GS_comp[gs_WL_stats$prop_GS_comp < 90])
  
  if(prop_complete_check > 0) {
    message(paste0("Warning: There are ", prop_complete_check, " sites that have water level measurements for less than 90% of growing season."))
  }
  
  return(gs_WL_stats)
}


## Create function to loop that calculates water level stats for each of the six plots
wl_stats_loop <- function(plot.number) {
  
  output <- gmwell %>% 
    filter(plot.num == plot.number) %>% 
    select(-plot.num) %>% 
    calc_WL_stats(., from = 2016, to = 2025) %>% 
    mutate(site = paste0("great.meadow.", plot.number))
  
  return(output)
}


#-------------------------------------------#
####    Calculating Water Level Stats    ####
#-------------------------------------------#

## Calculate the water level stats; 2015 does not have enough data so skipping here

## Great Meadow
gm_wl_stats <- map_dfr(1:6, ~wl_stats_loop(.))

## Gilmore Meadow
gil_wl_stats <- calc_WL_stats(gilm, from = 2016, to = 2025)

## Merge the two meadow wl stats
wl_stats <- bind_rows(gm_wl_stats, gil_wl_stats) %>% 
  select(site, Year, everything())

## Pivot into a table for exploration
wl_table <- wl_stats %>% 
  pivot_longer(cols = WL_mean:prop_under_neg30cm) %>% 
  pivot_wider(names_from = site, values_from = value) %>% 
  select(year = Year, stat = name, gilmore.meadow, great.meadow.1, great.meadow.2,
         great.meadow.3, great.meadow.4, great.meadow.5, great.meadow.6)


## Write out the water level stats
# write_csv(wl_table, "data/processed_data/hydrology_data/gm_gl_wl_stats_2025_20260304.csv")




