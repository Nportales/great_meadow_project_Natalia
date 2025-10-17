#### Hydrograph Visualizer Shiny Dashboard #### 

#---------------------------------------------#
####        Load Required Packages         ####
#---------------------------------------------#

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

#-------------------------------------------#
####    Read & Prepare Processed Data    ####
#-------------------------------------------#

# Great Meadow data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250915.csv") %>%
  mutate(date = as.Date(date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, year, sep = "_"))

# Gilmore Meadow data 
gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    
    # Try to parse full datetime, fallback to date only + midnight
    timestamp_parsed = mdy_hm(timestamp),
    timestamp_parsed = if_else(
      is.na(timestamp_parsed),
      mdy(timestamp),
      timestamp_parsed
    ),
    timestamp_parsed = as.POSIXct(timestamp_parsed),
    timestamp = timestamp_parsed,
    
    # Standardize Date column
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         date = Date,
         doy,
         year = Year,
         precip.cm = precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# Need to select Great Meadow precip data as default for all data since Gilmore Meadow has precip data too
# Create precipitation lookup from Great Meadow data
gm_precip_lookup <- gm %>%
  select(timestamp, precip.cm, lag.precip) %>%
  distinct()

# Replace Gilmore precip with Great Meadow precip for matching timestamps
gl_with_gm_precip <- gl %>%
  select(-precip.cm, -lag.precip) %>%  # Remove original precip columns
  left_join(gm_precip_lookup, by = "timestamp") %>%
  # If no match found, keep original Gilmore precip values as fallback
  left_join(
    gl %>% select(timestamp, orig_precip_cm = precip.cm, orig_lag_precip = lag.precip),
    by = "timestamp"
  ) %>%
  mutate(
    precip.cm = coalesce(precip.cm, orig_precip_cm),
    lag.precip = coalesce(lag.precip, orig_lag_precip)
  ) %>%
  select(-orig_precip_cm, -orig_lag_precip)

# Combine Great Meadow and Gilmore Meadow datasets
all_data <- bind_rows(gm, gl_with_gm_precip) %>% 
  filter(year >= 2016 & year <= 2024) %>% 
  select(timestamp, 
         date, 
         year, 
         doy, 
         hr, 
         doy_h,
         precip_cm = precip.cm,
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# Format water level stats for WL table output 
wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         `Gilmore Meadow` = gilmore.meadow, 
         `Great Meadow 1` = great.meadow.1, 
         `Great Meadow 2` = great.meadow.2, 
         `Great Meadow 3` = great.meadow.3, 
         `Great Meadow 4` = great.meadow.4, 
         `Great Meadow 5` = great.meadow.5, 
         `Great Meadow 6` = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )

#-----------------------#
####    Functions    ####
#-----------------------#

# Function to calculate significance tests between wetlands
calculate_wetland_significance <- function(data, selected_years, selected_sites, alpha = 0.05) {
  # Check minimum number of years
  if (length(selected_years) <= 3) {
    message("⚠️ Not enough years selected for significance testing (need >3).")
    return(NULL)
  }
  
  # Filter data for selected years and sites
  filtered_data <- data %>%
    filter(year %in% selected_years, site %in% selected_sites) %>%
    mutate(site_group = ifelse(grepl("Great Meadow", site), "Great Meadow", site))
  
  # Check both wetlands are present
  wetlands_present <- unique(filtered_data$site_group)
  if (length(wetlands_present) < 2 || !all(c("Great Meadow", "Gilmore Meadow") %in% wetlands_present)) {
    message("⚠️ Both Great Meadow and Gilmore Meadow must be present for comparison.")
    return(NULL)
  }
  
  # Get numeric stat columns
  stat_cols <- filtered_data %>%
    select(where(is.numeric), -year) %>%
    names()
  
  # Step 1: average each variable by wetland × year
  yearly_means <- filtered_data %>%
    group_by(year, site_group) %>%
    summarise(across(all_of(stat_cols), mean, na.rm = TRUE), .groups = "drop")
  
  # Step 2: run t-tests across wetlands for each variable
  t_test_results <- map_dfr(stat_cols, function(var) {
    tryCatch({
      test <- t.test(as.formula(paste(var, "~ site_group")), data = yearly_means)
      data.frame(
        variable = var,
        p_value = test$p.value,
        significant = test$p.value < alpha,
        stringsAsFactors = FALSE
      )
    }, error = function(e) {
      data.frame(
        variable = var,
        p_value = NA,
        significant = FALSE,
        stringsAsFactors = FALSE
      )
    })
  })
  
  return(t_test_results)
}




#----------------#
####    UI    ####
#----------------#

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1B365D",
    secondary = "#4C6D9A",    
    success = "#2E86C1",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Open Sans", wght = c(400, 700))
  ),
  
  # Custom CSS for additional styling
  tags$head(
    tags$style(HTML("
      .content-section {
        margin: 30px 0;
        padding: 25px;
        border-radius: 15px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border: 2px solid #1B365D;
      }
      
      .section-title {
        color: #1B365D;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #4C6D9A;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #1B365D 0%, #4C6D9A 100%);
        color: white;
        padding: 30px;
        margin: -15px -15px 30px -15px;
        text-align: center;
        border-radius: 0 0 20px 20px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      }
      
      .brush-info-section {
        background: linear-gradient(135deg, #e8f4f8 0%, #f0f8ff 100%);
        border-radius: 12px;
        padding: 20px;
        margin: 20px 0;
        border: 1px solid #4C6D9A;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
      .dataTables_wrapper {
        font-size: 0.85rem !important;
      }

      .dataTables_wrapper table {
        font-size: 0.8rem !important;
      }

      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_paginate {
        font-size: 0.75rem !important;
      }
      
    .significance-info h5 {
      background-color: #fff3cd;
      color: #856404;
      padding: 6px 10px;
      border-radius: 4px;
      display: inline-block;
      margin-bottom: 10px;
    }
    
    .significance-info p {
      margin-bottom: 8px;
      font-size: 0.9rem;
    }
    
    .significance-info ul li {
      font-size: 0.85rem;
      color: #856404;
    }
    
    .significance-info .note {
      margin-top: 5px;
      margin-bottom: 10px;
      font-size: 0.8rem;
      font-style: italic;
      color: #6c757d;
    }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrographs 
  div(class = "content-section",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("selected_sites", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(all_data$site)),
                      selected = c("Great Meadow 1"),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2024,
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool (+) by clicking and dragging with your cursor to select data on the hydrograph and view below.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 10px; text-align: center;",
              downloadButton("download_plot", "Download Hydrograph", 
                             class = "btn-primary btn-sm", 
                             icon = icon("image"))),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_brush", "Download Selected Data", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#4C6D9A", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section 
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              card(
                card_header(
                  class = "bg-success text-white",
                  "Selected Data from Hydrograph:"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats
  div(class = "content-section stats-main",
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #1B365D; margin-bottom: 20px;"),
          
          pickerInput("stats_site", 
                      label = div(icon("map-marker"), "Select Site(s):"),
                      choices = sort(unique(wl_stats$site)),
                      selected = "Great Meadow 1",
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose site(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("stats_year", 
                          label = div(icon("calendar"), "Select Years:"),
                          choices = sort(unique(wl_stats$year)),
                          selected = c(2024, 2023, 2022, 2021),
                          multiple = TRUE,
                          options = list(
                            `actions-box` = TRUE,
                            `deselect-all-text` = "Clear all",
                            `select-all-text` = "Select all",
                            `none-selected-text` = "Choose year(s)",
                            `live-search` = TRUE,
                            style = "btn-outline-primary"
                          ))),
          
          div(style = "margin-bottom: 15px;",
              radioButtons(
                "time_summary", "Summarize Water Level Statistics By:",
                choices = c(
                  "Each Year" = "year",
                  "Average Across Years" = "multi",
                  "All Sites (with statistical significance)" = "all_sites"
                ),
                selected = "year")),
          
          br(),
          div(style = "padding: 10px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
          div(style = "margin-top: 15px; text-align: center;",
              downloadButton("download_stats", "Download Table", 
                             class = "btn-primary btn-sm", 
                             icon = icon("download")))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          
          div(style = "padding: 10px;",
              uiOutput("significance_info"),
              dataTableOutput("wl_stats"))
        )
      )
  )
)

#--------------------#
####    SERVER    ####
#--------------------#

server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$selected_sites)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% input$selected_sites)
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Color palette for sites
    sites <- unique(plot_data_filtered$site)
    site_colors <- c(
      "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
      "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
      "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
    )
    
    # Create the plot
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(aes(color = site), size = 0.7) +
      
      # Precip line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = site_colors, 
                         breaks = c(sites, "Precipitation")) +
      
      # Axes and labels
      labs(y = 'Water Level (cm)', x = 'Date') +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      
      # Secondary axis for precip
      scale_y_continuous(
        sec.axis = sec_axis(~ .,
                            breaks = c(minWL, minWL + 10),
                            name = 'Hourly Precip. (cm)',
                            labels = c('0', '2'))
      ) +
      
      # Theme
      theme_bw() +
      theme(
        plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y.right = element_text(color = 'blue'),
        axis.title.y.right = element_text(color = 'blue'),
        strip.text = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_blank()
      )
  })
  
  # Reactive for processed brushed data - with site columns
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Get brushed points using brushedPoints()
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(timestamp, year, doy_h, site, water_depth, lag_precip) %>%
      arrange(year, doy_h) %>%
      mutate(
        doy_h = round(doy_h, 2),
        water_depth = round(water_depth, 2),
        lag_precip = round(lag_precip, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if (nrow(selected_data) == 0) {
      return(data.frame(Message = "No points selected - try brushing over the water level lines"))
    }
    
    # Create the base data with unique timestamp/precip combinations
    base_data <- selected_data %>%
      group_by(year, doy_h, timestamp, lag_precip) %>%
      summarise(.groups = 'drop') %>%
      arrange(year, doy_h)
    
    # Create water depth columns for each site
    water_depth_data <- selected_data %>%
      group_by(year, doy_h, timestamp, site) %>%
      summarise(water_depth = mean(water_depth, na.rm = TRUE), .groups = 'drop') %>%
      pivot_wider(names_from = site, values_from = water_depth, names_prefix = "")
    
    # Join the data together
    result_data <- base_data %>%
      left_join(water_depth_data, by = c("year", "doy_h", "timestamp")) %>%
      rename(
        Year = year,
        Timestamp = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      )
    
    # Add "Water Depth (cm)" suffix to site columns
    site_columns <- intersect(names(result_data), input$selected_sites)
    if (length(site_columns) > 0) {
      result_data <- result_data %>%
        rename_with(~ paste(.x, "Water Depth (cm)"), .cols = all_of(site_columns))
    }
    
    result_data %>% arrange(Year, `Day of Year`)
  })
  
  # Reactive for significance testing results - checks if both wetlands are selected
  significance_results <- reactive({
    req(input$stats_year, input$stats_site)
    
    # Check if both wetlands are represented in selected sites
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    # Only calculate significance if both wetlands are represented
    if (length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands)) {
      calculate_wetland_significance(wl_stats, input$stats_year, input$stats_site, alpha = 0.05)
    } else {
      NULL
    }
  })
  
  # Reactive to determine if we should show significance info
  show_significance_info <- reactive({
    req(input$stats_site, input$time_summary)
    
    if (input$time_summary != "all_sites") return(FALSE)
    
    selected_wetlands <- wl_stats %>%
      filter(site %in% input$stats_site) %>%
      pull(wetland) %>%
      unique()
    
    return(length(selected_wetlands) >= 2 && all(c("Great Meadow", "Gilmore Meadow") %in% selected_wetlands))
  })
  
  
  # Setting up average WL stats and calculating stats output tables
  filtered_stats <- reactive({
    req(input$stats_site, input$stats_year, input$time_summary)
    
    data <- wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year)
    
    if (input$time_summary == "year") {
      # ---- Option 1: Per-Year Summary ----
      data %>%
        mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
        select(
          Year = year,
          Site = site,
          `Mean Water Level (cm)` = WL_mean,
          `SD Water Level (cm)` = WL_sd,
          `Minimum Water Level (cm)` = WL_min,
          `Maximum Water Level (cm)` = WL_max,
          `Maximum Hourly Increase (cm)` = max_inc,
          `Maximum Hourly Decrease (cm)` = max_dec,
          `Growing Season Change (cm)` = GS_change,
          `GS % Surface Water` = prop_over_0cm,
          `GS % Within 30cm` = prop_bet_0_neg30cm,
          `GS % Over 30cm Deep` = prop_under_neg30cm
        )
    } else if (input$time_summary == "multi") {
      # ---- Option 2: Average Across Years ----
      # Per-site summary
      data_site <- data %>%
        group_by(site, wetland) %>%
        summarise(
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        rename(Site = site, Wetland = wetland)
      
    } else if (input$time_summary == "all_sites") {
      # ---- Option 3: All Sites (with statistical significance) ----
      
      # Only run if enough years are selected
      if (length(unique(input$stats_year)) <= 3) {
        return(data.frame(
          Message = "Significance testing requires at least 4 years of data."
        ))
      }
      
      sig_results <- calculate_wetland_significance(
        wl_stats,
        selected_years = input$stats_year,
        selected_sites = input$stats_site
      )
      
      # Per-wetland "All Sites" summary
      all_sites_data <- data %>%
        group_by(wetland) %>%
        summarise(
          site = "All Sites",
          Year = paste0(min(year), "–", max(year)),
          `Mean Water Level (cm)` = round(mean(WL_mean, na.rm = TRUE), 2),
          `SD Water Level (cm)` = round(mean(WL_sd, na.rm = TRUE), 2),
          `Minimum Water Level (cm)` = round(mean(WL_min, na.rm = TRUE), 2),
          `Maximum Water Level (cm)` = round(mean(WL_max, na.rm = TRUE), 2),
          `Maximum Hourly Increase (cm)` = round(mean(max_inc, na.rm = TRUE), 2),
          `Maximum Hourly Decrease (cm)` = round(mean(max_dec, na.rm = TRUE), 2),
          `Growing Season Change (cm)` = round(mean(GS_change, na.rm = TRUE), 2),
          `GS % Surface Water` = round(mean(prop_over_0cm, na.rm = TRUE), 2),
          `GS % Within 30cm` = round(mean(prop_bet_0_neg30cm, na.rm = TRUE), 2),
          `GS % Over 30cm Deep` = round(mean(prop_under_neg30cm, na.rm = TRUE), 2),
          .groups = "drop"
        ) %>%
        rename(Site = site, Wetland = wetland)
      
      if (is.null(sig_results)) {
        all_sites_data
      } else {
        all_sites_data
      }
    }
  })
  
  
  
  # Reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # Output for significance information display
  output$significance_info <- renderUI({
    # Ensure the input exists before using it
    req(input$summary_option)
    
    # Only show this section when "All Sites (with statistical significance)" is selected
    if (input$summary_option != "All Sites (with statistical significance)") {
      return(NULL)
    }
    
    # Base note (directions)
    base_note <- div(
      style = "margin-bottom: 10px; background-color: #f9f9f9; padding: 10px; border-left: 4px solid #1B365D;",
      tags$b("Note:"),
      p("• To compare sites, both Great Meadow and Gilmore Meadow must be selected."),
      p("• Significance testing is only available when more than three years are selected."),
      p("• If fewer than three years are chosen, results will display without significance testing.")
    )
    
    # Significance info (only if available)
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        sig_vars <- sig_results %>% 
          filter(significant) %>% 
          pull(variable)
        
        if (length(sig_vars) > 0) {
          # Map variable names to display names
          var_display_names <- c(
            "WL_mean" = "Mean Water Level",
            "WL_sd" = "SD Water Level", 
            "WL_min" = "Minimum Water Level",
            "WL_max" = "Maximum Water Level",
            "max_inc" = "Maximum Hourly Increase",
            "max_dec" = "Maximum Hourly Decrease",
            "GS_change" = "Growing Season Change",
            "prop_over_0cm" = "GS % Surface Water",
            "prop_bet_0_neg30cm" = "GS % Within 30cm",
            "prop_under_neg30cm" = "GS % Over 30cm Deep"
          )
          
          sig_display_names <- var_display_names[sig_vars]
          sig_display_names <- sig_display_names[!is.na(sig_display_names)]
          
          return(tagList(
            base_note,
            div(class = "significance-info",
                h5(icon("asterisk"), " Statistical Significance"),
                p("Yellow highlighted variables show significant differences (p < 0.05) between Great Meadow and Gilmore Meadow wetlands:"),
                tags$ul(
                  lapply(sig_display_names, function(name) {
                    tags$li(name)
                  })
                ),
                p(class = "note", "Statistical tests compare averages across all selected years and sites within each wetland.")
            )
          ))
        }
      }
    }
    
    # Show note even if no significance info is available
    return(base_note)
  })
  
  # reactive table output for WL stats with significance highlighting
  output$wl_stats <- DT::renderDataTable({
    data <- filtered_stats()
    
    # Reorder so "All Sites" rows come first
    data <- data %>%
      dplyr::arrange(desc(Site == "All Sites"), Site)
    
    dt <- datatable(
      data,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        columns = names(data),  # apply to all columns
        valueColumns = "Site",
        fontWeight = styleEqual("All Sites", "bold")
      )
    
    # Add significance highlighting only when appropriate
    if (show_significance_info()) {
      sig_results <- significance_results()
      
      if (!is.null(sig_results)) {
        # Map variable names to column names in the table
        var_mapping <- c(
          "WL_mean" = "Mean Water Level (cm)",
          "WL_sd" = "SD Water Level (cm)", 
          "WL_min" = "Minimum Water Level (cm)",
          "WL_max" = "Maximum Water Level (cm)",
          "max_inc" = "Maximum Hourly Increase (cm)",
          "max_dec" = "Maximum Hourly Decrease (cm)",
          "GS_change" = "Growing Season Change (cm)",
          "prop_over_0cm" = "GS % Surface Water",
          "prop_bet_0_neg30cm" = "GS % Within 30cm",
          "prop_under_neg30cm" = "GS % Over 30cm Deep"
        )
        
        # Apply highlighting for significant variables
        for (var_name in names(var_mapping)) {
          col_name <- var_mapping[var_name]
          sig_row <- sig_results[sig_results$variable == var_name, ]
          
          if (nrow(sig_row) > 0 && sig_row$significant) {
            dt <- dt %>%
              formatStyle(
                col_name,
                valueColumns = "Site",
                backgroundColor = styleEqual("All Sites", "#fff3cd"), # light yellow highlight
                fontWeight = styleEqual("All Sites", "bold")
              )
          }
        }
      }
    }
    
    dt
  })
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_brush_data(), file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      if (input$time_summary == "multi") {
        paste("wetland_averaged_stats_", Sys.Date(), ".csv", sep = "")
      } else if (input$time_summary == "all_sites") {
        paste("all_sites_significance_stats_", Sys.Date(), ".csv", sep = "")
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
  # Download handler for hydrograph plot
  output$download_plot <- downloadHandler(
    filename = function() {
      sites_label <- paste(input$selected_sites, collapse = "_")
      years_label <- paste(input$year, collapse = "_")
      paste0("hydrograph_", sites_label, "_", years_label, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # Recreate the plot
      plot_data_filtered <- plot_data()
      req(nrow(plot_data_filtered) > 0)
      
      # Calculate minimum water level across all data for consistent precipitation baseline
      minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
      
      # Create color palette for sites
      sites <- unique(plot_data_filtered$site)
      site_colors <- c(
        "Great Meadow 1" = "black", "Great Meadow 2" = "chartreuse4", "Great Meadow 3" = "green",
        "Great Meadow 4" = "darkorange", "Great Meadow 5" = "deeppink2", "Great Meadow 6" = "purple",
        "Gilmore Meadow" = "darkgray", "Precipitation" = "blue"
      )
      
      # Create the plot
      p <- ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
        # Water level lines by site
        geom_line(aes(color = site), size = 0.7) +
        
        # Precipitation line (scaled by multiplier of 5 and offset by minWL)
        geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                  size = 0.7) +
        
        # Ground level reference
        geom_hline(yintercept = 0, color = 'brown') +
        
        # Facet by year
        facet_wrap(~ year, ncol = 1, scales = "free_y") +
        
        # Colors
        scale_color_manual(values = site_colors, 
                           breaks = c(sites, "Precipitation")) +
        
        # Axes and labels
        labs(y = 'Water Level (cm)', x = 'Date') +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
        ) +
        
        # Secondary axis for precipitation
        scale_y_continuous(
          sec.axis = sec_axis(~ .,
                              breaks = c(minWL, minWL + 10),
                              name = 'Hourly Precip. (cm)',
                              labels = c('0', '2'))
        ) +
        
        # Theme
        theme_bw() +
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text.y.right = element_text(color = 'blue'),
          axis.title.y.right = element_text(color = 'blue'),
          strip.text = element_text(size = 11),
          legend.position = "bottom",
          legend.title = element_blank()
        )
      
      # Save the plot
      ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
    }
  )
  
}

# Run app
shinyApp(ui, server)

