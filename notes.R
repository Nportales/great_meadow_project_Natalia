
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

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
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

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
  arrange(site, year)


# UI 
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
    heading_font = font_google("Roboto", wght = c(400, 700))
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
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
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
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
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
  
  # Selected data points section with improved styling
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
  
  # Second section: Water Level Stats with improved styling
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
                          selected = c(2023, 2022),
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
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
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
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - CORRECTED WIDE FORMAT
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create wide format with separate columns for each site
      wide_data <- selected_data %>%
        select(year, doy_h, timestamp, water_depth, site) %>%
        # Round for better display
        mutate(
          doy_h = round(doy_h, 2),
          water_depth = round(water_depth, 2),
          timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
        ) %>%
        # Group by time points that are very close together (within 0.1 doy_h)
        mutate(time_group = round(doy_h * 10) / 10) %>%
        group_by(year, time_group) %>%
        summarise(
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          .groups = 'drop'
        ) %>%
        # Get the water level data for each site at these time points
        left_join(
          selected_data %>%
            select(year, doy_h, water_depth, site) %>%
            mutate(
              time_group = round(doy_h * 10) / 10,
              water_depth = round(water_depth, 2)
            ) %>%
            group_by(year, time_group, site) %>%
            summarise(water_depth = first(water_depth), .groups = 'drop') %>%
            pivot_wider(
              names_from = site,
              values_from = water_depth,
              names_glue = "{tolower(gsub(' ', '_', site))}_wl"
            ),
          by = c("year", "time_group")
        ) %>%
        select(-time_group) %>%
        rename(
          Year = year,
          Timestamp = timestamp,
          `Day of Year` = doy_h
        ) %>%
        arrange(Year, `Day of Year`)
      
      wide_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler for brushed data - UPDATED TO DOWNLOAD WIDE FORMAT
  output$download_brush <- downloadHandler(
    filename = function() {
      paste("hydrograph_selected_data_wide_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the same wide format data as displayed in the table
      plot_data_filtered <- plot_data()
      
      if(is.null(input$hydro_brush) || nrow(plot_data_filtered) == 0) {
        # Create empty file if no data
        write.csv(data.frame(Message = "No data selected"), file, row.names = FALSE)
        return()
      }
      
      selected_data <- brushedPoints(
        plot_data_filtered,
        brush = input$hydro_brush,
        xvar = "doy_h",
        yvar = "water_depth"
      )
      
      if(nrow(selected_data) == 0) {
        write.csv(data.frame(Message = "No points selected"), file, row.names = FALSE)
      } else {
        # Use the same wide format logic as the table display
        wide_data <- selected_data %>%
          select(year, doy_h, timestamp, water_depth, site) %>%
          mutate(
            doy_h = round(doy_h, 2),
            water_depth = round(water_depth, 2),
            timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
          ) %>%
          mutate(time_group = round(doy_h * 10) / 10) %>%
          group_by(year, time_group) %>%
          summarise(
            timestamp = first(timestamp),
            doy_h = first(doy_h),
            .groups = 'drop'
          ) %>%
          left_join(
            selected_data %>%
              select(year, doy_h, water_depth, site) %>%
              mutate(
                time_group = round(doy_h * 10) / 10,
                water_depth = round(water_depth, 2)
              ) %>%
              group_by(year, time_group, site) %>%
              summarise(water_depth = first(water_depth), .groups = 'drop') %>%
              pivot_wider(
                names_from = site,
                values_from = water_depth,
                names_glue = "{tolower(gsub(' ', '_', site))}_wl"
              ),
            by = c("year", "time_group")
          ) %>%
          select(-time_group) %>%
          rename(
            Year = year,
            Timestamp = timestamp,
            Day_of_Year = doy_h
          ) %>%
          arrange(Year, Day_of_Year)
        
        write.csv(wide_data, file, row.names = FALSE)
      }
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use your filtered stats data
      filtered_stats <- wl_stats %>%
        filter(site %in% input$stats_site,
               year %in% input$stats_year)
      write.csv(filtered_stats, file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)






### app with pivot wider dataset

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read and prepare your processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

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
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

# combine all well data together
all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) %>% 
  select(timestamp, 
         date = Date, 
         year = Year, 
         doy, 
         hr, 
         doy_h, 
         lag_precip = lag.precip,
         water_depth = water.depth,
         site)

# # Re-format data
# # 1. Separate metadata per timestamp
# timestamp_meta <- all_data %>%
#   select(timestamp, Date, Year, doy, hr, doy_h, lag.precip) %>%
#   distinct()
# 
# # 2. Pivot water level wide
# wl_wide <- all_data %>%
#   select(timestamp, site, water.depth) %>%
#   pivot_wider(names_from = site, values_from = water.depth) %>%
#   left_join(timestamp_meta, by = "timestamp") %>%
#   relocate(timestamp, Date, Year, doy, hr, doy_h, lag.precip) %>% 
#   rename(date = Date,
#          year = Year,
#          lag_precip = lag.precip,
#          gilmore_meadow = `Gilmore Meadow`, 
#          great_meadow_1 = `Great Meadow 1`, 
#          great_meadow_2 = `Great Meadow 2`,
#          great_meadow_3 = `Great Meadow 3`,
#          great_meadow_4 = `Great Meadow 4`,
#          great_meadow_5 = `Great Meadow 5`,
#          great_meadow_6 = `Great Meadow 6`)


# Read and prepare wl stats
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
  arrange(site, year)


# UI 
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
    heading_font = font_google("Roboto", wght = c(400, 700))
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
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          div(style = "margin-bottom: 15px;",
              pickerInput("year", 
                          label = div(icon("calendar"), "Select Year(s):"),
                          choices = sort(unique(all_data$year)),
                          selected = 2023,
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
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the hydrograph for a detailed view of the data.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;")),
          
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
  
  # Selected data points section with improved styling
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
  
  # Second section: Water Level Stats with improved styling
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
                          selected = c(2023, 2022),
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
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
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
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(year) %>%
      summarise(
        min_water = min(c(
          water_depth[site == input$gm_site & !is.na(water_depth)],
          water_depth[site == "Gilmore Meadow" & !is.na(water_depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "year") %>%
      mutate(precip_y = lag_precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water_depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water_depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    brush <- input$hydro_brush
    if (is.null(brush)) return(NULL)
    
    filtered_long <- all_data %>%
      filter(doy_h >= brush$xmin, doy_h <= brush$xmax,
             water_depth >= brush$ymin, water_depth <= brush$ymax,
             site %in% c("Gilmore Meadow", input$gm_site)) %>%
      select(timestamp, site, water_depth) %>%
      distinct()
    
    wide_selected <- filtered_long %>%
      pivot_wider(names_from = site, values_from = water_depth)
    
    wide_selected
  })
  
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # Download handler for brushed data
  output$download_brush <- downloadHandler(
    filename = function() {
      paste0("selected_hydrograph_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- event_data("plotly_selected", source = "subset_data")
      
      if (is.null(d)) {
        return(NULL)
      }
      
      selected_timestamps <- as.POSIXct(d$x, origin = "1970-01-01", tz = "America/New_York")
      
      filtered_long <- all_data %>%
        filter(timestamp %in% selected_timestamps,
               site %in% c("Gilmore Meadow", input$gm_site)) %>%
        select(timestamp, site, water_depth) %>%
        distinct()
      
      wide_selected <- filtered_long %>%
        pivot_wider(names_from = site, values_from = water_depth)
      
      write.csv(wide_selected, file, row.names = FALSE)
    }
  )
  
  # Download handler for water level stats
  output$download_stats <- downloadHandler(
    filename = function() {
      paste("water_level_stats_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Use your filtered stats data
      filtered_stats <- wl_stats %>%
        filter(site %in% input$stats_site,
               year %in% input$stats_year)
      write.csv(filtered_stats, file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)











#### More complex style and design version 7/30/25

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

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
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

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
  arrange(site, year)


# UI with improved styling
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C5F41",
    secondary = "#5A9B7C", 
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
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
        border-left: 5px solid #2C5F41;
      }
      
      .section-title {
        color: #2C5F41;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #5A9B7C;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #2C5F41 0%, #5A9B7C 100%);
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
        border: 1px solid #5A9B7C;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
      
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrograph with improved styling
  div(class = "content-section",
      h2("Wetland Hydrograph Analysis", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Hydrograph Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          br(),
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$Year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the chart for detailed analysis.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#5A9B7C", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              h3("Selected Data Points:", 
                 style = "color: #2C5F41; text-align: center; margin-bottom: 20px;"),
              card(
                card_header(
                  class = "bg-info text-white",
                  "Detailed View of Brushed Data"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      h2("Water Level Statistics", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
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
          
          br(),
          
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2022, 2023),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = NULL,
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)





#### BASIC HYDROGRAPH VISUALIZER

library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

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
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

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
  arrange(site, year)


# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Visualizer"),
  
  # First section: Hydrograph with its own sidebar
  sidebarLayout(
    sidebarPanel(
      h4("Hydrograph Controls"),
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Site(s) for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1")
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px", brush = brushOpts(id = "hydro_brush"))
    )
  ),
  
  # Selected data points section
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Selected Data Points:"),
           tableOutput("brush_info"))
  ),
  
  # Second section: Water Level Stats with its own sidebar
  h3("Water Level Statistics", style = "text-align: center; margin-top: 40px; margin-bottom: 20px;"),
  sidebarLayout(
    sidebarPanel(
      h4("Statistics Controls"),
      pickerInput("stats_site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  )),
      pickerInput("stats_year", "Select Year(s) for Stats:",
                  choices = sort(unique(wl_stats$year)),
                  selected = c(2022, 2023),
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)"))
    ),
    mainPanel(
      dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = "Hydrographs by Year",
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)





#-------------------------------------------------------------------------------


library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

wl_stats <- read.csv("data/processed_data/gm_gl_wl_stats.csv") %>% 
  select(year,
         stat, 
         Gilmore Meadow = gilmore.meadow, 
         Great Meadow 1 = great.meadow.1, 
         Great Meadow 2 = great.meadow.2, 
         Great Meadow 3 = great.meadow.3, 
         Great Meadow 4 = great.meadow.4, 
         Great Meadow 5 = great.meadow.5, 
         Great Meadow 6 = great.meadow.6) %>% 
  pivot_longer(cols = -c(year, stat), names_to = "site", values_to = "value") %>% 
  pivot_wider(names_from = stat, values_from = value) %>%
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(actions-box = TRUE,
                                 deselect-all-text = "Clear all",
                                 select-all-text = "Select all",
                                 none-selected-text = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    actions-box = TRUE,
                    deselect-all-text = "Clear all",
                    select-all-text = "Select all",
                    none-selected-text = "Choose site(s)"
                  ))
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px")
    )
  ),
  
  fluidRow(
    column(width = 10, offset = 1, align = "center",
           h4("Growing Season Water Level Statistics"),
           dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_list <- lapply(input$year, function(yr) {
      plot_data_year <- all_data %>%
        filter(Year == yr, doy > 134, doy < 275)
      
      # Calculate min water level for consistent precipitation offset
      min_gm <- plot_data_year %>%
        filter(site == input$gm_site, !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      min_gil <- plot_data_year %>%
        filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
        pull(water.depth) %>% 
        min(na.rm = TRUE)
      
      minWL <- min(min_gm, min_gil)
      
      # Create individual hydrograph plot
      ggplot() +
        geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                  aes(x = doy_h, y = water.depth, color = input$gm_site), size = 0.7) +
        geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                  aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 0.7) +
        geom_line(data = plot_data_year,
                  aes(x = doy_h, y = lag.precip * 5 + minWL),
                  color = "blue", size = 0.7) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_color_manual(values = setNames(c("black", "darkgray"), 
                                             c(input$gm_site, "Gilmore Meadow"))) +
        labs(title = paste("Hydrograph for", yr),
             x = "Date", y = "Water Level (cm)") +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
        scale_y_continuous(
          name = "Water Level (cm)",
          sec.axis = sec_axis(~ (. - minWL) / 5,
                              name = "Hourly Precip (cm)",
                              breaks = seq(0, 8, by = 2))
        ) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              strip.text = element_text(size = 11),
              plot.title = element_text(hjust = 0.5))
    })
    
    # Combine plots vertically using patchwork
    wrap_plots(plot_list, ncol = 1)
  })
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        Mean Water Level = WL_mean,
        SD Water Level = WL_sd,
        Minimum Water Level = WL_min,
        Maximum Water Level = WL_max,
        Maximum Hourly Increase = max_inc,
        Maximum Hourly Decrease = max_dec,
        Growing Season Change = GS_change,
        GS % Complete Data = prop_GS_comp,
        GS % Surface Water = prop_over_0cm,
        GS % Within 30cm = prop_bet_0_neg30cm,
        GS % Over 30cm Deep = prop_under_neg30cm
      )
  })
  
  
}



# Run app
shinyApp(ui, server)




--------------------------------------------------------------------------------



# Load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(plotly)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%  # drop the all-NA column
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

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
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  ))
    ),
    mainPanel(
      plotlyOutput("hydrograph", height = "600px"),
      br(),
      downloadButton("download_points", "Download Selected Points"),
      br(), br(),
      DTOutput("selected_points"),
      br(), br(),
      fluidRow(
        column(width = 10, offset = 1,
               div(style = "text-align: center;",
                   h4("Growing Season Water Level Statistics")),
               DTOutput("wl_stats"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_data <- all_data %>%
      filter(Year %in% input$year,
             doy > 134, doy < 275,
             site %in% c(input$gm_site, "Gilmore Meadow")) %>%
      mutate(row_id = row_number())  # assign temporary row id
    
    # Store selected plot data in a reactive for reuse
    plot_data_reactive <- reactive({
      plot_data
    })
    
    p <- ggplot(plot_data, aes(x = doy_h, y = water.depth, color = site, key = row_id)) +
      geom_line(size = 0.7) +
      geom_point(size = 1.5, alpha = 0.7) +
      geom_hline(yintercept = 0, color = "brown") +
      scale_color_manual(values = c("black", "darkgray")) +
      labs(title = "Interactive Hydrograph", x = "DOY", y = "Water Level (cm)") +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggplotly(p, tooltip = c("x", "y", "color", "key")) %>%
      layout(dragmode = "select")
  })
  
  output$selected_points <- renderDataTable({
    sel <- event_data("plotly_selected")
    if (is.null(sel)) return(NULL)
    
    ids <- sel$key
    plot_data_reactive() %>% filter(row_id %in% ids)
  })
  
  output$download_points <- downloadHandler(
    filename = function() {
      paste0("selected_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sel <- event_data("plotly_selected")
      if (is.null(sel)) return()
      
      ids <- sel$key
      data_out <- plot_data_reactive() %>% filter(row_id %in% ids)
      
      write.csv(data_out, file, row.names = FALSE)
    }
  )
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Complete Data` = prop_GS_comp,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm
      )
  })
  
  
}



# Run app
shinyApp(ui, server)




--------------------------------------------------------------------------------
  
  
  
  
  
  # Load packages
  library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)

# Read & prepare your processed data
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  select(-timestamp) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         site = "Gilmore Meadow",
         siteyear = paste(site, Year, sep = "_"))

all_data <- bind_rows(gm, gl) %>%
  filter(Year >= 2016 & Year <= 2023)

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
  arrange(site, year)

# UI
ui <- fluidPage(
  titlePanel("Wetland Hydrograph Viewer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput("year", "Select Year(s):",
                  choices = sort(unique(all_data$Year)),
                  selected = 2023,
                  multiple = TRUE,
                  options = list(`actions-box` = TRUE,
                                 `deselect-all-text` = "Clear all",
                                 `select-all-text` = "Select all",
                                 `none-selected-text` = "Choose year(s)")),
      selectInput("gm_site", "Select Great Meadow Plot for Hydrograph:",
                  choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                  selected = "Great Meadow 1"),
      
      pickerInput("site", "Select Site(s) for Water Level Stats:",
                  choices = sort(unique(wl_stats$site)),
                  selected = "Great Meadow 1",
                  multiple = TRUE,
                  options = list(
                    `actions-box` = TRUE,
                    `deselect-all-text` = "Clear all",
                    `select-all-text` = "Select all",
                    `none-selected-text` = "Choose site(s)"
                  )),
      br(),
      downloadButton("download_points", "Download Selected Points")
    ),
    mainPanel(
      plotOutput("hydrograph", height = "600px", brush = brushOpts(id = "plot_brush")),
      br(),
      h4("Selected Points from Hydrograph"),
      DTOutput("selected_points"),
      br()
    ),
    fluidRow(
      column(width = 10, offset = 1, align = "center",
             h4("Growing Season Water Level Statistics"),
             dataTableOutput("wl_stats"))
    )
  )
)

# Server
server <- function(input, output, session) {
  output$hydrograph <- renderPlot({
    req(input$year, input$gm_site)
    
    plot_list <- lapply(input$year, function(yr) {
      plot_data_year <- all_data %>%
        filter(Year == yr, doy > 134, doy < 275)
      
      min_gm <- plot_data_year %>%
        filter(site == input$gm_site, !is.na(water.depth)) %>%
        pull(water.depth) %>%
        min(na.rm = TRUE)
      
      min_gil <- plot_data_year %>%
        filter(site == "Gilmore Meadow", !is.na(water.depth)) %>%
        pull(water.depth) %>%
        min(na.rm = TRUE)
      
      minWL <- min(min_gm, min_gil)
      
      ggplot() +
        geom_line(data = plot_data_year %>% filter(site == input$gm_site),
                  aes(x = doy_h, y = water.depth, color = input$gm_site), size = 0.7) +
        geom_line(data = plot_data_year %>% filter(site == "Gilmore Meadow"),
                  aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), size = 0.7) +
        geom_line(data = plot_data_year,
                  aes(x = doy_h, y = lag.precip * 5 + minWL),
                  color = "blue", size = 0.7) +
        geom_hline(yintercept = 0, color = "brown") +
        scale_color_manual(values = setNames(c("black", "darkgray"),
                                             c(input$gm_site, "Gilmore Meadow"))) +
        labs(title = paste("Hydrograph for", yr),
             x = "Date", y = "Water Level (cm)") +
        scale_x_continuous(
          breaks = c(121, 152, 182, 213, 244, 274),
          labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
        scale_y_continuous(
          name = "Water Level (cm)",
          sec.axis = sec_axis(~ (. - minWL) / 5,
                              name = "Hourly Precip (cm)",
                              breaks = seq(0, 8, by = 2))
        ) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5))
    })
    
    wrap_plots(plot_list, ncol = 1)
  })
  
  output$wl_stats <- DT::renderDataTable({
    req(input$site, input$year)
    
    wl_stats %>%
      filter(site %in% input$site, year %in% input$year) %>%
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Complete Data` = prop_GS_comp,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm
      )
  })
  
  # Reactive: Filtered dataset for brushing
  brushed_data <- reactive({
    req(input$plot_brush)
    
    brushedPoints(
      all_data %>% filter(site %in% c("Gilmore Meadow", input$gm_site),
                          Year %in% input$year,
                          doy > 134, doy < 275),
      input$plot_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    )
  })
  
  output$selected_points <- renderDT({
    req(brushed_data())
    brushed_data() %>%
      select(Date, site, Year, doy, doy_h, water.depth, lag.precip) %>%
      arrange(site, Year, doy_h) %>%
      datatable(options = list(pageLength = 10))
  })
  
  # Download handler
  output$download_points <- downloadHandler(
    filename = function() {
      paste0("selected_points_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(brushed_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)




#----------------------------------------------------------------------------
  



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(shinythemes)
library(shinydashboard)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

gl <- read.csv("data/raw_data/hydrology_data/gilmore_well_prec_data_2013-2024.csv") %>%
  rename(water.depth = GILM_WL) %>%
  mutate(
    site = "Gilmore Meadow",
    timestamp = mdy_hm(timestamp),
    timestamp = if_else(is.na(timestamp), as.POSIXct(mdy(Date)), timestamp),
    timestamp = as.POSIXct(timestamp, tz = "UTC"),
    Date = as.Date(mdy(Date))
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

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
  arrange(site, year)


# UI
ui <- fluidPage(
  theme = shinytheme("flatly"),  # Try "flatly", "cosmo", "cerulean", etc.
  
  tags$head(
    tags$style(HTML("
      .shiny-input-container {
        margin-bottom: 15px;
      }
      .box {
        box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
        border-radius: 10px;
      }
      .box-title {
        font-weight: bold;
        font-size: 18px;
      }
    "))
  ),
  
  titlePanel(tagList(icon("tint"), "Wetland Hydrograph Visualizer")),
  br(),
  
  fluidRow(
    box(width = 3, title = "Hydrograph Controls", solidHeader = TRUE, status = "primary",
        pickerInput("year", "Select Year(s):", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE)),
        selectInput("gm_site", "Select Site to Compare to Gilmore Meadow:", choices = NULL)
    ),
    
    box(width = 9, title = "Hydrograph", solidHeader = TRUE, status = "info",
        plotOutput("hydrograph", height = "450px",
                   brush = brushOpts(id = "plot_brush", direction = "x"))
    )
  ),
  
  fluidRow(
    box(width = 12, title = "Selected Data Points", solidHeader = TRUE, status = "warning",
        tableOutput("brush_info")
    )
  ),
  
  fluidRow(
    box(width = 3, title = "Statistics Controls", solidHeader = TRUE, status = "success",
        pickerInput("site_stats", "Select Site(s) for Stats:", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("year_stats", "Select Year(s) for Stats:", choices = NULL, 
                    multiple = TRUE, options = list(`actions-box` = TRUE))
    ),
    
    box(width = 9, title = "Water Level Statistics", solidHeader = TRUE, status = "success",
        dataTableOutput("wl_stats")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Populate input choices
  observe({
    updatePickerInput(session, "year", choices = sort(unique(all_data$Year)))
    updateSelectInput(session, "gm_site", choices = unique(all_data$site[all_data$site != "Gilmore Meadow"]))
    updatePickerInput(session, "site_stats", choices = unique(all_data$site))
    updatePickerInput(session, "year_stats", choices = sort(unique(all_data$Year)))
  })
  
  # Reactive: Filtered hydrograph data
  plot_data <- reactive({
    req(input$year, input$gm_site)
    all_data %>%
      filter(
        Year %in% input$year,
        doy > 134, doy < 275,
        site %in% c(input$gm_site, "Gilmore Meadow")
      )
  })
  
  # Reactive: Precip offset (scaled to water depth)
  precip_data <- reactive({
    d <- plot_data()
    
    d %>%
      group_by(Year) %>%
      summarise(
        min_water = min(water.depth[!is.na(water.depth)], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(d, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
  })
  
  # Hydrograph Plot
  output$hydrograph <- renderPlot({
    d <- plot_data()
    p <- precip_data()
    req(nrow(d) > 0)
    
    global_min <- min(d$water.depth, na.rm = TRUE)
    
    ggplot() +
      geom_line(data = d %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site),
                size = 0.7) +
      geom_line(data = d %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"),
                size = 0.7) +
      geom_line(data = p,
                aes(x = doy_h, y = precip_y),
                color = "blue", size = 0.7) +
      geom_hline(yintercept = 0, color = "brown") +
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      scale_color_manual(
        values = setNames(
          c("black", "darkgray"),
          c(input$gm_site, "Gilmore Meadow")
        )
      ) +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')
      ) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))
      ) +
      labs(title = "Hydrographs by Year",
           x = "Day of Year",
           y = "Water Level (cm)") +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        strip.text = element_text(size = 11),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  # Table: Selected points via brush
  output$brush_info <- renderTable({
    d <- plot_data()
    brushedPoints(d, input$plot_brush) %>%
      select(site, timestamp, Year, water.depth, lag.precip) %>%
      arrange(timestamp)
  })
  
  # Stats: Calculate water level summaries
  output$wl_stats <- renderDataTable({
    req(input$site_stats, input$year_stats)
    
    stats_data <- all_data %>%
      filter(site %in% input$site_stats, Year %in% input$year_stats)
    
    calc_WL_stats(stats_data)
  })
}


# Run app
shinyApp(ui, server) 



#-------------------------------------------------------------------------------



library(shiny)
library(tidyverse)
library(lubridate)
library(patchwork)
library(shinyWidgets)
library(DT)
library(bslib)

# Read & prepare your processed data (from your current script)
gm <- read.csv("data/processed_data/great_meadow_well_data_2024_20250715.csv") %>%
  rename(Date = date, Year = year, precip_cm = precip.cm) %>%
  mutate(Date = as.Date(Date),
         timestamp = as_datetime(timestamp),
         site = paste("Great Meadow", plot.num),
         water.depth = case_when(
           Year == 2016 & doy_h == 159.12 & plot.num == 3 & water.depth < -120 ~ NA_real_,
           Year == 2017 & doy_h == 215.02 & plot.num == 6 & water.depth < -115 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 224 & water.depth > 400 ~ NA_real_,
           Year == 2021 & plot.num == 3 & doy == 225 & water.depth > 400 ~ NA_real_,
           TRUE ~ water.depth
         )) %>%
  mutate(siteyear = paste(site, Year, sep = "_"))

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
    
    # Standardize Date column too
    Date = mdy(Date)
  ) %>%
  select(timestamp, 
         Date,
         doy,
         Year,
         precip_cm,
         water.depth,
         lag.precip,
         hr,
         doy_h,
         site)

all_data <- bind_rows(gm, gl) %>% 
  filter(Year >= 2016 & Year <= 2023) 

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
  arrange(site, year)


# UI with improved styling
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2C5F41",
    secondary = "#5A9B7C", 
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Roboto", wght = c(400, 700))
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
        border-left: 5px solid #2C5F41;
      }
      
      .section-title {
        color: #2C5F41;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 2px solid #5A9B7C;
      }
      
      .sidebar-custom {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
        border-radius: 10px;
        padding: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        border: 1px solid #dee2e6;
      }
      
      .main-title {
        background: linear-gradient(135deg, #2C5F41 0%, #5A9B7C 100%);
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
        border: 1px solid #5A9B7C;
      }
      
      .stats-main {
        background: linear-gradient(135deg, #f8f9fa 0%, #ffffff 100%);
      }
    "))
  ),
  
  # Main title with gradient background
  div(class = "main-title",
      h1("Wetland Hydrograph Visualizer", 
         style = "margin: 0; font-size: 2.5rem; text-shadow: 2px 2px 4px rgba(0,0,0,0.3)")
  ),
  
  # First section: Hydrograph with improved styling
  div(class = "content-section",
      h2("Wetland Hydrograph Analysis", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Chart Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
          selectInput("gm_site", 
                      label = div(icon("map-marker"), "Select a Great Meadow Site:"),
                      choices = sort(unique(all_data$site[grepl("Great Meadow", all_data$site)])),
                      selected = "Great Meadow 1"),
          
          br(),
          pickerInput("year", 
                      label = div(icon("calendar"), "Select Year(s):"),
                      choices = sort(unique(all_data$Year)),
                      selected = 2023,
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-primary"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #e8f4f8; border-radius: 8px; border-left: 4px solid #3498db;",
              p(icon("info-circle"), " Use the brush tool to select data points on the chart for detailed analysis.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-primary text-white",
            "Hydrographs by Year"
          ),
          plotOutput("hydrograph", height = "600px", 
                     brush = brushOpts(id = "hydro_brush", fill = "#5A9B7C", opacity = 0.3))
        )
      )
  ),
  
  # Selected data points section with improved styling
  div(class = "brush-info-section",
      div(class = "row",
          div(class = "col-12",
              h3("Selected Data Points:", 
                 style = "color: #2C5F41; text-align: center; margin-bottom: 20px;"),
              card(
                card_header(
                  class = "bg-info text-white",
                  "Detailed View of Brushed Data"
                ),
                tableOutput("brush_info")
              )
          )
      )
  ),
  
  # Second section: Water Level Stats with improved styling
  div(class = "content-section stats-main",
      h2("Water Level Statistics", class = "section-title"),
      layout_sidebar(
        sidebar = sidebar(
          class = "sidebar-custom",
          width = 300,
          h4("Statistics Controls", style = "color: #2C5F41; margin-bottom: 20px;"),
          
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
                        style = "btn-outline-success"
                      )),
          
          br(),
          
          pickerInput("stats_year", 
                      label = div(icon("calendar"), "Select Years:"),
                      choices = sort(unique(wl_stats$year)),
                      selected = c(2022, 2023),
                      multiple = TRUE,
                      options = list(
                        `actions-box` = TRUE,
                        `deselect-all-text` = "Clear all",
                        `select-all-text` = "Select all",
                        `none-selected-text` = "Choose year(s)",
                        `live-search` = TRUE,
                        style = "btn-outline-success"
                      )),
          
          br(),
          div(style = "padding: 15px; background-color: #f0f8f0; border-radius: 8px; border-left: 4px solid #27ae60;",
              p(icon("chart-bar"), " Growing Season statistics calculated from May through October.", 
                style = "margin: 0; font-size: 0.9rem; color: #2c3e50;"))
        ),
        
        card(
          full_screen = TRUE,
          card_header(
            class = "bg-success text-white",
            "Growing Season Water Level Statistics"
          ),
          div(style = "padding: 10px;",
              dataTableOutput("wl_stats"))
        )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data for plotting
  plot_data <- reactive({
    req(input$year, input$gm_site)
    
    all_data %>%
      filter(Year %in% input$year, doy > 134, doy < 275) %>%
      filter(site %in% c(input$gm_site, "Gilmore Meadow"))
  })
  
  output$hydrograph <- renderPlot({
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Calculate precipitation offset for each year
    precip_data <- plot_data_filtered %>%
      group_by(Year) %>%
      summarise(
        min_water = min(c(
          water.depth[site == input$gm_site & !is.na(water.depth)],
          water.depth[site == "Gilmore Meadow" & !is.na(water.depth)]
        ), na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      left_join(plot_data_filtered, by = "Year") %>%
      mutate(precip_y = lag.precip * 5 + min_water)
    
    global_min_water <- min(plot_data_filtered$water.depth, na.rm = TRUE)
    
    # Create the plot using facet_wrap instead of patchwork
    ggplot() +
      # Water level lines
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(x = doy_h, y = water.depth, color = input$gm_site), 
                size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(x = doy_h, y = water.depth, color = "Gilmore Meadow"), 
                size = 0.7) +
      # Precipitation line
      geom_line(data = precip_data,
                aes(x = doy_h, y = precip_y, color = "Precipitation"), size = 0.7) +
      # Ground level reference
      geom_hline(yintercept = 0, color = "brown") +
      
      # Facet by year
      facet_wrap(~ Year, ncol = 1, scales = "free_y") +
      
      # Styling
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      labs(title = "Hydrographs by Year",
           x = "Date", y = "Water Level (cm)") +
      scale_x_continuous(
        breaks = c(121, 152, 182, 213, 244, 274),
        labels = c('May-01', 'Jun-01', 'Jul-01', 'Aug-01', 'Sep-01', 'Oct-01')) +
      scale_y_continuous(
        name = "Water Level (cm)",
        sec.axis = sec_axis(~ (. - global_min_water) / 5,
                            name = "Hourly Precip. (cm)",
                            breaks = seq(0, 8, by = 2))) +
      theme_bw() +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            strip.text = element_text(size = 11),
            plot.title = element_text(hjust = 0.5))
  })
  
  # Selected data output table - Now with separate columns for each site
  output$brush_info <- renderTable({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    # Use brushedPoints function which works properly with faceted plots
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water.depth"
    ) %>%
      select(site, Year, doy_h, timestamp, water.depth, precip_cm) %>%
      arrange(Year, doy_h) %>%
      # Round for better display
      mutate(
        doy_h = round(doy_h, 2),
        water.depth = round(water.depth, 2),
        precip_cm = round(precip_cm, 3),
        timestamp = format(timestamp, "%Y-%m-%d %H:%M:%S")
      )
    
    if(nrow(selected_data) == 0) {
      data.frame(Message = "No points selected - try brushing over the water level lines")
    } else {
      # Create a proper time grouping to match data points from both sites
      # Group by rounded doy_h to combine data from both sites at similar times
      pivoted_data <- selected_data %>%
        # Round doy_h slightly to group nearby times together
        mutate(doy_h_rounded = round(doy_h, 1)) %>%
        # Group by the rounded time
        group_by(Year, doy_h_rounded) %>%
        summarise(
          # Use the first timestamp and precipitation value in each group
          timestamp = first(timestamp),
          doy_h = first(doy_h),
          precip_cm = first(precip_cm),
          # Create separate columns for each site's water depth
          great_meadow_depth = ifelse(any(site == input$gm_site), 
                                      water.depth[site == input$gm_site][1], NA),
          gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                                 water.depth[site == "Gilmore Meadow"][1], NA),
          .groups = 'drop'
        ) %>%
        # Remove the grouping column and rename for display
        select(-doy_h_rounded) %>%
        rename(
          `Timestamp` = timestamp,
          `Day of Year` = doy_h,
          `Precipitation (cm)` = precip_cm
        ) %>%
        # Create dynamic column names based on selected site
        rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
        rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
        # Arrange by year and day
        arrange(Year, `Day of Year`)
      
      pivoted_data
    }
  })
  
  # WL stats output table
  output$wl_stats <- DT::renderDataTable({
    req(input$stats_site, input$stats_year)
    
    wl_stats %>%
      filter(site %in% input$stats_site, year %in% input$stats_year) %>% 
      mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
      select(
        Year = year,
        Site = site,
        `Mean Water Level` = WL_mean,
        `SD Water Level` = WL_sd,
        `Minimum Water Level` = WL_min,
        `Maximum Water Level` = WL_max,
        `Maximum Hourly Increase` = max_inc,
        `Maximum Hourly Decrease` = max_dec,
        `Growing Season Change` = GS_change,
        `GS % Surface Water` = prop_over_0cm,
        `GS % Within 30cm` = prop_bet_0_neg30cm,
        `GS % Over 30cm Deep` = prop_under_neg30cm,
        `GS % Complete Data` = prop_GS_comp
      )
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# Run app
shinyApp(ui, server)




