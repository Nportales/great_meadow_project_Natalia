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
         precip_cm,
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
  arrange(site, year) %>% 
  mutate(
    wetland = case_when(
      grepl("Great Meadow", site, ignore.case = TRUE) ~ "Great Meadow",
      grepl("Gilmore Meadow", site, ignore.case = TRUE) ~ "Gilmore Meadow",
      TRUE ~ "Unknown"
    )
  )


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
                      selected = c(2023, 2022, 2021, 2020),
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
              radioButtons("time_summary", "Summarize Stats:",
                           choices = c("Each Year" = "year", "Average Across Years" = "multi"),
                           selected = "year",
                           inline = TRUE)),
          
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
    
    # Calculate minimum water level across all data for consistent precipitation baseline
    minWL <- min(plot_data_filtered$water_depth, na.rm = TRUE)
    
    # Create the plot using your simplified approach
    ggplot(plot_data_filtered, aes(x = doy_h, y = water_depth)) +
      # Water level lines by site
      geom_line(data = plot_data_filtered %>% filter(site == input$gm_site),
                aes(color = input$gm_site), size = 0.7) +
      geom_line(data = plot_data_filtered %>% filter(site == "Gilmore Meadow"),
                aes(color = "Gilmore Meadow"), size = 0.7) +
      
      # Precipitation line (scaled by multiplier of 5 and offset by minWL)
      geom_line(aes(x = doy_h, y = lag_precip * 5 + minWL, color = "Precipitation"), 
                size = 0.7) +
      
      # Ground level reference
      geom_hline(yintercept = 0, color = 'brown') +
      
      # Facet by year
      facet_wrap(~ year, ncol = 1, scales = "free_y") +
      
      # Colors
      scale_color_manual(values = c(
        setNames("black", input$gm_site),
        "Gilmore Meadow" = "darkgray",
        "Precipitation" = "blue"
      )) +
      
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
  })
  
  # Reactive for processed brushed data
  processed_brush_data <- reactive({
    req(input$hydro_brush)
    
    plot_data_filtered <- plot_data()
    req(nrow(plot_data_filtered) > 0)
    
    selected_data <- brushedPoints(
      plot_data_filtered,
      brush = input$hydro_brush,
      xvar = "doy_h",
      yvar = "water_depth"
    ) %>%
      select(site, year, doy_h, timestamp, water_depth, lag_precip) %>%
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
    
    selected_data %>%
      mutate(doy_h_rounded = round(doy_h, 1)) %>%
      group_by(year, doy_h_rounded) %>%
      summarise(
        timestamp = first(timestamp),
        doy_h = first(doy_h),
        lag_precip = first(lag_precip),
        great_meadow_depth = ifelse(any(site == input$gm_site), 
                                    water_depth[site == input$gm_site][1], NA),
        gilmore_depth = ifelse(any(site == "Gilmore Meadow"), 
                               water_depth[site == "Gilmore Meadow"][1], NA),
        .groups = 'drop'
      ) %>%
      select(-doy_h_rounded) %>%
      rename(
        `Year` = year, 
        `Timestamp` = timestamp,
        `Day of Year` = doy_h,
        `Precipitation (cm)` = lag_precip
      ) %>%
      rename_with(~ paste(input$gm_site, "Water Depth (cm)"), .cols = great_meadow_depth) %>%
      rename_with(~ "Gilmore Meadow Water Depth (cm)", .cols = gilmore_depth) %>%
      arrange(Year, `Day of Year`)
  })
  
  
  # setting up average WL stats and calculating stats output tables
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
    } else {
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
        )
      
      # Per-wetland "All Sites" summary
      data_wetland <- data %>%
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
        )
      
      bind_rows(data_site, data_wetland) %>%
        distinct() %>% 
        rename(Site = site,
               Wetland = wetland)
    }
  })
  
  # reactive table output for selected data from hydrograph
  output$brush_info <- renderTable({
    processed_brush_data()
  })
  
  # reactive table output for WL stats 
  output$wl_stats <- DT::renderDataTable({
    datatable(
      filtered_stats(),
      options = list(pageLength = 10, scrollX = TRUE)
    ) %>%
      formatStyle(
        'Site',
        target = 'cell',
        fontWeight = styleEqual("All Sites", "bold")
      )
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
      } else {
        paste("site_stats_", Sys.Date(), ".csv", sep = "")
      }
    },
    content = function(file) {
      write.csv(filtered_stats(), file, row.names = FALSE)
    }
  )
  
}

# Run app
shinyApp(ui, server)


