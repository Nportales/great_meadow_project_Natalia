#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-4",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c(    
    # Primary naturalist colors
    "#2E8B57", "#1ABC9C","#556B2F", "#4A90E2", "#8E44AD", 
    "#F39C12", "#FF6B35","#E74C3C", "#3498DB", "#9B59B6",
    
    # Secondary vibrant colors
    "#FF5733", "#33FF57", "#3357FF", "#FF33F5", "#F5FF33",
    "#33FFF5", "#F533FF", "#57FF33", "#FF3357", "#5733FF",
    
    # Earthy/natural tones
    "#8B4513", "#2F4F4F", "#27AE60", "#8B008B", "#FF4500",
    "#32CD32", "#FFD700", "#DC143C", "#00CED1", "#9932CC",
    
    # Pastel variations
    "#FFB6C1", "#98FB98", "#87CEEB", "#DDA0DD", "#F0E68C",
    "#AFEEEE", "#DB7093", "#90EE90", "#FFA07A", "#20B2AA",
    
    # Additional distinct colors
    "#CD853F", "#4682B4", "#D2691E", "#B0C4DE", "#F4A460",
    "#6495ED", "#DEB887", "#5F9EA0", "#A0522D", "#2E8B57",
    
    # Final set for very large datasets
    "#7B68EE", "#FA8072", "#FFA500", "#32CD32", "#FF69B4",
    "#00FF7F", "#FF1493", "#1E90FF", "#FFD700", "#ADFF2F"
  )
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    merged_data %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    legend_html <- create_legend(data$SPECIES_GROUP, taxon_colors[1:nrow(data)])
    insertUI(selector = "#ebird_spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- iNaturalist stats ----
  inat_summary <- reactive({
    list(
      n_observations = n_distinct(inat_data$uuid),
      n_people = n_distinct(inat_data$user_id)
    )
  })
  
  output$inat_observations <- renderText({
    formatC(inat_summary()$n_observations, big.mark = ",")
  })
  
  output$inat_people <- renderText({
    formatC(inat_summary()$n_people, big.mark = ",")
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_checklists = n_distinct(eBird_data$SAMPLING.EVENT.IDENTIFIER),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_checklists <- renderText({
    formatC(ebird_summary()$n_checklists, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)














#### Biodiversity Data ####

#### iNat and ebird data -------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

# iNat data
inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")

# eBird data
eBird_data <- read.csv("data/raw_data/biodiversity_data/ebird_greatmeadow_20250825.csv")

eBird_tax <- read.csv("data/raw_data/biodiversity_data/eBird_taxonomy_v2024.csv")

# merge eBird data
merged_data <- full_join(
  eBird_data, 
  eBird_tax, 
  by = c("TAXONOMIC.ORDER" = "TAXON_ORDER")
) %>% 
  filter(!is.na(SCIENTIFIC.NAME))


#UI
ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Page Header
  div(
    class = "text-center mb-4",
    h2("Biodiversity of Great Meadow", class = "display-4 text-primary"),
    p("Summary of participatory science observations", class = "lead text-muted")
  ),
  
  #### iNaturalist Section ####
  div(class = "my-5",
      h3("iNaturalist", class = "text-center text-success mb-4"),
      
      # Single pie chart for species by taxa
      div(class = "text-center mb-4",
          plotlyOutput("spp_plot", height = "400px"),
          div(id = "spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("binoculars", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("inat_observations"), class = "mb-1 text-primary"),
              div("Observations", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("inat_people"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  #### eBird Section ####
  div(class = "my-5",
      h2("eBird", class = "text-center text-primary mb-4"),
      
      # Single pie chart for species by taxonomic group
      div(class = "text-center mb-4",
          plotlyOutput("ebird_spp_plot", height = "400px"),
          div(id = "ebird_spp_legend", class = "mt-3")
      ),
      
      # Stats with icons underneath
      layout_columns(
        col_widths = c(6, 6),
        
        card(
          class = "text-center",
          card_body(
            div(
              icon("clipboard-list", style = "font-size: 2rem; color: #28a745; margin-bottom: 10px;"),
              h3(textOutput("ebird_checklists"), class = "mb-1 text-primary"),
              div("Checklists", class = "text-muted")
            )
          )
        ),
        
        card(
          class = "text-center", 
          card_body(
            div(
              icon("users", style = "font-size: 2rem; color: #17a2b8; margin-bottom: 10px;"),
              h3(textOutput("ebird_observers"), class = "mb-1 text-info"),
              div("Observers", class = "text-muted")
            )
          )
        )
      )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist and eBird • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  taxon_colors <- c(    
    # Primary naturalist colors
    
    
    "#556B2F","#1ABC9C", "#90EE90","#4682B4", "#3498DB",
    "#1E90FF", "#3357FF","#F39C12", "#FF6B35","#E74C3C", "#8B4513",
    "#8B008B", "#8E44AD", "#9932CC", "#DDA0DD","#F533FF","#FF33F5", 
    "#FF1493", "#FF69B4", "#FF3357", "#DC143C",
    
    "#4A90E2", "#8E44AD", 
    "#F39C12", "#FF6B35","#E74C3C", "#3498DB", "#9B59B6",
    "#2E8B57","#27AE60",
    
    # Secondary vibrant colors
    "#FF5733", "#33FF57", "#3357FF", "#FF33F5", "#F5FF33",
    "#33FFF5", "#F533FF", "#57FF33", "#FF3357", "#5733FF",
    
    # Earthy/natural tones
    "#8B4513", "#2F4F4F", "#27AE60", "#8B008B", "#FF4500",
    "#32CD32", "#FFD700", "#DC143C", "#00CED1", "#9932CC",
    
    # Pastel variations
    "#FFB6C1", "#98FB98", "#87CEEB", "#DDA0DD", "#F0E68C",
    "#AFEEEE", "#DB7093", "#90EE90", "#FFA07A", "#20B2AA",
    
    # Additional distinct colors
    "#CD853F", "#4682B4", "#D2691E", "#B0C4DE", "#F4A460",
    "#6495ED", "#DEB887", "#5F9EA0", "#A0522D", "#2E8B57",
    
    # Final set for very large datasets
    "#7B68EE", "#FA8072", "#FFA500", "#32CD32", "#FF69B4",
    "#00FF7F", "#FF1493", "#1E90FF", "#FFD700", "#ADFF2F"
  )
  
  # Helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Species pie chart for iNaturalist ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
      filter(!is.na(iconic_taxon_name), iconic_taxon_name != "") %>%
      count(iconic_taxon_name) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$spp_plot <- renderPlotly({
    data <- spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~iconic_taxon_name, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for iNat species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- eBird species pie chart ----
  ebird_spp_summary <- reactive({
    merged_data %>%
      distinct(SCIENTIFIC.NAME, SPECIES_GROUP) %>%
      count(SPECIES_GROUP) %>%
      arrange(desc(n)) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$ebird_spp_plot <- renderPlotly({
    data <- ebird_spp_summary()
    total_spp <- sum(data$n)
    
    plot_ly(data, 
            labels = ~SPECIES_GROUP, 
            values = ~n,
            type = "pie", 
            hole = 0.4,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Species: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 40, b = 40, l = 40, r = 40),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:14px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 18))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for eBird species
  observe({
    data <- ebird_spp_summary()
    legend_html <- create_legend(data$SPECIES_GROUP, taxon_colors[1:nrow(data)])
    insertUI(selector = "#ebird_spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- iNaturalist stats ----
  inat_summary <- reactive({
    list(
      n_observations = n_distinct(inat_data$uuid),
      n_people = n_distinct(inat_data$user_id)
    )
  })
  
  output$inat_observations <- renderText({
    formatC(inat_summary()$n_observations, big.mark = ",")
  })
  
  output$inat_people <- renderText({
    formatC(inat_summary()$n_people, big.mark = ",")
  })
  
  ## ---- eBird summaries ----
  ebird_summary <- reactive({
    list(
      n_checklists = n_distinct(eBird_data$SAMPLING.EVENT.IDENTIFIER),
      n_observers = n_distinct(eBird_data$OBSERVER.ID)
    )
  })
  
  output$ebird_checklists <- renderText({
    formatC(ebird_summary()$n_checklists, big.mark = ",")
  })
  
  output$ebird_observers <- renderText({
    formatC(ebird_summary()$n_observers, big.mark = ",")
  })
}

shinyApp(ui, server)

