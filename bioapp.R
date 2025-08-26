#### Biodiversity Data ####

library(shiny)
library(dplyr)
library(plotly)


# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


# Example app
ui <- fluidPage(
  fluidRow(
    column(3, plotlyOutput("obs_plot")),
    column(3, plotlyOutput("spp_plot")),
   # column(3, plotlyOutput("id_plot")),
    column(3, plotlyOutput("ppl_plot"))
  )
)

server <- function(input, output, session) {
  
  # ---- Observations ----
  obs_summary <- inat_data %>%
    distinct(uuid, quality_grade) %>%        # unique obs by uuid
    count(quality_grade)
  
  output$obs_plot <- renderPlotly({
    plot_ly(obs_summary, labels = ~quality_grade, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$uuid), big.mark = ","),
                                        "<br>OBSERVATIONS"),
                          x = 0.5, y = 0.5))
  })
  
  # ---- Species ----
  spp_summary <- inat_data %>%
    distinct(taxon_species_name, iconic_taxon_name) %>%  # unique species
    count(iconic_taxon_name)
  
  output$spp_plot <- renderPlotly({
    plot_ly(spp_summary, labels = ~iconic_taxon_name, values = ~n,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_distinct(inat_data$taxon_species_name), big.mark = ","),
                                        "<br>SPECIES"),
                          x = 0.5, y = 0.5))
  })
  
  # # ---- Identifications ----
  # # Assuming you have a column "ident_type"
  # id_summary <- df %>%
  #   count(ident_type)
  # 
  # output$id_plot <- renderPlotly({
  #   plot_ly(id_summary, labels = ~ident_type, values = ~n,
  #           type = "pie", hole = 0.6) %>%
  #     layout(title = list(text = paste0(formatC(sum(id_summary$n), big.mark = ","),
  #                                       "<br>IDENTIFICATIONS"),
  #                         x = 0.5, y = 0.5))
  # })
  
  # ---- People ----
  # Just total unique users for now
  n_people <- n_distinct(inat_data$user_id)
  
  output$ppl_plot <- renderPlotly({
    plot_ly(labels = "People", values = n_people,
            type = "pie", hole = 0.6) %>%
      layout(title = list(text = paste0(formatC(n_people, big.mark = ","),
                                        "<br>PEOPLE"),
                          x = 0.5, y = 0.5),
             showlegend = FALSE)
  })
}

shinyApp(ui, server)






#### DRAFT 2 -------------------------------------------------------------------



library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations card
    card(
      card_header(
        icon("binoculars"), " Observations",
        class = "text-center bg-primary text-white"
      ),
      card_body(
        plotlyOutput("obs_plot", height = "250px"),
        div(id = "obs_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # Species card
    card(
      card_header(
        icon("leaf"), " Species",
        class = "text-center bg-success text-white"
      ),
      card_body(
        plotlyOutput("spp_plot", height = "250px"),
        div(id = "spp_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    ),
    
    # People card
    card(
      card_header(
        icon("users"), " Contributors",
        class = "text-center bg-info text-white"
      ),
      card_body(
        plotlyOutput("ppl_plot", height = "250px"),
        div(id = "ppl_legend", class = "mt-2"),
        class = "p-2"
      ),
      full_screen = TRUE
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43")
  
  # Simplified helper function to create legend HTML
  create_legend <- function(categories, colors) {
    legend_items <- map2_chr(categories, colors, function(cat, col) {
      paste0('<span style="display: inline-block; margin-right: 15px; margin-bottom: 5px;">',
             '<span style="display: inline-block; width: 12px; height: 12px; ',
             'background-color: ', col, '; margin-right: 5px; border-radius: 2px;"></span>',
             '<span style="font-size: 12px;">', cat, '</span></span>')
    })
    paste(legend_items, collapse = "")
  }
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
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
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)








#### DRAFT 3 -------------------------------------------------------------------




library(shiny)
library(bslib)
library(dplyr)
library(plotly)
library(purrr)

# Read & prepare processed data

inat_data <- read.csv("data/raw_data/biodiversity_data/inat_greatmeadow_20250825.csv")


ui <- page_fillable(
  theme = bs_theme(
    bootswatch = "flatly",
    primary = "#2E8B57", 
    base_font = font_google("Open Sans")
  ),
  
  # Header
  div(
    class = "text-center mb-4",
    h1("iNaturalist Biodiversity Dashboard", 
       class = "display-4 text-primary"),
    p("Summary of observations from Great Meadow", 
      class = "lead text-muted")
  ),
  
  # Main content
  layout_columns(
    col_widths = c(4, 4, 4),
    
    # Observations
    div(
      class = "text-center",
      plotlyOutput("obs_plot", height = "300px"),
      div(id = "obs_legend", class = "mt-3")
    ),
    
    # Species
    div(
      class = "text-center",
      plotlyOutput("spp_plot", height = "300px"),
      div(id = "spp_legend", class = "mt-3")
    ),
    
    # People
    div(
      class = "text-center",
      plotlyOutput("ppl_plot", height = "300px"),
      div(id = "ppl_legend", class = "mt-3")
    )
  ),
  
  # Footer
  hr(),
  div(
    class = "text-center text-muted small mt-3",
    p("Data sourced from iNaturalist • Last updated: ", Sys.Date())
  )
)

server <- function(input, output, session) {
  
  # Define color palettes
  quality_colors <- c("research" = "#28a745", "needs_id" = "#ffc107", "casual" = "#6c757d")
  taxon_colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4", "#FECA57", 
                    "#FF9FF3", "#54A0FF", "#5F27CD", "#00D2D3", "#FF9F43",
                    "#A8E6CF", "#FFB347", "#87CEEB", "#DDA0DD", "#F0E68C")
  
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
  
  # ---- Observations ----
  obs_summary <- reactive({
    inat_data %>%
      distinct(uuid, quality_grade) %>%
      count(quality_grade) %>%
      mutate(
        percentage = round(n/sum(n) * 100, 1)
      )
  })
  
  output$obs_plot <- renderPlotly({
    data <- obs_summary()
    total_obs <- sum(data$n)
    
    plot_ly(data, 
            labels = ~quality_grade, 
            values = ~n,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = quality_colors[data$quality_grade],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_obs, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Observations</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for observations
  observe({
    data <- obs_summary()
    legend_html <- create_legend(data$quality_grade, quality_colors[data$quality_grade])
    insertUI(selector = "#obs_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- Species ----
  spp_summary <- reactive({
    inat_data %>%
      distinct(taxon_species_name, iconic_taxon_name) %>%
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
            hole = 0.6,
            marker = list(colors = taxon_colors[1:nrow(data)],
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>%{label}</b><br>Count: %{value} (%{percent})<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(total_spp, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>Species</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for species
  observe({
    data <- spp_summary()
    legend_html <- create_legend(data$iconic_taxon_name, taxon_colors[1:nrow(data)])
    insertUI(selector = "#spp_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
  
  # ---- People ----
  output$ppl_plot <- renderPlotly({
    n_people <- n_distinct(inat_data$user_id)
    
    plot_ly(labels = "Contributors", 
            values = n_people,
            type = "pie", 
            hole = 0.6,
            marker = list(colors = "#17a2b8",
                          line = list(color = "white", width = 2)),
            textinfo = "none",
            hovertemplate = "<b>Contributors</b><br>Count: %{value}<extra></extra>") %>%
      layout(
        showlegend = FALSE,
        margin = list(t = 20, b = 20, l = 20, r = 20),
        annotations = list(
          list(x = 0.5, y = 0.5, 
               text = paste0("<b>", formatC(n_people, big.mark = ","), "</b><br>",
                             "<span style='font-size:12px'>People</span>"),
               showarrow = FALSE,
               font = list(size = 16))
        )
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Create legend for people (simple single item)
  observe({
    legend_html <- create_legend("Contributors", "#17a2b8")
    insertUI(selector = "#ppl_legend", 
             ui = div(HTML(legend_html), class = "text-center"),
             immediate = TRUE)
  })
}

shinyApp(ui, server)
