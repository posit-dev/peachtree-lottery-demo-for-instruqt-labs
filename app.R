library(shiny)
library(bslib)
library(tidyverse)
library(leaflet)
library(sf)
library(tigris)
library(plotly)

georgia_lottery <- readRDS("georgia_lottery.RDS")
source("create_lottery_map.R")

# Brand green from dashboard-theme.css
brand_green <- "#9fbe93"

app_theme <- bs_theme(bootswatch = "cosmo") |>
  bs_add_rules("
    .navbar, .quarto-dashboard .navbar {
      background-color: #9fbe93 !important;
      border-color: #9fbe93 !important;
    }
    .navbar-brand, .navbar .navbar-brand, .navbar-title {
      color: #ffffff !important;
    }
    .card-header, .quarto-dashboard .card-header {
      background-color: #9fbe93 !important;
      color: #ffffff !important;
      border-color: #9fbe93 !important;
    }
  ")

ui <- page_sidebar(
  title = "2025 Georgia Lottery Sales",
  theme = app_theme,

  sidebar = sidebar(
    width = 230,
    selectInput(
      "year", "Year",
      choices = sort(unique(georgia_lottery$year)),
      selected = max(georgia_lottery$year),
      multiple = TRUE
    ),
    selectInput(
      "sex", "Sex",
      choices = c("All", sort(unique(georgia_lottery$sex))),
      selected = "All",
      multiple = TRUE
    ),
    selectInput(
      "race", "Race / Ethnicity",
      choices = c("All", sort(unique(georgia_lottery$race_ethnicity))),
      selected = "All",
      multiple = TRUE
    ),
    selectInput(
      "income", "Income Level",
      choices = c("All", sort(unique(georgia_lottery$income_level))),
      selected = "All",
      multiple = TRUE
    ),
    selectInput(
      "age", "Age Group",
      choices = c("All", sort(unique(georgia_lottery$age_group))),
      selected = "All",
      multiple = TRUE
    )
  ),

  layout_columns(
    col_widths = c(6, 6),

    # Left column — map
    card(
      full_screen = TRUE,
      card_header("Per Capita Lottery Sales"),
      leafletOutput("map", height = "620px")
    ),

    # Right column — two rows of two charts
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Lottery Spend by Sex"),
        plotlyOutput("sex_plot", height = "270px")
      ),
      card(
        card_header("Lottery Spend by Race/Ethnicity"),
        plotlyOutput("race_plot", height = "270px")
      ),
      card(
        card_header("Lottery Spend by Age Group"),
        plotOutput("age_plot", height = "270px")
      ),
      card(
        card_header("Lottery Spend by Income Level"),
        plotOutput("income_plot", height = "270px")
      )
    )
  )
)

server <- function(input, output, session) {

  filtered_data <- reactive({
    df <- georgia_lottery

    if (length(input$year) > 0) {
      df <- df |> filter(year %in% as.integer(input$year))
    }

    if (length(input$sex) > 0 && !("All" %in% input$sex)) {
      df <- df |> filter(sex %in% input$sex)
    }

    if (length(input$race) > 0 && !("All" %in% input$race)) {
      df <- df |> filter(race_ethnicity %in% input$race)
    }

    if (length(input$income) > 0 && !("All" %in% input$income)) {
      df <- df |> filter(income_level %in% input$income)
    }

    if (length(input$age) > 0 && !("All" %in% input$age)) {
      df <- df |> filter(age_group %in% input$age)
    }

    df
  })

  output$map <- renderLeaflet({
    create_lottery_map(filtered_data(), per_capita = TRUE)
  })

  output$sex_plot <- renderPlotly({
    sex_totals <- filtered_data() |>
      group_by(sex) |>
      summarise(spend = sum(lottery_spend, na.rm = TRUE), .groups = "drop")

    plot_ly(
      sex_totals,
      labels = ~sex,
      values = ~spend,
      type = "pie",
      hole = 0.5,
      marker = list(
        colors = c("#4393c3", "#d6604d"),
        line = list(color = "white", width = 2)
      ),
      textinfo = "label+percent",
      textfont = list(size = 14, color = "white")
    ) |>
      layout(
        showlegend = FALSE,
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )
  })

  output$race_plot <- renderPlotly({
    race_totals <- filtered_data() |>
      group_by(race_ethnicity) |>
      summarise(spend = sum(lottery_spend, na.rm = TRUE), .groups = "drop")

    race_colors <- c(
      "#4393c3", "#d6604d", "#74c476", "#9e9ac8",
      "#fdae6b", "#41ab5d", "#f4a582"
    )

    plot_ly(
      race_totals,
      labels = ~race_ethnicity,
      values = ~spend,
      type = "pie",
      hole = 0.5,
      marker = list(
        colors = race_colors,
        line = list(color = "white", width = 2)
      ),
      textinfo = "label+percent",
      textfont = list(size = 12, color = "white")
    ) |>
      layout(
        showlegend = FALSE,
        margin = list(t = 0, b = 0, l = 0, r = 0)
      )
  })

  output$age_plot <- renderPlot({
    age_totals <- filtered_data() |>
      group_by(age_group) |>
      summarise(spend = sum(lottery_spend, na.rm = TRUE), .groups = "drop")

    ggplot(age_totals, aes(x = age_group, y = spend / 1e6, fill = age_group)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(
        aes(label = paste0("$", round(spend / 1e6, 1), "M")),
        vjust = -0.4, size = 3.5, fontface = "bold"
      ) +
      scale_fill_brewer(palette = "Blues", direction = 1) +
      scale_y_continuous(
        labels = scales::dollar_format(suffix = "M"),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(x = "Age group", y = "Spend (millions)") +
      theme_minimal(base_size = 13) +
      theme(panel.grid.major.x = element_blank())
  })

  output$income_plot <- renderPlot({
    income_totals <- filtered_data() |>
      group_by(income_level) |>
      summarise(spend = sum(lottery_spend, na.rm = TRUE), .groups = "drop")

    ggplot(income_totals, aes(x = income_level, y = spend / 1e6, fill = income_level)) +
      geom_col(show.legend = FALSE, width = 0.7) +
      geom_text(
        aes(label = paste0("$", round(spend / 1e6, 1), "M")),
        vjust = -0.4, size = 3.5, fontface = "bold"
      ) +
      scale_fill_brewer(palette = "Oranges", direction = 1) +
      scale_y_continuous(
        labels = scales::dollar_format(suffix = "M"),
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(x = "Income level", y = "Spend (millions)") +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )
  })
}

shinyApp(ui, server)
