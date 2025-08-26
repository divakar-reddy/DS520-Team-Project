###############################################
# 06_shiny_dashboard.R
# Interactive dashboard for visualizing skill demand
###############################################

# ==== Load Libraries ====
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)

# ==== Load Data ====
skills_df <- read_csv("/workspaces/DS520-Team-Project/data/processed/top_skills.csv", show_col_types = FALSE)

# Count frequency of each skill
top_skills <- skills_df %>%
  select(word, n) %>%  # n will be the frequency
  arrange(desc(n))

# Load Skill Clusters
clusters_path <- "/workspaces/DS520-Team-Project/data/processed/skill_clusters1.csv"
clusters_df <- if (file.exists(clusters_path)) {
  read_csv(clusters_path, show_col_types = FALSE)
} else {
  NULL
}

# Load Demand-Supply Gap
demand_gap_path <- "/workspaces/DS520-Team-Project/data/processed/demand_supply_gap.csv"
demand_gap_df <- if (file.exists(demand_gap_path)) {
  read_csv(demand_gap_path, show_col_types = FALSE)
} else {
  NULL
}

# ==== UI ====
ui <- dashboardPage(
  dashboardHeader(title = "Skill Demand Dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Top Skills", tabName = "top_skills", icon = icon("chart-bar")),
      menuItem("Skill Clusters", tabName = "clusters", icon = icon("project-diagram")),
      menuItem("Demand-Supply Gap", tabName = "demand_gap", icon = icon("balance-scale")),
      menuItem("Raw Data", tabName = "data", icon = icon("table"))
    )
  ),

  dashboardBody(
    tabItems(
      # ---- Tab 1: Top Skills ----
      tabItem(tabName = "top_skills",
              fluidRow(
                box(width = 12, title = "Top 15 Skills by Demand", status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_top_skills", height = "450px"))
              ),
              fluidRow(
                box(width = 12, title = "Complete Skills Table", status = "info", solidHeader = TRUE,
                    DTOutput("skills_table"))
              )
      ),

      # ---- Tab 2: Skill Clusters ----
      tabItem(tabName = "clusters",
              conditionalPanel(
                condition = "output.has_clusters",
                fluidRow(
                  box(width = 12, title = "Skill Clusters Visualization", status = "success", solidHeader = TRUE,
                      plotlyOutput("plot_clusters", height = "450px"))
                ),
                fluidRow(
                  box(width = 12, title = "Clustered Skills Table", status = "info", solidHeader = TRUE,
                      DTOutput("clusters_table"))
                )
              ),
              conditionalPanel(
                condition = "!output.has_clusters",
                h3("Clustering results not found. Please run 03_skill_clustering.R first.")
              )
      ),

      # ---- Tab 3: Demand-Supply Gap ----
      tabItem(tabName = "demand_gap",
              conditionalPanel(
                condition = "output.has_demand_gap",
                fluidRow(
                  box(width = 12, title = "Skills Demand vs Supply Gap", status = "danger", solidHeader = TRUE,
                      plotlyOutput("plot_demand_gap", height = "450px"))
                ),
                fluidRow(
                  box(width = 12, title = "Demand-Supply Table", status = "info", solidHeader = TRUE,
                      DTOutput("demand_gap_table"))
                )
              ),
              conditionalPanel(
                condition = "!output.has_demand_gap",
                h3("Demand-Supply data not found.")
              )
      ),

      # ---- Tab 4: Raw Data ----
      tabItem(tabName = "data",
              fluidRow(
                box(width = 12, title = "Raw Extracted Skills Data", status = "primary", solidHeader = TRUE,
                    DTOutput("raw_data"))
              )
      )
    )
  )
)

# ==== Server ====
server <- function(input, output, session) {

# ---- Top Skills Plot ----
output$plot_top_skills <- renderPlotly({
  top_15 <- top_skills %>% slice(1:15)

  p <- ggplot(top_15, aes(x = reorder(word, n), y = n)) +
    geom_col(fill = "#2E86AB") +
    coord_flip() +
    labs(title = "Top 15 Skills in Demand", x = "Skill", y = "Frequency") +
    theme_minimal()

  ggplotly(p)
})

# ---- Top Skills Table ----
output$skills_table <- renderDT({
  datatable(top_skills, options = list(pageLength = 15))
})

  # ---- Skill Clusters ----
  output$has_clusters <- reactive({ !is.null(clusters_df) })
  outputOptions(output, "has_clusters", suspendWhenHidden = FALSE)

  output$plot_clusters <- renderPlotly({
    req(clusters_df)
    p <- ggplot(clusters_df, aes(x = cluster, fill = cluster)) +
      geom_bar() +
      labs(title = "Number of Skills per Cluster", x = "Cluster", y = "Count") +
      theme_minimal()
    ggplotly(p)
  })

  output$clusters_table <- renderDT({
    req(clusters_df)
    datatable(clusters_df, options = list(pageLength = 15))
  })

  # ---- Demand-Supply Gap ----
  output$has_demand_gap <- reactive({ !is.null(demand_gap_df) })
  outputOptions(output, "has_demand_gap", suspendWhenHidden = FALSE)

  output$plot_demand_gap <- renderPlotly({
    req(demand_gap_df)
    p <- ggplot(demand_gap_df, aes(x = reorder(word, gap), y = gap)) +
      geom_col(fill = "#E74C3C") +
      coord_flip() +
      labs(title = "Demand-Supply Gap per Skill", x = "Skill", y = "Gap") +
      theme_minimal()
    ggplotly(p)
  })

  output$demand_gap_table <- renderDT({
    req(demand_gap_df)
    datatable(demand_gap_df, options = list(pageLength = 15))
  })

  # ---- Raw Data Table ----
  output$raw_data <- renderDT({
    datatable(skills_df, options = list(pageLength = 15))
  })
}

# ==== Run App ====
shinyApp(ui, server)