###############################################
# 05_visualization.R
# Objective: Create key visual insights
###############################################

library(tidyverse)
library(ggplot2)
library(reshape2)

# ==== Create visualization folder if it doesn't exist ====
vis_dir <- "/workspaces/DS520-Team-Project/visualizations"
if (!dir.exists(vis_dir)) {
  dir.create(vis_dir, recursive = TRUE)
}

# ==== Load Data ====
skills <- read_csv("/workspaces/DS520-Team-Project/data/processed/top_skills.csv")
demand_supply <- read_csv("/workspaces/DS520-Team-Project/data/processed/demand_supply_gap.csv")

# ==== Top 15 Skills ====
top_skills_plot <- skills %>%
  slice_max(n, n = 15) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 15 In-demand Skills",
    x = "Skills",
    y = "Frequency"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(vis_dir, "top_skills_plot.png"),
  plot = top_skills_plot,
  width = 8,
  height = 6,
  dpi = 300
)

# ==== Heatmap: Demand vs Supply ====
demand_matrix <- demand_supply %>%
  select(word, demand, available) %>%
  melt(id.vars = "word")

heatmap_plot <- ggplot(demand_matrix, aes(x = variable, y = reorder(word, value), fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "red") +
  labs(
    title = "Demand vs Supply Heatmap",
    x = "",
    y = "Skills"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(vis_dir, "heatmap.png"),
  plot = heatmap_plot,
  width = 8,
  height = 6,
  dpi = 300
)

message("Visualization completed successfully!")