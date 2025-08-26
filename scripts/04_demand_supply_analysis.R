###############################################
# 04_demand_supply_analysis.R
# Objective: Analyze demand vs supply of skills
###############################################

library(tidyverse)

# ==== Load Processed Data ====
skills <- read_csv("/workspaces/DS520-Team-Project/data/processed/top_skills.csv")
clusters <- read_csv("/workspaces/DS520-Team-Project/data/processed/skill_clusters1.csv")

# ==== Hypothetical Supply Data (Survey / Census) ====
supply_data <- tibble(
  skill = skills$word,
  available = round(skills$n * runif(nrow(skills), 0.4, 1.2))
)

# ==== Merge Demand & Supply ====
demand_supply <- skills %>%
  rename(demand = n) %>%
  inner_join(supply_data, by = c("word" = "skill")) %>%
  mutate(gap = demand - available)

# ==== Save Gap Analysis ====
write_csv(demand_supply, "/workspaces/DS520-Team-Project/data/processed/demand_supply_gap.csv")

message("Demand-Supply Gap Analysis completed successfully!")