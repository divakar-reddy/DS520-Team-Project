###############################################
# 03_skill_clustering_refactored.R
# Objective: Cluster related skills using K-means
# Works with 1D frequency data
###############################################

library(tidyverse)
library(cluster)
library(ggplot2)

# ==== Load Top Skills ====
skills <- read_csv("/workspaces/DS520-Team-Project/data/processed/top_skills.csv",
                   show_col_types = FALSE)

# ==== Convert to Matrix for Clustering ====
# Use numeric columns only (here 'n')
skills_matrix <- skills %>%
  column_to_rownames("word") %>%
  as.matrix()

# ==== K-means Clustering ====
set.seed(123)
kmeans_model <- kmeans(skills_matrix, centers = 5, nstart = 20)

# Add cluster assignment to the original data
skills$cluster <- as.factor(kmeans_model$cluster)

# ==== Save Clustered Skills ====
write_csv(skills, "/workspaces/DS520-Team-Project/data/processed/skill_clusters.csv")
cat("Skill clusters saved to skill_clusters.csv\n")

# ==== Visualize Clusters with Bar Plot ====
# Each skill colored by cluster
# Save to PNG
ggsave("/workspaces/DS520-Team-Project/visualizations/skill_clusters_bar.png",
       width = 8, height = 6)

# Display interactively
ggplot(skills, aes(x = reorder(word, n), y = n, fill = cluster)) +
  geom_col() +
  coord_flip() +
  labs(title = "Skill Clusters (K-means)", x = "Skill", y = "Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

dev.off()
cat("Bar plot saved to visualizations/skill_clusters_bar.png\n")