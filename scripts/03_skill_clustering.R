###############################################
# 03_skill_clustering_refactored.R
# Objective: Cluster related words/skills using TF-IDF + K-means
###############################################

# ==== Load Libraries ====
library(tidyverse)
library(text2vec)
library(cluster)
library(factoextra)
library(ggplot2)
library(readr)

# ==== Load Data ====
skills <- read_csv("/workspaces/DS520-Team-Project/data/processed/top_skills.csv", show_col_types = FALSE)

# Use 'word' column instead of 'skill'
skills <- skills %>% filter(!is.na(word))

# ==== TF-IDF Vectorization ====
# Create iterator over words
it <- itoken(skills$word,
             preprocessor = tolower,
             tokenizer = word_tokenizer,
             progressbar = FALSE)

# Build vocabulary
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)

# Create TF-IDF matrix
dtm <- create_dtm(it, vectorizer)
tfidf <- TfIdf$new()
tfidf_matrix <- fit_transform(dtm, tfidf)

# ==== Determine Optimal Number of Clusters ====
# We'll limit clusters to between 2 and 10
wss <- vector()
for (k in 2:10) {
  km <- kmeans(tfidf_matrix, centers = k, nstart = 10)
  wss[k] <- km$tot.withinss
}

# Plot elbow curve (optional)
png("/workspaces/DS520-Team-Project/visualizations/elbow_plot.png", width=800, height=600)
plot(2:10, wss[2:10], type="b",
     xlab="Number of Clusters",
     ylab="Within-Cluster Sum of Squares",
     main="Elbow Method for Optimal k")
dev.off()

# ==== Final K-Means Clustering ====
# Choose 5 clusters by default (or adjust based on elbow plot)
set.seed(42)
k <- 5
kmeans_result <- kmeans(tfidf_matrix, centers = k, nstart = 25)

# Add cluster labels to skills
skills$cluster <- as.factor(kmeans_result$cluster)

# ==== Save Results ====
write_csv(skills, "/workspaces/DS520-Team-Project/data/processed/skill_clusters1.csv")

# ==== Visualize Clusters (Optional Debug) ====
fviz_cluster(list(data = tfidf_matrix, cluster = kmeans_result$cluster)) +
  ggtitle("Skill Clusters Visualization")

cat("Clustering complete! Results saved to: data/processed/skill_clusters.csv\n")