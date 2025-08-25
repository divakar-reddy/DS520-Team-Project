###############################################
# 02_skill_extraction_refactored.R
# Nationwide Skill Gap Mapping Tool
# Memory-efficient skill extraction
###############################################

# ==== Load Required Libraries ====
library(data.table)  # faster CSV reading and processing
library(dplyr)
library(tidytext)
library(stringr)
library(ggplot2)

# ==== Define Parameters ====
input_file <- "/workspaces/DS520-Team-Project/data/raw/postings.csv"
output_file <- "/workspaces/DS520-Team-Project/data/processed/top_skills.csv"
chunk_size <- 5000  # number of rows to process at a time
plot_output_file <- "top_skills_plot.png"

# ==== Define Custom Skill Dictionary ====
skills_list <- c(
  "python", "r", "sql", "java", "c++", "aws", "azure", "docker", "kubernetes",
  "hadoop", "spark", "tensorflow", "pytorch", "machine learning",
  "deep learning",
  "nlp", "data analysis", "data visualization", "tableau", "powerbi", "excel",
  "cloud computing", "devops", "git"
)
skills_set <- unique(skills_list)

# ==== Initialize Empty Data Table for Aggregation ====
skills_count <- data.table(word = character(), n = integer())

# ==== Process CSV in Chunks ====
fread_chunk <- function(file, nrows, skip = 0) {
  fread(file, nrows = nrows, skip = skip,
        encoding = "UTF-8", showProgress = FALSE)
}

# Get total rows in CSV
total_rows <- as.integer(system(paste("wc -l", input_file,
                                      "| awk '{print $1}'"), intern = TRUE)) - 1
cat("Total rows in CSV:", total_rows, "\n")

start_row <- 0
while (start_row < total_rows) {
  df_chunk <- fread_chunk(input_file, nrows = chunk_size, skip = start_row)
  # Ensure 'description' column exists
  if (!"description" %in% colnames(df_chunk)) {
    warning("Column 'description' not found in chunk 
            starting at row ", start_row)
    start_row <- start_row + chunk_size
    next
  }
  # Clean and tokenize
  df_clean <- df_chunk %>%
    filter(!is.na(description)) %>%
    mutate(description = str_to_lower(description))
  tokens <- df_clean %>% unnest_tokens(word, description)
  # Remove stop words
  data("stop_words")
  tokens_clean <- tokens %>% anti_join(stop_words, by = "word")
  # Match skills
  skills_df <- tokens_clean %>% filter(word %in% skills_set)
  # Count skills in this chunk
  chunk_count <- skills_df %>% count(word, sort = FALSE)
  # Aggregate counts
  skills_count <- merge(skills_count, chunk_count, by = "word",
                        all = TRUE, suffixes = c(".old", ".new"))
  skills_count[is.na(n.old), n.old := 0]
  skills_count[is.na(n.new), n.new := 0]
  skills_count[, n := n.old + n.new]
  skills_count <- skills_count[, .(word, n)]
  cat("Processed rows:", start_row + nrow(df_chunk), "/", total_rows, "\n")
  start_row <- start_row + chunk_size
}

# ==== Save Extracted Skills ====
write.csv(skills_count, output_file, row.names = FALSE)
cat("✅ Top skills saved to", output_file, "\n")

# ==== Visualization: Top 15 In-demand Skills ====
if (nrow(skills_count) > 0) {
  plot <- skills_count %>%
    arrange(desc(n)) %>%
    slice(1:15) %>%
    ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(
      title = "Top 15 In-demand Skills Across Job Postings",
      x = "Skills",
      y = "Frequency"
    ) +
    theme_minimal()

  ggsave(plot_output_file, plot, width = 8, height = 6, units = "in")
  cat("✅ Plot saved to", plot_output_file, "\n")

  print(plot) # Display the plot in the notebook output
}