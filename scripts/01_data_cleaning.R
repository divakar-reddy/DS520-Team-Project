###############################################
# 01_data_cleaning.R
# Objective: Clean raw job postings dataset
###############################################

library(tidyverse)
library(readr)
library(stringr)

# ==== Load Dataset ====
df <- read_csv("/workspaces/DS520-Team-Project/data/raw/postings.csv")

# ==== Inspect ====
glimpse(df)

# ==== Handle Missing Values ====
df_clean <- df %>%
  filter(!is.na(description) & description != "") %>%
  distinct()

# ==== Basic Text Cleaning ====
df_clean <- df_clean %>%
  mutate(description = str_to_lower(description),
         description = str_replace_all(description, "[^a-zA-Z0-9 ]", " "))

# ==== Save Clean Data ====
write_csv(df_clean, "/workspaces/DS520-Team-Project/data/processed/cleaned_postings.csv")

message("✅ Data cleaning completed successfully!")