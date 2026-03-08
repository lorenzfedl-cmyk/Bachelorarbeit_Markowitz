# libraries laden
library(tidyquant)

# daten laden
meta_test_2025 <- read.csv2("data/DAX_metadata_2025.csv", stringsAsFactors = FALSE)
returns_test_2025 <- read.csv2("data/DAX_returns_2025.csv", stringsAsFactors = FALSE)

returns_numeric <- returns_test_2025[, -1]

# 0 als fehlende Werte interpretieren
returns_numeric[returns_numeric == 0] <- NA

# Kovarianzmatrix berechnen
cov_matrix <- cov(returns_numeric, use = "pairwise.complete.obs")

git status

