# libraries laden
library(tidyquant)
library(readxl)

# daten laden
meta_2010 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2025 META")
return_2010 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2010 RETURN")
return_2015 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2015 RETURN")
return_2020 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2020 RETURN")
return_2025 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2025 RETURN")

# Datumspalte (erste Spalte) entfernen
return_2010 <- return_2010[, -1]
return_2015 <- return_2015[, -1]
return_2020 <- return_2020[, -1]
return_2025 <- return_2025[, -1]

# sämtliche Spalten in numerische umwandeln
return_2010 <- data.frame(lapply(return_2010, as.numeric))
return_2015 <- data.frame(lapply(return_2015, as.numeric))
return_2020 <- data.frame(lapply(return_2020, as.numeric))
return_2025 <- data.frame(lapply(return_2025, as.numeric))

# Kovarianzmatrizen nur mit "kompletten/vorhandenen" Zahlenpaare berechnen
cov_matrix_2010 <- cov(return_2010, use = "pairwise.complete.obs")
cov_matrix_2015 <- cov(return_2015, use = "pairwise.complete.obs")
cov_matrix_2020 <- cov(return_2020, use = "pairwise.complete.obs")
cov_matrix_2025 <- cov(return_2025, use = "pairwise.complete.obs")










