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

# Listen erstellen für Schleifen
meta_list <- list(
  meta_2010,
  meta_2015,
  meta_2020,
  meta_2025
)
return_list <- list(
  return_2010,
  return_2015,
  return_2020,
  return_2025
)



# Datumspalte entfernen
return_2010 <- return_2010[, -1]
return_2015 <- return_2015[, -1]
return_2020 <- return_2020[, -1]
return_2025 <- return_2025[, -1]

# 0 als fehlende Werte interpretieren (0 wird zu NA)
return_2010[return_2010 == 0] <- NA
return_2015[return_2015 == 0] <- NA
return_2020[return_2020 == 0] <- NA
return_2025[return_2025 == 0] <- NA

# Kovarianzmatrizen nur mit "kompletten/vorhandenen" Zahlenpaare berechnen
a3 <- cov(a2, use = "pairwise.complete.obs")
b3 <- cov(b2, use = "pairwise.complete.obs")
c3 <- cov(c2, use = "pairwise.complete.obs")
d3 <- cov(d2, use = "pairwise.complete.obs")



meta_test_2025 <- read.csv2("data/2025_metadata_test.csv", stringsAsFactors = FALSE)
returns_test_2025 <- read.csv2("data/2025_returns_test.csv", stringsAsFactors = FALSE)

returns_numeric <- returns_test_2025[, -1]

# 0 als fehlende Werte interpretieren
returns_numeric[returns_numeric == 0] <- NA

# Kovarianzmatrix berechnen
cov_matrix <- cov(returns_numeric, use = "pairwise.complete.obs")



