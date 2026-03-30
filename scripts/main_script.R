# libraries laden
library(tidyquant)
library(readxl)

# V A R I A B L E N

# Schwellenwert für die Summe von Nullen an Tagen, die mit "NA" ersetzt werden sollen
thresh_zero_return <- 0.05


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

# Funktion zum Ersetzen von Nullen durch NA bei Überschreiten des Schwellenwerts
replace_zeros_with_na <- function(df, threshold) {
  # Zeilen ermitteln, die den Schwellenwert erreichen oder überschreiten
  target_rows <- rowSums(df == 0, na.rm = TRUE) / ncol(df) >= threshold
  
  # Isoliere diese Zeilen temporär
  temp_subset <- df[target_rows, ]
  
  # Ersetze nur innerhalb dieser isolierten Zeilen die Nullen durch NA
  temp_subset[temp_subset == 0] <- NA
  
  # Füge die bearbeiteten Zeilen wieder in den originalen Datensatz ein
  df[target_rows, ] <- temp_subset
  
  return(df)
}

# Funktion auf alle vier Datensätze anwenden
return_2010 <- replace_zeros_with_na(return_2010, thresh_zero_return)
return_2015 <- replace_zeros_with_na(return_2015, thresh_zero_return)
return_2020 <- replace_zeros_with_na(return_2020, thresh_zero_return)
return_2025 <- replace_zeros_with_na(return_2025, thresh_zero_return)

# Kovarianzmatrizen nur mit "kompletten/vorhandenen" Zahlenpaare berechnen
cov_matrix_2010 <- cov(return_2010, use = "pairwise.complete.obs")
cov_matrix_2015 <- cov(return_2015, use = "pairwise.complete.obs")
cov_matrix_2020 <- cov(return_2020, use = "pairwise.complete.obs")
cov_matrix_2025 <- cov(return_2025, use = "pairwise.complete.obs")










