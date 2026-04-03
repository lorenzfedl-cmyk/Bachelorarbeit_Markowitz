# libraries laden
library(tidyverse)  # Beinhaltet ggplot2 (Plotting), dplyr (Datenmanipulation), readr (CSV-Import)
library(scales)     # für bessere Achsenbeschriftungen
library(ggrepel)    # Text und Plots separat darstellen (keine visuelle Überschneidung)
library(viridis)    # erweiterte Farbpaletten
library(readxl)     # Excel-Reader

# Daten laden
df_weights <- read_csv2("data/Portfolio_Gewichte_Master.csv")
df_summary <- read_csv2("data/Portfolio_Summary_Master.csv")
df_vola <- read_csv2("data/Portfolio_Volatilitat_Master.csv")
meta_2010 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/FTSE 100 FROM 2010 TO 2025.xlsx", sheet = "2025 META")

# "Jahr"-Spalte hinzufügen für das spätere Zuordnen nach dem merge
meta_2010 <- meta_2010 %>% mutate(Jahr = "2010")
meta_2015 <- meta_2015 %>% mutate(Jahr = "2015")
meta_2020 <- meta_2020 %>% mutate(Jahr = "2020")
meta_2025 <- meta_2025 %>% mutate(Jahr = "2025")

# alle 4 Metadaten in einem dataframe zusammenfassen
df_fundamentals <- bind_rows(meta_2010, meta_2015, meta_2020, meta_2025)

# Datenvorbereitung:
# mutate wegen Leerzeichenproblem bei Variablen!
# Kehrwert von MTB, da mit dem BTM ausgewertet wird
df_fundamentals <- df_fundamentals %>%
  mutate(
    # Leerzeichenproblemlösung
    Aktie = make.names(NAME), 
    
    # Kehrwert MTB = BTM
    BookToMarket = 1 / as.numeric(MTBV)
  )

# Jahr-Spalten der dfs in gleiches Format für den merge umwandeln
df_weights$Jahr <- as.character(df_weights$Jahr)
df_fundamentals$Jahr <- as.character(df_fundamentals$Jahr)
df_vola$Jahr <- as.character(df_vola$Jahr)

# sämtliche meta und fundamentals anhand von Jahr und Aktie zusammenführen
df_merged <- df_weights %>%
  left_join(df_fundamentals, by = c("Jahr", "Aktie")) %>%
  left_join(df_vola, by = c("Jahr", "Aktie"))

# Z-Scores berechnen
df_zscores <- df_merged %>%
  group_by(Jahr) %>%
  mutate(
    # Size: MV - Market Value
    Z_Size = as.numeric(scale(MV)),
    
    # Z-Score Value: BookToMarket
    Z_Value = as.numeric(scale(BookToMarket)),
    
    # Z-Score Low Volitility: (annualisierte) Volatilität 
    Z_LowVol = as.numeric(scale(Volatilitat)) * (-1)
  ) %>%
  ungroup()


# 5. Portfolio-Exposures berechnen (Die Aggregation)
# -------------------------------------------------------------------------
df_exposures <- df_zscores %>%
  group_by(Jahr, Portfolio_Typ) %>%
  summarise(
    Exp_Size = sum(Gewicht * Z_Size, na.rm = TRUE),
    Exp_Value = sum(Gewicht * Z_Value, na.rm = TRUE),
    Exp_LowVol = sum(Gewicht * Z_LowVol, na.rm = TRUE),
    
    .groups = "drop"
  )

# Ergebnis anzeigen
print(head(df_exposures))