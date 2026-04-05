# libraries ladens
library(tidyverse)  # Beinhaltet ggplot2 (Plotting), dplyr (Datenmanipulation), readr (CSV-Import)
library(scales)     # für bessere Achsenbeschriftungen
library(ggrepel)    # Text und Plots separat darstellen (keine visuelle Überschneidung)
library(viridis)    # erweiterte Farbpaletten
library(readxl)     # Excel-Reader

# Daten laden
df_weights <- read_csv2("data/Portfolio_Gewichte_Master.csv")
df_summary <- read_csv2("data/Portfolio_Summary_Master.csv")
df_vola <- read_csv2("data/Portfolio_Volatilitat_Master.csv")
df_momentum <- read_csv2("data/Portfolio_Momentum_Master.csv")
meta_2010 <- read_excel("data/CUSTOM TEST FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/CUSTOM TEST FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/CUSTOM TEST FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/CUSTOM TEST FROM 2010 TO 2025.xlsx", sheet = "2025 META")

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
    
    # Kehrwert MTB => BTM
    BookToMarket = 1 / as.numeric(MTBV)
  )

# Jahr-Spalten der dfs in gleiches Format für den merge umwandeln
df_weights$Jahr <- as.character(df_weights$Jahr)
df_fundamentals$Jahr <- as.character(df_fundamentals$Jahr)
df_vola$Jahr <- as.character(df_vola$Jahr)
df_momentum$Jahr <- as.character(df_momentum$Jahr)

# Titel aus der Vola-Tabelle mit den Fundamentals versehen
# (diese enthält noch alle "gültigen" Aktien!)
df_universe <- df_vola %>%
  left_join(df_fundamentals, by = c("Jahr", "Aktie")) %>%

  left_join(df_momentum, by = c("Jahr", "Aktie"))

# Aus den vier Universen werden die Verteilungsparameter ermittelt 
df_universe_zscores <- df_universe %>%
  # Gruppierung nach JAHR
  group_by(Jahr) %>%
  mutate(
    # Size: MV
    Z_Size = as.numeric(scale(MV)),
    
    # Z-Score Value: BookToMarket
    Z_Value = as.numeric(scale(BookToMarket)),
    
    # Z-Score Low Volatility: (annualisierte) Volatilität 
    Z_LowVol = as.numeric(scale(Volatilitat)) * (-1),
    
    # Z-Score Momentum: 12m-1m
    Z_Momentum = as.numeric(scale(Momentum))
  ) %>%
  ungroup()

# Portfoliogewichte mit entsprechendem Universum zusammenfügen
df_merged <- df_weights %>%
  # left join: wichtig, da eine Aktie in mehreren Portfolios im Jahr sein kann!
  left_join(df_universe_zscores, by = c("Jahr", "Aktie"))

# df für Stilfaktoren
df_exposures_styles <- df_merged %>%
  group_by(Jahr, Portfolio_Typ) %>%
  summarise(
    Exp_Size = sum(Gewicht * Z_Size, na.rm = TRUE),
    Exp_Value = sum(Gewicht * Z_Value, na.rm = TRUE),
    Exp_LowVol = sum(Gewicht * Z_LowVol, na.rm = TRUE),
    Exp_Momentum = sum(Gewicht * Z_Momentum, na.rm = TRUE),
    .groups = "drop"
  )

# df für Branchen
df_exposures_branche <- df_merged %>%
  group_by(Jahr, Portfolio_Typ, TR3N) %>%
  summarise(
    Gewicht_Prozent = sum(Gewicht, na.rm = TRUE),
    .groups = "drop"
  )

# df für Regionen
df_exposures_region <- df_merged %>%
  group_by(Jahr, Portfolio_Typ, GEOGN) %>%
  summarise(
    Gewicht_Prozent = sum(Gewicht, na.rm = TRUE),
    .groups = "drop"
  )









