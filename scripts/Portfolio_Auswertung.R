# libraries laden
library(tidyverse)  # Beinhaltet ggplot2 (Plotting), dplyr (Datenmanipulation), readr (CSV-Import)
library(scales)     # Macht Achsenbeschriftungen schön (z.B. 10% statt 0.1)
library(ggrepel)    # Verhindert, dass sich Text-Labels in Plots überschneiden
library(viridis)    # Bietet farbenblinde-freundliche und gut lesbare Farbpaletten  

# Daten laden
# Passe den Pfad an, falls dein "data"-Ordner woanders liegt
df_weights <- read_csv2("data/Portfolio_Gewichte_Master.csv")
df_summary <- read_csv2("data/Portfolio_Summary_Master.csv")

# Kurzer Check, ob alles da ist (gibt die Struktur in der Konsole aus)
glimpse(df_weights)
glimpse(df_summary)