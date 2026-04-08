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

# 'X(MV)~USD in MV umbenennen (Sonderzeichen Problem)
# "Jahr"-Spalte hinzufügen für das spätere Zuordnen nach dem merge (+ numeric erzwingen wg NAs)
meta_2010 <- meta_2010 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2010", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2015 <- meta_2015 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2015", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2020 <- meta_2020 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2020", MV = as.numeric(MV), MTBV = as.numeric(MTBV))
meta_2025 <- meta_2025 %>% 
  rename(MV = `X(MV)~USD`) %>% 
  mutate(Jahr = "2025", MV = as.numeric(MV), MTBV = as.numeric(MTBV))

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

# VISUALISIERUNGEN

# 1) Regionen





# =========================================================================
# 1. REGIONEN
# =========================================================================

# Benchmark-Gewichte vom Index für ALLE Regionen nach MV berechnen (für Sortierung)
df_benchmark_region <- df_universe %>%
  group_by(Jahr, GEOGN) %>%
  # MV pro Jahr und Region addieren
  summarise(Region_MV = sum(MV, na.rm = TRUE), .groups = "drop_last") %>%
  # Region-MV durch Summe ergibt den relativen Anteil einer Region pro Jahr
  mutate(Index_Gewicht = Region_MV / sum(Region_MV, na.rm = TRUE)) %>%
  ungroup()

# Top 4 Regionen aus den Portfolios PRO JAHR GESAMT ermitteln
top4_regions_per_year <- df_exposures_region %>%
  group_by(Jahr, GEOGN) %>%
  # Summen pro Jahr und Region bilden
  summarise(Gesamt_Portfolio_Gewicht = sum(Gewicht_Prozent, na.rm = TRUE), .groups = "drop_last") %>%
  # die 4 stärksten Regionen nach Anteil absteigend ordnen
  slice_max(order_by = Gesamt_Portfolio_Gewicht, n = 4) %>%
  select(Jahr, GEOGN) %>%
  mutate(Kategorie = GEOGN) 

# Gesamtgewichte zu Regionen zuordnen und "Sonstige" (=alles nach den 4 größten) einfügen
df_bench_plot <- df_benchmark_region %>%
  # alle die nicht zu den Top 4 gehören werden als NA bzw. Sonstige bezeichnet
  left_join(top4_regions_per_year, by = c("Jahr", "GEOGN")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Kategorie) %>%
  summarise(Index_Gewicht = sum(Index_Gewicht, na.rm = TRUE), .groups = "drop")

# nochmal Gesamtgewichte zu Regionen zuordnen für Portfolios
df_port_plot <- df_exposures_region %>%
  left_join(top4_regions_per_year, by = c("Jahr", "GEOGN")) %>%
  mutate(Kategorie = replace_na(Kategorie, "Sonstige")) %>%
  group_by(Jahr, Portfolio_Typ, Kategorie) %>%
  summarise(Gewicht_Prozent = sum(Gewicht_Prozent, na.rm = TRUE), .groups = "drop")

# Gewichte mit Jahr versehen damit 4 Gruppen/Summen gebildet werden können
plot_data_region <- expand_grid(
  df_bench_plot,
  Portfolio_Typ = unique(df_port_plot$Portfolio_Typ)
) %>%
  left_join(df_port_plot, by = c("Jahr", "Kategorie", "Portfolio_Typ")) %>%
  mutate(Gewicht_Prozent = replace_na(Gewicht_Prozent, 0))

# 5. Sortierungen für das Diagramm festlegen
portfolio_order <- c("Min Variance", "Target Vol 10%", "Target Vol 12%", "Target Vol 15%", 
                     "Target Ret 12%", "Target Ret 15%", "Target Ret 18%")
plot_data_region$Portfolio_Typ <- factor(plot_data_region$Portfolio_Typ, levels = portfolio_order)

# ---> NEU: Dynamische Sortierung PRO JAHR (Facet-Trick) <---
# Zuerst die absolute Summe der Portfoliogewichte pro Region und Jahr berechnen
plot_data_region <- plot_data_region %>%
  group_by(Jahr, Kategorie) %>%
  mutate(Summe_Gewicht_Jahr = sum(Gewicht_Prozent, na.rm = TRUE)) %>%
  ungroup()

# "Sonstige" zwingend ans Ende setzen (fiktiv sehr negative Summe)
plot_data_region <- plot_data_region %>%
  mutate(Summe_Gewicht_Jahr = ifelse(Kategorie == "Sonstige", -Inf, Summe_Gewicht_Jahr))

# Um pro Jahr individuell sortieren zu können, bauen wir eine Hilfsspalte: "Jahr__Kategorie"
plot_data_region <- plot_data_region %>%
  mutate(Kategorie_Facet = paste(Jahr, Kategorie, sep = "__"))

# Jetzt sortieren wir diese Hilfsspalte absteigend (-) nach der berechneten Summe
plot_data_region$Kategorie_Facet <- reorder(plot_data_region$Kategorie_Facet, -plot_data_region$Summe_Gewicht_Jahr)


# 6. Der finale Plot
plot_region_grid <- ggplot(plot_data_region, aes(x = Kategorie_Facet)) +  # Hier die Hilfsspalte nutzen!
  
  geom_col(aes(y = Gewicht_Prozent, fill = Portfolio_Typ), 
           position = position_dodge(width = 0.85), 
           color = "black", linewidth = 0.2, alpha = 0.9) +
  
  geom_errorbar(aes(ymin = Index_Gewicht, ymax = Index_Gewicht), 
                color = "red", linewidth = 1, width = 0.85) +
  
  facet_wrap(~ Jahr, ncol = 2, scales = "free_x") +
  
  # ---> NEU: Das künstliche "__" wieder sauber abschneiden <---
  scale_x_discrete(labels = function(x) gsub("^.*__", "", x)) +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_viridis_d(option = "mako") + 
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    title = "Regionale Portfolio-Allokation vs. Benchmark",
    subtitle = "Top 4 Regionen im Portfolio + 'Sonstige' (Sortiert nach Portfoliogewicht)",
    x = NULL,
    y = "Anteil am Portfolio / Index",
    fill = "Portfolio:"
  )

print(plot_region_grid)
# Dateiexport
ggsave("data/Plot_Regionen_Grid.png", plot = plot_region_grid, width = 14, height = 8, dpi = 300) 
       
       
       
       
       
       
       
       
       
       
       
       