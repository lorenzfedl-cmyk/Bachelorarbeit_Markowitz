# libraries laden
library(corpcor)
library(readxl)
library(quadprog)

# VARIABLEN

# Schwellenwert für die relative Anzahl von Nullen an Tagen, die mit "NA" ersetzt werden sollen
thresh_zero_return <- 0.05

# Schwellenewert für die relative Anzahl von Renditen pro Aktie die vorhanden sein müssen
thresh_valid_returns <- 0.4

# Anzahl der ermittelten Portfolios auf der Effizienzlinie (für Portfolios mit Zielrisiko)
# zwischen min var Portfolio und max. Rendite Portfolio
num_port_eff <- 10

# Zielrenditen für Portfolios
target_ret_a <- 0.12
target_ret_b <- 0.15
target_ret_c <- 0.18

# Zielrisiko für Portfolios
target_vol_a <- 0.10
target_vol_b <- 0.12
target_vol_c <- 0.15

# Mindestgewicht für Aktien, die in den finalen Portfolios aufgenommen werden sollen
min_share_weight <- 0.0001

# Daten laden
meta_2010 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2010 META")
meta_2015 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2015 META")
meta_2020 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2020 META")
meta_2025 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2025 META")
return_2010 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2010 RETURN")
return_2015 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2015 RETURN")
return_2020 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2020 RETURN")
return_2025 <- read_excel("data/S&P500 FROM 2010 TO 2025.xlsx", sheet = "2025 RETURN")

# Datumspalte (=erste Spalte) entfernen
return_2010 <- return_2010[, -1]
return_2015 <- return_2015[, -1] 
return_2020 <- return_2020[, -1] 
return_2025 <- return_2025[, -1] 

# sämtliche Spalten in numerische umwandeln
return_2010 <- data.frame(lapply(return_2010, as.numeric))
return_2015 <- data.frame(lapply(return_2015, as.numeric))
return_2020 <- data.frame(lapply(return_2020, as.numeric))
return_2025 <- data.frame(lapply(return_2025, as.numeric))

# Prozentzahlen in Dezimalzahlen umwandeln
return_2010 <- return_2010 / 100
return_2015 <- return_2015 / 100
return_2020 <- return_2020 / 100
return_2025 <- return_2025 / 100

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

# Funktion zum Entfernen von Aktien (Spalten) mit zu wenig Datenpunkten
filter_stocks_by_na <- function(df, threshold) {
  # relativen Anteil an gesamten Return ermitteln um mit dem Schwellenwert vergleichen zu können
  valid_ratio <- colSums(!is.na(df)) / nrow(df)
  
  # Informativ: Zusammenfassung der Aktien, die entfernt wurden
  dropped_stocks <- names(df)[valid_ratio < threshold]
  
  # Konsolenausgabe der entfernten Aktien
  if(length(dropped_stocks) > 0) {
    cat("Entfernte Aktien (weniger als", threshold * 100, "% Daten):\n")
    cat(paste(dropped_stocks, collapse = ", "), "\n\n")
  }
  
  # Behalte nur die Spalten, die den Schwellenwert erreichen oder überschreiten
  df_filtered <- df[, valid_ratio >= threshold]
  
  return(df_filtered)
}

# Funktion zum Extrahieren und Säubern der Portfolio-Gewichte
extract_weights <- function(portfolio_result, return_df, year_label, port_label) {
  
  # Sicherheitsprüfung, falls das Portfolio nicht berechnet werden konnte
  if(is.na(portfolio_result$risk[1])) return(NULL)
  
  # Tabelle bauen aus Spaltennamen (Aktien) und Gewichten
  df <- data.frame(
    Jahr = year_label,
    Portfolio_Typ = port_label,
    Aktie = colnames(return_df),
    Gewicht = round(portfolio_result$weights, 4) # Runden auf 4 Nachkommastellen
  )
  
  # Es werden nur Aktien behalten, die ein Gewicht von mind. "min_share_weight" haben.
  df <- df[df$Gewicht >= min_share_weight, ]
  
  # Ergebnisse nach Gewicht absteigend sortieren
  df <- df[order(-df$Gewicht), ]
  
  # Zeilennamen resetten
  rownames(df) <- NULL
  
  return(df)
}

# Funktion zur Berechnung des 12M-1M Momentum bei DAILY RETURNS
calculate_momentum <- function(return_df) {
  
  n_days <- nrow(return_df)
  
  # letzte Zeile ist jüngstes Datum
  # 252 - "heute" = 251
  start_row <- max(1, n_days - 251) 
  
  # letzten 21 Tage (=1 Monat) wird ausgeschlossen
  end_row <- max(1, n_days - 21)    
  
  # Dieser Zeitraum wird abgetrennt...
  mom_data <- return_df[start_row:end_row, , drop = FALSE]
  
  # ... und davon die kummulierte Rendite berechnet = Momentum
  mom_scores <- apply(mom_data, 2, function(x) {
    prod(1 + x[!is.na(x)]) - 1
  })
  
  return(mom_scores)
}

# Alle NA/0 - Funktionen auf alle vier Datensätze anwenden
return_2010 <- replace_zeros_with_na(return_2010, thresh_zero_return)
return_2015 <- replace_zeros_with_na(return_2015, thresh_zero_return)
return_2020 <- replace_zeros_with_na(return_2020, thresh_zero_return)
return_2025 <- replace_zeros_with_na(return_2025, thresh_zero_return)

return_2010 <- filter_stocks_by_na(return_2010, thresh_valid_returns)
return_2015 <- filter_stocks_by_na(return_2015, thresh_valid_returns)
return_2020 <- filter_stocks_by_na(return_2020, thresh_valid_returns)
return_2025 <- filter_stocks_by_na(return_2025, thresh_valid_returns)

# Daten bereinigen: Tage mit mind. 1xNA werden gelöscht
return_2010_cc <- na.omit(return_2010)
return_2015_cc <- na.omit(return_2015)
return_2020_cc <- na.omit(return_2020)
return_2025_cc <- na.omit(return_2025)

# Übersichtstabelle für den Datenverlust erstellen
robustness_summary <- data.frame(
  Jahr = c("2010", "2015", "2020", "2025"),
  Verbleibende_Tage = c(nrow(return_2010_cc), nrow(return_2015_cc), nrow(return_2020_cc), nrow(return_2025_cc)),
  Gesamte_Tage = c(nrow(return_2010), nrow(return_2015), nrow(return_2020), nrow(return_2025))
)

# Prozentualen Verlust berechnen
robustness_summary$Verlust_Prozent <- round(100 - (robustness_summary$Verbleibende_Tage / robustness_summary$Gesamte_Tage * 100), 2)

# Tabelle in der Konsole anzeigen (und im Environment von RStudio abrufbar)
print("--- ROBUSTHEITSCHECK: DATENVERLUST DURCH NA.OMIT ---")
print(robustness_summary)

# Alternative (na.omit) Kovarianzmatrizen berechnen
cov_matrix_2010_cc <- cov(return_2010_cc) * 252
cov_matrix_2015_cc <- cov(return_2015_cc) * 252
cov_matrix_2020_cc <- cov(return_2020_cc) * 252
cov_matrix_2025_cc <- cov(return_2025_cc) * 252

# Kovarianzmatrizen nur mit "kompletten/vorhandenen" Zahlenpaare berechnen (annualisiert)
cov_matrix_2010 <- cov(return_2010, use = "pairwise.complete.obs") * 252
cov_matrix_2015 <- cov(return_2015, use = "pairwise.complete.obs") * 252
cov_matrix_2020 <- cov(return_2020, use = "pairwise.complete.obs") * 252
cov_matrix_2025 <- cov(return_2025, use = "pairwise.complete.obs") * 252

# Positive Definitheit erzwingen (für solve.QP)
cov_matrix_2010 <- make.positive.definite(cov_matrix_2010)
cov_matrix_2015 <- make.positive.definite(cov_matrix_2015)
cov_matrix_2020 <- make.positive.definite(cov_matrix_2020)
cov_matrix_2025 <- make.positive.definite(cov_matrix_2025)

# historische, diskrete, annualisierte Erwartungswerte sämtlicher Aktien
exp_return_2010 <- colMeans(return_2010, na.rm = TRUE) * 252
exp_return_2015 <- colMeans(return_2015, na.rm = TRUE) * 252
exp_return_2020 <- colMeans(return_2020, na.rm = TRUE) * 252
exp_return_2025 <- colMeans(return_2025, na.rm = TRUE) * 252

# Funktion für eine erwartete Portfoliorendite
portfolio_return <- function(w, mu) { sum(w * mu)}

# Funktion für eine Portfoliovolatilität
portfolio_risk <- function(w, Sigma) {sqrt(as.numeric(t(w) %*% Sigma %*% w))}

# Funktion für ein Min.Var.-Portfolio
min_var_portfolio <- function(mu, Sigma) {
  
  # Anzahl Assets
  n <- length(mu)
  
  # Varianzminimierung
  Dmat <- 2 * Sigma 
  dvec <- rep(0, n)  
  
  # Nebenbedingungen: Summe Gewichte = 1 UND keine Short Sales w >= 0
  Amat <- cbind(
    rep(1, n),   
    diag(n)      
  )
  
  bvec <- c(
    1,          
    rep(0, n)  
  )
  
  # meq = 1 bedeutet:  erste Bedingung ist Gleichung (sum(w) = 1) rest sind Ungleichungen (>=)
  result <- solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
  
  # optimale Gewichte w
  w <- result$solution
  
  # Rückgabe als Liste
  list(
    weights = w,
    expected_return = portfolio_return(w, mu),
    risk = portfolio_risk(w, Sigma)
  )
}

# Funktion für ein Portfolio mit vorgegebener Zielrendite
target_return_portfolio <- function(mu, Sigma, target_ret) {
  
  n <- length(mu)
  Dmat <- 2 * Sigma  
  dvec <- rep(0, n)  
  
  # Sicherheitsprüfung ob die Renditeerwartung möglich ist
  max_possible_ret <- max(mu, na.rm = TRUE)
  if(target_ret > max_possible_ret) {
    warning(paste("Zielrendite", round(target_ret*100,2), "% ist mathematisch unmöglich. Setze auf maximal mögliche Rendite von", round(max_possible_ret*100,2), "%."))
    target_ret <- max_possible_ret
  }
  
  # Nebenbedingungen: Summe Gewichte = 1 UND keine Short Sales w >= 0
  Amat <- cbind(
    rep(1, n),   
    mu,          
    diag(n)      
  )
  
  bvec <- c(
    1,           
    target_ret,  
    rep(0, n)    
  )
  
  # solve.QP durchführen (eingepackt in tryCatch, falls es unlösbare Konstellationen gibt)
  result <- try(solve.QP(Dmat, dvec, Amat, bvec, meq = 1), silent = TRUE)
  
  # Fehlermeldung falls keine Lösung gefunden wurde
  if (inherits(result, "try-error")) {
    return(list(weights = rep(NA, n), expected_return = NA, risk = NA))
  }
  
  w <- result$solution
  
  list(
    weights = w,
    expected_return = portfolio_return(w, mu),
    risk = portfolio_risk(w, Sigma)
  )
}

# Funktion für ein Portfolio mit vorgegebenem Zielrisiko
target_vola_portfolio <- function(mu, Sigma, target_vol) {
  
  # 1. min var Portfolio ermitteln
  mvp <- min_var_portfolio(mu, Sigma)
  
  # Sicherheitsprüfung ob Zielrisiko erreichbar ist, wenn nicht -> Fehlermeldung
  if(target_vol < mvp$risk) {
    warning(paste("Zielrisiko", round(target_vol*100,2), "% ist zu gering! Das niedrigste Risiko möglich ist", round(mvp$risk*100,2), "%. Gebe Minimum-Varianz-Portfolio zurück."))
    return(mvp)
  }
  
  # Maximal mögliche Rendite ermitteln
  max_ret <- max(mu, na.rm = TRUE)
  
  # einzelne Portfolios auf der Effizienzlinie ermitteln mit Anzahl von "num_port_eff"
  ret_grid <- seq(mvp$expected_return, max_ret, length.out = num_port_eff)
  
  best_port <- mvp
  min_diff <- Inf 
  
  # Portfolio nehmen, das am nächsten vom Zielportfolio ist
  for(ret in ret_grid) {
    port <- target_return_portfolio(mu, Sigma, ret)
    if(!is.na(port$risk)) {
      # Differenz von Zielportfolio und und grid-portfolio
      diff <- abs(port$risk - target_vol)
      
      # Wenn es näher dran ist, dann speichern
      if(diff < min_diff) {
        min_diff <- diff
        best_port <- port
      }
    }
  }
  
  return(best_port)
}

# ZIEL-PORTFOLIOS BERECHNEN: Jahr 2010

# Minimum-Varianz-Portfolio berechnen
mvp_2010 <- min_var_portfolio(exp_return_2010, cov_matrix_2010)

# Portfolios basierend auf Zielrenditen berechnen
port_ret_a <- target_return_portfolio(exp_return_2010, cov_matrix_2010, target_ret_a)
port_ret_b <- target_return_portfolio(exp_return_2010, cov_matrix_2010, target_ret_b)
port_ret_c <- target_return_portfolio(exp_return_2010, cov_matrix_2010, target_ret_c)

# Portfolios basierend auf Zielrisiko berechnen
port_vol_a <- target_vola_portfolio(exp_return_2010, cov_matrix_2010, target_vol_a)
port_vol_b <- target_vola_portfolio(exp_return_2010, cov_matrix_2010, target_vol_b)
port_vol_c <- target_vola_portfolio(exp_return_2010, cov_matrix_2010, target_vol_c)

# Ergebnisse tabelarisch zusammenfassen
summary_2010 <- data.frame(
  Portfolio_Typ = c(
    "Min Variance", 
    paste0("Target Ret ", target_ret_a * 100, "%"), 
    paste0("Target Ret ", target_ret_b * 100, "%"), 
    paste0("Target Ret ", target_ret_c * 100, "%"), 
    paste0("Target Vol ", target_vol_a * 100, "%"), 
    paste0("Target Vol ", target_vol_b * 100, "%"), 
    paste0("Target Vol ", target_vol_c * 100, "%")
                    ),
  Rendite_Prozent = round(c(mvp_2010$expected_return, 
                            port_ret_a$expected_return, port_ret_b$expected_return, port_ret_c$expected_return,
                            port_vol_a$expected_return, port_vol_b$expected_return, port_vol_c$expected_return) * 100, 2),
  Vola_Prozent = round(c(mvp_2010$risk, 
                         port_ret_a$risk, port_ret_b$risk, port_ret_c$risk,
                         port_vol_a$risk, port_vol_b$risk, port_vol_c$risk) * 100, 2)
)

print(summary_2010)

# Gewichte exportieren/extrahieren 2010
w_mvp_2010   <- extract_weights(mvp_2010, return_2010, "2010", "Min Variance")

w_ret_a_2010 <- extract_weights(port_ret_a, return_2010, "2010", paste0("Target Ret ", target_ret_a*100, "%"))
w_ret_b_2010 <- extract_weights(port_ret_b, return_2010, "2010", paste0("Target Ret ", target_ret_b*100, "%"))
w_ret_c_2010 <- extract_weights(port_ret_c, return_2010, "2010", paste0("Target Ret ", target_ret_c*100, "%"))

w_vol_a_2010 <- extract_weights(port_vol_a, return_2010, "2010", paste0("Target Vol ", target_vol_a*100, "%"))
w_vol_b_2010 <- extract_weights(port_vol_b, return_2010, "2010", paste0("Target Vol ", target_vol_b*100, "%"))
w_vol_c_2010 <- extract_weights(port_vol_c, return_2010, "2010", paste0("Target Vol ", target_vol_c*100, "%"))

# Alle Gewichte in ein Dataframe zusamensetzen
all_weights_2010 <- rbind(w_mvp_2010, 
                          w_ret_a_2010, w_ret_b_2010, w_ret_c_2010, 
                          w_vol_a_2010, w_vol_b_2010, w_vol_c_2010)

# Minimum-Varianz-Portfolio berechnen
mvp_2015 <- min_var_portfolio(exp_return_2015, cov_matrix_2015)

# Portfolios basierend auf Zielrenditen berechnen
port_ret_a <- target_return_portfolio(exp_return_2015, cov_matrix_2015, target_ret_a)
port_ret_b <- target_return_portfolio(exp_return_2015, cov_matrix_2015, target_ret_b)
port_ret_c <- target_return_portfolio(exp_return_2015, cov_matrix_2015, target_ret_c)

# Portfolios basierend auf Zielrisiko berechnen
port_vol_a <- target_vola_portfolio(exp_return_2015, cov_matrix_2015, target_vol_a)
port_vol_b <- target_vola_portfolio(exp_return_2015, cov_matrix_2015, target_vol_b)
port_vol_c <- target_vola_portfolio(exp_return_2015, cov_matrix_2015, target_vol_c)

# Ergebnisse tabelarisch zusammenfassen
summary_2015 <- data.frame(
  Portfolio_Typ = c(
    "Min Variance", 
    paste0("Target Ret ", target_ret_a * 100, "%"), 
    paste0("Target Ret ", target_ret_b * 100, "%"), 
    paste0("Target Ret ", target_ret_c * 100, "%"), 
    paste0("Target Vol ", target_vol_a * 100, "%"), 
    paste0("Target Vol ", target_vol_b * 100, "%"), 
    paste0("Target Vol ", target_vol_c * 100, "%")
  ),
  Rendite_Prozent = round(c(mvp_2015$expected_return, 
                            port_ret_a$expected_return, port_ret_b$expected_return, port_ret_c$expected_return,
                            port_vol_a$expected_return, port_vol_b$expected_return, port_vol_c$expected_return) * 100, 2),
  Vola_Prozent = round(c(mvp_2015$risk, 
                         port_ret_a$risk, port_ret_b$risk, port_ret_c$risk,
                         port_vol_a$risk, port_vol_b$risk, port_vol_c$risk) * 100, 2)
)

print(summary_2015)

# Gewichte exportieren/extrahieren 2015
w_mvp_2015   <- extract_weights(mvp_2015, return_2015, "2015", "Min Variance")

w_ret_a_2015 <- extract_weights(port_ret_a, return_2015, "2015", paste0("Target Ret ", target_ret_a*100, "%"))
w_ret_b_2015 <- extract_weights(port_ret_b, return_2015, "2015", paste0("Target Ret ", target_ret_b*100, "%"))
w_ret_c_2015 <- extract_weights(port_ret_c, return_2015, "2015", paste0("Target Ret ", target_ret_c*100, "%"))

w_vol_a_2015 <- extract_weights(port_vol_a, return_2015, "2015", paste0("Target Vol ", target_vol_a*100, "%"))
w_vol_b_2015 <- extract_weights(port_vol_b, return_2015, "2015", paste0("Target Vol ", target_vol_b*100, "%"))
w_vol_c_2015 <- extract_weights(port_vol_c, return_2015, "2015", paste0("Target Vol ", target_vol_c*100, "%"))

# Alle Gewichte in ein Dataframe zusamensetzen
all_weights_2015 <- rbind(w_mvp_2015, 
                          w_ret_a_2015, w_ret_b_2015, w_ret_c_2015, 
                          w_vol_a_2015, w_vol_b_2015, w_vol_c_2015)

# ZIEL-PORTFOLIOS BERECHNEN: Jahr 2020

# Minimum-Varianz-Portfolio berechnen
mvp_2020 <- min_var_portfolio(exp_return_2020, cov_matrix_2020)

# Portfolios basierend auf Zielrenditen berechnen
port_ret_a <- target_return_portfolio(exp_return_2020, cov_matrix_2020, target_ret_a)
port_ret_b <- target_return_portfolio(exp_return_2020, cov_matrix_2020, target_ret_b)
port_ret_c <- target_return_portfolio(exp_return_2020, cov_matrix_2020, target_ret_c)

# Portfolios basierend auf Zielrisiko berechnen
port_vol_a <- target_vola_portfolio(exp_return_2020, cov_matrix_2020, target_vol_a)
port_vol_b <- target_vola_portfolio(exp_return_2020, cov_matrix_2020, target_vol_b)
port_vol_c <- target_vola_portfolio(exp_return_2020, cov_matrix_2020, target_vol_c)

# Ergebnisse tabelarisch zusammenfassen
summary_2020 <- data.frame(
  Portfolio_Typ = c(
    "Min Variance", 
    paste0("Target Ret ", target_ret_a * 100, "%"), 
    paste0("Target Ret ", target_ret_b * 100, "%"), 
    paste0("Target Ret ", target_ret_c * 100, "%"), 
    paste0("Target Vol ", target_vol_a * 100, "%"), 
    paste0("Target Vol ", target_vol_b * 100, "%"), 
    paste0("Target Vol ", target_vol_c * 100, "%")
  ),
  Rendite_Prozent = round(c(mvp_2020$expected_return, 
                            port_ret_a$expected_return, port_ret_b$expected_return, port_ret_c$expected_return,
                            port_vol_a$expected_return, port_vol_b$expected_return, port_vol_c$expected_return) * 100, 2),
  Vola_Prozent = round(c(mvp_2020$risk, 
                         port_ret_a$risk, port_ret_b$risk, port_ret_c$risk,
                         port_vol_a$risk, port_vol_b$risk, port_vol_c$risk) * 100, 2)
)

print(summary_2020)

# Gewichte exportieren/extrahieren 2020
w_mvp_2020   <- extract_weights(mvp_2020, return_2020, "2020", "Min Variance")

w_ret_a_2020 <- extract_weights(port_ret_a, return_2020, "2020", paste0("Target Ret ", target_ret_a*100, "%"))
w_ret_b_2020 <- extract_weights(port_ret_b, return_2020, "2020", paste0("Target Ret ", target_ret_b*100, "%"))
w_ret_c_2020 <- extract_weights(port_ret_c, return_2020, "2020", paste0("Target Ret ", target_ret_c*100, "%"))

w_vol_a_2020 <- extract_weights(port_vol_a, return_2020, "2020", paste0("Target Vol ", target_vol_a*100, "%"))
w_vol_b_2020 <- extract_weights(port_vol_b, return_2020, "2020", paste0("Target Vol ", target_vol_b*100, "%"))
w_vol_c_2020 <- extract_weights(port_vol_c, return_2020, "2020", paste0("Target Vol ", target_vol_c*100, "%"))

# Alle Gewichte in ein Dataframe zusamensetzen
all_weights_2020 <- rbind(w_mvp_2020, 
                          w_ret_a_2020, w_ret_b_2020, w_ret_c_2020, 
                          w_vol_a_2020, w_vol_b_2020, w_vol_c_2020)

# ZIEL-PORTFOLIOS BERECHNEN: Jahr 2025

# Minimum-Varianz-Portfolio berechnen
mvp_2025 <- min_var_portfolio(exp_return_2025, cov_matrix_2025)

# Portfolios basierend auf Zielrenditen berechnen
port_ret_a <- target_return_portfolio(exp_return_2025, cov_matrix_2025, target_ret_a)
port_ret_b <- target_return_portfolio(exp_return_2025, cov_matrix_2025, target_ret_b)
port_ret_c <- target_return_portfolio(exp_return_2025, cov_matrix_2025, target_ret_c)

# Portfolios basierend auf Zielrisiko berechnen
port_vol_a <- target_vola_portfolio(exp_return_2025, cov_matrix_2025, target_vol_a)
port_vol_b <- target_vola_portfolio(exp_return_2025, cov_matrix_2025, target_vol_b)
port_vol_c <- target_vola_portfolio(exp_return_2025, cov_matrix_2025, target_vol_c)

# Ergebnisse tabelarisch zusammenfassen
summary_2025 <- data.frame(
  Portfolio_Typ = c(
    "Min Variance", 
    paste0("Target Ret ", target_ret_a * 100, "%"), 
    paste0("Target Ret ", target_ret_b * 100, "%"), 
    paste0("Target Ret ", target_ret_c * 100, "%"), 
    paste0("Target Vol ", target_vol_a * 100, "%"), 
    paste0("Target Vol ", target_vol_b * 100, "%"), 
    paste0("Target Vol ", target_vol_c * 100, "%")
  ),
  Rendite_Prozent = round(c(mvp_2025$expected_return, 
                            port_ret_a$expected_return, port_ret_b$expected_return, port_ret_c$expected_return,
                            port_vol_a$expected_return, port_vol_b$expected_return, port_vol_c$expected_return) * 100, 2),
  Vola_Prozent = round(c(mvp_2025$risk, 
                         port_ret_a$risk, port_ret_b$risk, port_ret_c$risk,
                         port_vol_a$risk, port_vol_b$risk, port_vol_c$risk) * 100, 2)
)

print(summary_2025)

# Gewichte exportieren/extrahieren 2025
w_mvp_2025   <- extract_weights(mvp_2025, return_2025, "2025", "Min Variance")

w_ret_a_2025 <- extract_weights(port_ret_a, return_2025, "2025", paste0("Target Ret ", target_ret_a*100, "%"))
w_ret_b_2025 <- extract_weights(port_ret_b, return_2025, "2025", paste0("Target Ret ", target_ret_b*100, "%"))
w_ret_c_2025 <- extract_weights(port_ret_c, return_2025, "2025", paste0("Target Ret ", target_ret_c*100, "%"))

w_vol_a_2025 <- extract_weights(port_vol_a, return_2025, "2025", paste0("Target Vol ", target_vol_a*100, "%"))
w_vol_b_2025 <- extract_weights(port_vol_b, return_2025, "2025", paste0("Target Vol ", target_vol_b*100, "%"))
w_vol_c_2025 <- extract_weights(port_vol_c, return_2025, "2025", paste0("Target Vol ", target_vol_c*100, "%"))

# Alle Gewichte in ein Dataframe zusamensetzen
all_weights_2025 <- rbind(w_mvp_2025, 
                          w_ret_a_2025, w_ret_b_2025, w_ret_c_2025, 
                          w_vol_a_2025, w_vol_b_2025, w_vol_c_2025)

# Alle Jahre zu einer Tabelle zusammenfügen 
master_weights <- rbind(all_weights_2010, all_weights_2015, all_weights_2020, all_weights_2025)

# jeweiliges Jahr zur Summary hinzufügen
summary_2010$Jahr <- "2010"
summary_2015$Jahr <- "2015"
summary_2020$Jahr <- "2020"
summary_2025$Jahr <- "2025"

# Zusammenfügen der Summary-Tabellen
master_summary <- rbind(summary_2010, summary_2015, summary_2020, summary_2025)

# Spaltenordnung herstellen
master_summary <- master_summary[, c("Jahr", "Portfolio_Typ", "Rendite_Prozent", "Vola_Prozent")]

# Vola aus Kovarianzmatrix ermitteln
vola_2010 <- data.frame(Jahr = "2010", Aktie = colnames(cov_matrix_2010), Volatilitat = sqrt(diag(cov_matrix_2010)))
vola_2015 <- data.frame(Jahr = "2015", Aktie = colnames(cov_matrix_2015), Volatilitat = sqrt(diag(cov_matrix_2015)))
vola_2020 <- data.frame(Jahr = "2020", Aktie = colnames(cov_matrix_2020), Volatilitat = sqrt(diag(cov_matrix_2020)))
vola_2025 <- data.frame(Jahr = "2025", Aktie = colnames(cov_matrix_2025), Volatilitat = sqrt(diag(cov_matrix_2025)))

# Alle Vola-Jahre zusammenfügen
master_vola <- rbind(vola_2010, vola_2015, vola_2020, vola_2025)
rownames(master_vola) <- NULL

# Momentum berechnen (Funktion auf alle Jahre)
mom_2010 <- data.frame(Jahr = "2010", Aktie = colnames(return_2010), Momentum = as.numeric(calculate_momentum(return_2010)))
mom_2015 <- data.frame(Jahr = "2015", Aktie = colnames(return_2015), Momentum = as.numeric(calculate_momentum(return_2015)))
mom_2020 <- data.frame(Jahr = "2020", Aktie = colnames(return_2020), Momentum = as.numeric(calculate_momentum(return_2020)))
mom_2025 <- data.frame(Jahr = "2025", Aktie = colnames(return_2025), Momentum = as.numeric(calculate_momentum(return_2025)))

# Alle Momentum-Jahre zusammenfügen
master_momentum <- rbind(mom_2010, mom_2015, mom_2020, mom_2025)
rownames(master_momentum) <- NULL

# als CSV speichern
write.csv2(master_weights, "data/Portfolio_Gewichte_Master.csv", row.names = FALSE)
write.csv2(master_summary, "data/Portfolio_Summary_Master.csv", row.names = FALSE)
write.csv2(master_vola, "data/Portfolio_Volatilitat_Master.csv", row.names = FALSE)
write.csv2(master_momentum, "data/Portfolio_Momentum_Master.csv", row.names = FALSE)

# Abschlussmeldung
cat("Berechnung abgeschlossen. Daten wurden erfolgreich exportiert!\n")

# Available-Case vs. Complete-Case (na.omit)
# Exemplarisch für das min-var-portfolio aus 2025

cat("\nStarte Gewichtevergleich für 2025...\n")

# neue Erwartungswerte für die CC-Datene berechnen
exp_return_2025_cc <- colMeans(return_2025_cc) * 252

# Min-Var-Portfolio mit CC-Daten versuchen zu berechnen (in 'try' gewrappt)
mvp_2025_cc_result <- try(min_var_portfolio(exp_return_2025_cc, cov_matrix_2025_cc), silent = TRUE)

# Prüfen, ob der Optimierer mit der na.omit-Matrix abstürzt
if(!inherits(mvp_2025_cc_result, "try-error") && !is.na(mvp_2025_cc_result$risk[1])) {
  
  # Gewichte extrahieren
  w_mvp_2025_cc <- extract_weights(mvp_2025_cc_result, return_2025_cc, "2025", "Min Variance (CC)")
  
  # Gewichte mit der normalen Methode (w_mvp_2025) mergen
  vergleich_gewichte <- merge(
    x = w_mvp_2025[, c("Aktie", "Gewicht")],
    y = w_mvp_2025_cc[, c("Aktie", "Gewicht")],
    by = "Aktie",
    all = TRUE,
    suffixes = c("_Normal", "_na_omit")
  )
  
  # NAs durch 0 ersetzen (Aktien, die in der einen Methode gekauft wurden, in der anderen aber nicht)
  vergleich_gewichte[is.na(vergleich_gewichte)] <- 0
  
  # Absolute Differenz berechnen
  vergleich_gewichte$Differenz_absolut <- abs(vergleich_gewichte$Gewicht_Normal - vergleich_gewichte$Gewicht_na_omit)
  
  # Nach größter Differenz absteigend sortieren
  vergleich_gewichte <- vergleich_gewichte[order(-vergleich_gewichte$Differenz_absolut), ]
  
  # Konsole ausgeben und exportieren
  cat("\n--- GEWICHTEVERGLEICH: NORMAL VS. NA.OMIT (MVP 2025) ---\n")
  print(head(vergleich_gewichte, 15)) # Zeigt die Top 15 Abweichungen
  write.csv2(vergleich_gewichte, "data/Gewichtevergleich_MVP_2025.csv", row.names = FALSE)
  cat("Der Vergleich wurde als 'Gewichtevergleich_MVP_2025.csv' gespeichert!\n")
  
} else {
  # Prüfung ob na.omit möglich
  cat("\n=========================================================================\n")
  cat("ACHTUNG: Der Gewichtevergleich konnte nicht durchgeführt werden!\n")
  cat("Grund: Die na.omit Matrix für 2025 ist mathematisch kollabiert (nicht positiv definit).\n")
  cat("Complete-Case Analysis hier nicht anwendbar ist!\n")
  cat("=========================================================================\n")
}






