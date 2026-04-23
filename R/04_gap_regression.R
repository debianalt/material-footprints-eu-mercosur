# =============================================================================
# 04_gap_regression.R
# Regresión de panel + análisis de flujos comerciales EU-MERCOSUR (COMTRADE)
#
# Variable dependiente: Δgap_rel = cambio anual en (MF-DMC)/MF
# Todos los coeficientes en "puntos porcentuales de share" (adimensional × 100)
# =============================================================================

library(tidyverse)
library(fixest)
library(broom)

BASE <- "C:/Users/ant/OneDrive/articles_1_/material footprints/new submision EE"
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")
RAW  <- file.path(BASE, "data/raw")

panel_reg <- read_csv(file.path(PROC, "panel_regimes.csv"), show_col_types = FALSE)

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "RD", "RC", "RND", "SND")

# -----------------------------------------------------------------------------
# 1. Construir variable dependiente y controles
# -----------------------------------------------------------------------------

panel_reg <- panel_reg %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    d_gap_rel   = (gap_rel - lag(gap_rel)) * 100,  # en puntos porcentuales
    tapio_f     = factor(tapio, levels = TAPIO_LEVELS),
    eu_dummy    = as.integer(bloc == "EU"),
    d_log_mf    = log(mf_mt / lag(mf_mt)) * 100
  ) %>%
  ungroup() %>%
  filter(!is.na(d_gap_rel), !is.na(tapio_f))

cat("Observaciones para regresión:", nrow(panel_reg), "\n")
cat("Países:", n_distinct(panel_reg$iso3), "\n")
cat("Años:", n_distinct(panel_reg$year), "\n")

# Nota importante: el coeficiente de SD sobre Δgap_rel es en parte
# mecánico (SD implica que MF cae, lo que mecánicamente reduce el gap
# si DMC es estable). El análisis se presenta como "confirmación descriptiva
# de coherencia entre métricas", no como identificación causal.

# -----------------------------------------------------------------------------
# 2. Modelo principal — Efectos fijos país + año, SE clustered por país
# -----------------------------------------------------------------------------

# M1: solo estados Tapio, sin interacción
m1 <- feols(
  d_gap_rel ~ i(tapio_f, ref = "EC") | iso3 + year,
  cluster = ~iso3,
  data    = panel_reg
)

# M2: con interacción EU (prueba si la relación difiere entre bloques)
m2 <- feols(
  d_gap_rel ~ i(tapio_f, ref = "EC") +
              i(tapio_f, eu_dummy, ref = "EC") | iso3 + year,
  cluster = ~iso3,
  data    = panel_reg
)

# M3: indicador compuesto envPlus (estados ambientalmente favorables)
panel_reg <- panel_reg %>%
  mutate(env_plus = as.integer(tapio %in% c("SD", "RD", "RC", "RND")))

m3 <- feols(
  d_gap_rel ~ env_plus + eu_dummy:env_plus | iso3 + year,
  cluster = ~iso3,
  data    = panel_reg
)

# Mostrar resultados
cat("\n=== M1: Tapio states → Δgap_rel ===\n")
print(summary(m1))

cat("\n=== M2: Tapio × EU interaction ===\n")
print(summary(m2))

cat("\n=== M3: envPlus composite ===\n")
print(summary(m3))

# Tabla de resultados formateada
etable(m1, m2, m3,
       digits    = 3,
       se.below  = TRUE,
       tex       = FALSE,
       file      = file.path(TABS, "regression_main.txt"))

# Exportar coeficientes para ggplot
coef_m2 <- tidy(m2, conf.int = TRUE) %>%
  mutate(model = "M2_interaction")
coef_m1 <- tidy(m1, conf.int = TRUE) %>%
  mutate(model = "M1_main")

bind_rows(coef_m1, coef_m2) %>%
  write_csv(file.path(TABS, "regression_coefficients.csv"))

# -----------------------------------------------------------------------------
# 3. Robustez: solo EU / solo MERCOSUR por separado
# -----------------------------------------------------------------------------

m_eu <- feols(
  d_gap_rel ~ i(tapio_f, ref = "EC") | iso3 + year,
  cluster = ~iso3,
  data    = panel_reg %>% filter(bloc == "EU")
)

m_mcs <- feols(
  d_gap_rel ~ i(tapio_f, ref = "EC") | iso3 + year,
  cluster = ~iso3,
  data    = panel_reg %>% filter(bloc == "MERCOSUR")
)

cat("\n=== Robustez EU solamente ===\n"); print(summary(m_eu))
cat("\n=== Robustez MERCOSUR solamente ===\n"); print(summary(m_mcs))

bind_rows(
  tidy(m_eu,  conf.int = TRUE) %>% mutate(model = "EU_only"),
  tidy(m_mcs, conf.int = TRUE) %>% mutate(model = "MERCOSUR_only")
) %>%
  write_csv(file.path(TABS, "regression_robustness.csv"))

# -----------------------------------------------------------------------------
# 4. Datos COMTRADE — flujos comerciales EU↔MERCOSUR
#
# REQUIERE API KEY de UN COMTRADE (gratuita):
#   1. Registrarse en https://comtradeplus.un.org/
#   2. Obtener API key (plan gratuito: 500 requests/mes)
#   3. Ejecutar: comtradr::set_primary_comtrade_key("TU_API_KEY")
#      (o poner la key en .Renviron como COMTRADE_PRIMARY=tu_key)
#
# Si no tenés la key, usamos el fallback con el archivo manual
# -----------------------------------------------------------------------------

comtrade_path <- file.path(RAW, "comtrade_eu_mercosur.csv")

if (!file.exists(comtrade_path)) {

  api_key <- Sys.getenv("COMTRADE_PRIMARY")

  if (nchar(api_key) > 0 && requireNamespace("comtradr", quietly = TRUE)) {
    library(comtradr)
    cat("\nDescargando datos COMTRADE...\n")

    # Reporteros: EU-27 agregado (puede ser "EU" en COMTRADE)
    # Partners: Argentina (ARG), Brasil (BRA), Paraguay (PRY), Uruguay (URY)
    # Flow: M (imports) - perspectiva EU importando desde MERCOSUR
    # Commodity: Total (AG6 = 'TOTAL') + HS secciones I-V (materias primas)

    tryCatch({
      ct_total <- ct_get_data(
        reporter  = "EU",
        partner   = c("ARG", "BRA", "PRY", "URY"),
        flow_direction = "import",
        commodity_code = "TOTAL",
        start_date = "2000",
        end_date   = "2023"
      )

      # Materias primas: HS capítulos 01-27 (animales, vegetales, minerales, energía)
      ct_primary <- ct_get_data(
        reporter  = "EU",
        partner   = c("ARG", "BRA", "PRY", "URY"),
        flow_direction = "import",
        commodity_code = c("01","02","03","04","05","06","07","08","09","10",
                           "11","12","13","14","15","16","17","18","19","20",
                           "21","22","23","24","25","26","27"),
        start_date = "2000",
        end_date   = "2023"
      )

      comtrade <- bind_rows(
        ct_total   %>% mutate(category = "Total"),
        ct_primary %>% mutate(category = "Primary commodities (HS 01-27)")
      ) %>%
        select(year = period, partner = partnerDesc, category,
               trade_value_usd = primaryValue) %>%
        group_by(year, category) %>%
        summarise(trade_value_usd = sum(trade_value_usd, na.rm = TRUE), .groups = "drop")

      write_csv(comtrade, comtrade_path)
      cat("Datos COMTRADE guardados:", nrow(comtrade), "obs\n")

    }, error = function(e) {
      cat("Error en COMTRADE API:", conditionMessage(e), "\n")
      cat("Ver instrucciones en el script para configurar la API key.\n")
      cat("Alternativamente, descargar manualmente desde:\n")
      cat("https://comtradeplus.un.org/TradeFlow\n")
      cat("y guardar como data/raw/comtrade_eu_mercosur.csv\n")
    })

  } else {
    cat("\n--- COMTRADE API key no encontrada ---\n")
    cat("Para obtener datos de flujos comerciales:\n")
    cat("  1. Registrarse en https://comtradeplus.un.org/\n")
    cat("  2. Agregar a .Renviron: COMTRADE_PRIMARY=tu_api_key\n")
    cat("  3. Re-ejecutar este script\n\n")
    cat("Alternativa: descargar manualmente y guardar como:\n")
    cat("  data/raw/comtrade_eu_mercosur.csv\n")
    cat("  Columnas: year, category, trade_value_usd\n\n")
    cat("El análisis de COMTRADE se saltea por ahora.\n")
  }
}

# Si el archivo existe (descargado o manual), procesarlo
if (file.exists(comtrade_path)) {
  comtrade <- read_csv(comtrade_path, show_col_types = FALSE)

  comtrade_summary <- comtrade %>%
    mutate(trade_bn_usd = trade_value_usd / 1e9) %>%
    group_by(year, category) %>%
    summarise(trade_bn_usd = sum(trade_bn_usd, na.rm = TRUE), .groups = "drop")

  write_csv(comtrade_summary, file.path(TABS, "comtrade_summary.csv"))

  # Correlación: años de SD alto en EU vs. volumen de importaciones desde MERCOSUR
  eu_sd_annual <- read_csv(file.path(TABS, "tapio_rolling_shares.csv"),
                           show_col_types = FALSE) %>%
    filter(bloc == "EU") %>%
    select(year, share_SD)

  corr_data <- comtrade_summary %>%
    filter(category == "Primary commodities (HS 01-27)") %>%
    inner_join(eu_sd_annual, by = "year")

  if (nrow(corr_data) > 5) {
    corr_test <- cor.test(corr_data$share_SD, corr_data$trade_bn_usd,
                          method = "pearson")
    cat("\nCorrelación EU SD-share vs importaciones primarias desde MERCOSUR:\n")
    print(corr_test)
    write_csv(corr_data, file.path(TABS, "sd_comtrade_correlation.csv"))
  }

  cat("Análisis COMTRADE completado.\n")
}

cat("\n04_gap_regression.R completado.\n")
