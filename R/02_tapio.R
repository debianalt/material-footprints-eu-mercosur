# =============================================================================
# 02_tapio.R
# Clasificación de Tapio (8 estados) y análisis descriptivo
# =============================================================================

library(tidyverse)

# BASE derivado de la ubicación de este script (rename-proof)
.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

panel <- read_csv(file.path(PROC, "panel_main.csv"), show_col_types = FALSE)

# -----------------------------------------------------------------------------
# 1. Función de clasificación de Tapio
#    τ = 0.2 (banda de tolerancia estándar en la literatura)
#
#    Fuente: Tapio (2005), implementada exactamente como en Pothen & Welsch
#    (2019) y el análisis original.
#
#    Los 8 estados del cuadro de Tapio:
#    SD  = Strong Decoupling       (gGDP > 0, gMF < 0)
#    WD  = Weak Decoupling         (gGDP > 0, gMF >= 0, ε < 1-τ)
#    EC  = Expansive Coupling      (gGDP > 0, |ε-1| ≤ τ)
#    END = Expansive Neg. Decoupl. (gGDP > 0, ε > 1+τ)
#    SND = Strong Neg. Decoupling  (gGDP < 0, gMF > 0)
#    RD  = Recessive Decoupling    (gGDP < 0, gMF ≤ 0, ε < 1-τ)
#    RC  = Recessive Coupling      (gGDP < 0, |ε-1| ≤ τ)
#    RND = Recessive Neg. Decoupl. (gGDP < 0, gMF ≤ 0, ε > 1+τ) [WAS missing in JS: e < 0.8 = RD]
# -----------------------------------------------------------------------------

TAU <- 0.2

tapio_classify <- function(g_mf, g_gdp, tau = TAU) {
  # Manejo de casos extremos
  if (is.na(g_mf) || is.na(g_gdp)) return(NA_character_)
  if (!is.finite(g_mf) || !is.finite(g_gdp)) return(NA_character_)

  if (g_gdp == 0) {
    return(if (g_mf > 0) "END" else "RD")
  }

  e <- g_mf / g_gdp

  if (g_gdp > 0) {
    if (g_mf < 0)             return("SD")
    if (e < 1 - tau)          return("WD")
    if (abs(e - 1) <= tau)    return("EC")
    return("END")
  } else {
    if (g_mf > 0)             return("SND")
    if (e < 1 - tau)          return("RD")
    if (abs(e - 1) <= tau)    return("RC")
    return("RND")
  }
}
tapio_classify <- Vectorize(tapio_classify)

# Estado de referencia para regresiones (el más neutro) = "EC"
TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "RD", "RC", "RND", "SND")

# -----------------------------------------------------------------------------
# 2. Calcular tasas de crecimiento y clasificar
# -----------------------------------------------------------------------------

panel_tapio <- panel %>%
  group_by(iso3) %>%
  arrange(year) %>%
  mutate(
    g_mf  = (mf_mt  - lag(mf_mt))  / lag(mf_mt),
    g_gdp = (gdp_2015usd - lag(gdp_2015usd)) / lag(gdp_2015usd),
    elasticity = if_else(g_gdp != 0, g_mf / g_gdp, NA_real_),
    tapio = tapio_classify(g_mf, g_gdp, tau = TAU),
    tapio = factor(tapio, levels = TAPIO_LEVELS)
  ) %>%
  ungroup() %>%
  filter(!is.na(tapio))

cat("Panel con estados Tapio:", nrow(panel_tapio), "obs\n")
cat("Distribución de estados:\n")
print(table(panel_tapio$tapio, useNA = "ifany"))

# -----------------------------------------------------------------------------
# 3. Distribución de estados por bloque — Tabla 1 del paper
# -----------------------------------------------------------------------------

state_dist <- panel_tapio %>%
  count(bloc, tapio, name = "n") %>%
  group_by(bloc) %>%
  mutate(
    pct      = n / sum(n) * 100,
    se       = sqrt(pct * (100 - pct) / sum(n)),  # SE para la proporción
    ci_lower = pmax(pct - 1.96 * se, 0),
    ci_upper = pmin(pct + 1.96 * se, 100)
  ) %>%
  ungroup()

# Test chi-cuadrado: ¿difieren las distribuciones EU vs MERCOSUR?
chisq_data <- panel_tapio %>%
  count(bloc, tapio) %>%
  pivot_wider(names_from = bloc, values_from = n, values_fill = 0) %>%
  column_to_rownames("tapio") %>%
  as.matrix()

chisq_res <- chisq.test(chisq_data)
cat("\nChi-cuadrado EU vs MERCOSUR:\n")
print(chisq_res)

# Mann-Whitney: diferencia en elasticidades
mw_res <- wilcox.test(
  elasticity ~ bloc,
  data   = panel_tapio %>% filter(!is.na(elasticity)),
  exact  = FALSE
)
cat("\nMann-Whitney elasticidades:\n")
print(mw_res)

# Medianas de elasticidad
panel_tapio %>%
  group_by(bloc) %>%
  summarise(med_elasticity = median(elasticity, na.rm = TRUE)) %>%
  print()

write_csv(state_dist, file.path(TABS, "tapio_state_distribution.csv"))

# -----------------------------------------------------------------------------
# 4. Rolling 5-year shares de SD y END
# -----------------------------------------------------------------------------

rolling_shares <- panel_tapio %>%
  group_by(bloc, year) %>%
  summarise(
    n_total = n(),
    n_SD    = sum(tapio == "SD"),
    n_END   = sum(tapio == "END"),
    .groups = "drop"
  ) %>%
  group_by(bloc) %>%
  arrange(year) %>%
  mutate(
    # Rolling sum over 5-year window (current + 4 prior years)
    roll_n     = zoo::rollapply(n_total, 5, sum, fill = NA, align = "right"),
    roll_SD    = zoo::rollapply(n_SD,    5, sum, fill = NA, align = "right"),
    roll_END   = zoo::rollapply(n_END,   5, sum, fill = NA, align = "right"),
    share_SD   = roll_SD  / roll_n * 100,
    share_END  = roll_END / roll_n * 100
  ) %>%
  filter(!is.na(share_SD)) %>%
  ungroup()

if (!"zoo" %in% installed.packages()[, 1]) {
  install.packages("zoo", repos = "https://cloud.r-project.org")
}
library(zoo)

# Re-calcular con zoo cargado
rolling_shares <- panel_tapio %>%
  group_by(bloc, year) %>%
  summarise(n_total = n(), n_SD = sum(tapio == "SD"),
            n_END = sum(tapio == "END"), .groups = "drop") %>%
  group_by(bloc) %>%
  arrange(year) %>%
  mutate(
    roll_n   = rollapply(n_total, 5, sum, fill = NA, align = "right"),
    roll_SD  = rollapply(n_SD,   5, sum, fill = NA, align = "right"),
    roll_END = rollapply(n_END,  5, sum, fill = NA, align = "right"),
    share_SD  = roll_SD  / roll_n * 100,
    share_END = roll_END / roll_n * 100
  ) %>%
  filter(!is.na(share_SD)) %>%
  ungroup()

write_csv(rolling_shares, file.path(TABS, "tapio_rolling_shares.csv"))

# -----------------------------------------------------------------------------
# 5. Resumen por país (para Figura 7 scatter)
# -----------------------------------------------------------------------------

country_summary <- panel_tapio %>%
  group_by(iso3, bloc) %>%
  summarise(
    n_obs      = n(),
    pct_SD     = mean(tapio == "SD") * 100,
    pct_END    = mean(tapio == "END") * 100,
    pct_R3     = mean(tapio %in% c("SD", "WD")) * 100,  # efficiency-oriented
    med_gap    = median(gap_rel, na.rm = TRUE),
    mean_gap   = mean(gap_rel, na.rm = TRUE),
    med_elast  = median(elasticity, na.rm = TRUE),
    .groups    = "drop"
  )

write_csv(country_summary, file.path(TABS, "tapio_country_summary.csv"))
write_csv(panel_tapio,     file.path(PROC, "panel_tapio.csv"))

cat("\n02_tapio.R completado.\n")
