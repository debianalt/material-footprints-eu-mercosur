# =============================================================================
# 05_figures.R
# Todas las figuras del paper — ggplot2, exportadas en TIFF 300dpi
# =============================================================================

library(tidyverse)
library(patchwork)
library(scales)
library(countrycode)

# BASE derivado de la ubicación de este script (rename-proof)
.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")
FIGS <- file.path(BASE, "output/figures")

# Tema + exportación unificados (theme_paper(), LBL, WIDTH_IN, save_fig())
source(file.path(BASE, "R", "_paper_theme.R"))

# Paleta de colores consistente en todo el paper
COLS <- list(
  EU       = "#2166AC",
  MERCOSUR = "#D6604D",
  SD       = "#1A9850",
  END      = "#D73027",
  WD       = "#74C476",
  EC       = "#FEE08B",
  neutral  = "#969696"
)

TAPIO_COLS <- c(
  "SD"  = "#1A9850", "WD"  = "#74C476", "EC"  = "#FEE08B",
  "END" = "#D73027", "RD"  = "#A6D96A", "RC"  = "#FDAE61",
  "RND" = "#F46D43", "SND" = "#A50026"
)

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "RD", "RC", "RND", "SND")
TAPIO_LABELS <- c("Strong\nDecoupling", "Weak\nDecoupling", "Expansive\nCoupling",
                  "Expansive Neg.\nDecoupling", "Recessive\nDecoupling",
                  "Recessive\nCoupling", "Recessive Neg.\nDecoupling",
                  "Strong Neg.\nDecoupling")

# save_fig() provisto por _paper_theme.R (ancho fijo 190mm, TIFF+PNG 300dpi)

# Cargar datos
panel_tapio  <- read_csv(file.path(PROC, "panel_tapio.csv"),   show_col_types = FALSE)
panel_reg    <- read_csv(file.path(PROC, "panel_regimes.csv"), show_col_types = FALSE)
state_dist   <- read_csv(file.path(TABS, "tapio_state_distribution.csv"), show_col_types = FALSE)
rolling      <- read_csv(file.path(TABS, "tapio_rolling_shares.csv"), show_col_types = FALSE)
regime_sum   <- read_csv(file.path(TABS, "hmm_regime_summary.csv"), show_col_types = FALSE)
model_sel    <- read_csv(file.path(TABS, "hmm_model_selection.csv"), show_col_types = FALSE)
trans_mat    <- read_csv(file.path(TABS, "hmm_transition_matrix.csv"), show_col_types = FALSE)
emission_mat <- read_csv(file.path(TABS, "hmm_emission_matrix.csv"), show_col_types = FALSE)
cty_summ     <- read_csv(file.path(TABS, "tapio_country_summary.csv"), show_col_types = FALSE)
reg_coefs    <- read_csv(file.path(TABS, "regression_coefficients.csv"), show_col_types = FALSE)

# =============================================================================
# FIG 1: Distribución de estados Tapio por bloque
# =============================================================================

fig1 <- state_dist %>%
  mutate(tapio = factor(tapio, levels = TAPIO_LEVELS, labels = TAPIO_LABELS)) %>%
  ggplot(aes(x = tapio, y = pct, fill = bloc, color = bloc)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(width = 0.7), width = 0.2, linewidth = 0.5) +
  scale_fill_manual(values  = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  scale_color_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  labs(
    x = NULL, y = "Frequency (%)",
    fill = NULL, color = NULL
  ) +
  theme_paper() +
  theme(panel.grid.major.x = element_blank())

save_fig(fig1, "Fig1_tapio_distribution", h = 5.0)

# =============================================================================
# FIG 2: Rolling 5-year shares SD y END por bloque
# =============================================================================

fig2_data <- rolling %>%
  select(bloc, year, share_SD, share_END) %>%
  pivot_longer(c(share_SD, share_END), names_to = "indicator", values_to = "share") %>%
  mutate(
    indicator = recode(indicator, share_SD = "Strong Decoupling",
                                  share_END = "Expansive Neg. Decoupling")
  )

# Color EU/MERCOSUR sin leyenda (los strips ya titulan los paneles); la única
# leyenda es linetype, con claves largas para que el guionado sea visible.
fig2 <- fig2_data %>%
  ggplot(aes(x = year, y = share, color = bloc, linetype = indicator)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.2, alpha = 0.6) +
  facet_wrap(~bloc, ncol = 2) +
  scale_color_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR),
                     guide = "none") +
  scale_linetype_manual(values = c("Strong Decoupling" = "solid",
                                    "Expansive Neg. Decoupling" = "dashed")) +
  labs(
    x = NULL, y = "5-year rolling share (%)",
    linetype = NULL
  ) +
  theme_paper() +
  theme(legend.key.width = unit(2.5, "lines"))

save_fig(fig2, "Fig2_rolling_shares", h = 4.8)

# =============================================================================
# FIG 3: Selección de modelo HMM — BIC/AIC por K (NUEVA — responde R3.4)
# =============================================================================

fig3 <- model_sel %>%
  select(K, AIC, BIC, ICL) %>%
  pivot_longer(c(AIC, BIC, ICL), names_to = "criterion", values_to = "value") %>%
  ggplot(aes(x = K, y = value, color = criterion, shape = criterion)) +
  geom_line(linewidth = 1) +
  geom_point(size = 4) +
  scale_x_continuous(breaks = 2:5) +
  scale_color_manual(values = c(AIC = "#2166AC", BIC = "#D6604D", ICL = "#4DAC26")) +
  labs(
    x = "Number of latent regimes (K)",
    y = "Information criterion value",
    color = NULL, shape = NULL
  ) +
  theme_paper()

save_fig(fig3, "Fig3_hmm_model_selection", h = 4.8)

# =============================================================================
# FIG 4: Regime shares por bloque con CIs bootstrap
# =============================================================================

fig5_data <- regime_sum %>%
  select(bloc, regime_viterbi, dominant_tapio, pct, ci_lower, ci_upper, mean_dwell_time) %>%
  filter(!is.na(pct)) %>%
  mutate(
    regime_label = paste0("Regime ", regime_viterbi,
                          "\n(", dominant_tapio, ")"),
    dwell_label  = paste0("Dwell: ", round(mean_dwell_time, 1), " yr")
  )

fig5 <- fig5_data %>%
  ggplot(aes(x = factor(regime_viterbi), y = pct, fill = bloc)) +
  geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.5) +
  scale_fill_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  labs(
    x = "Latent regime", y = "Posterior-weighted share (%)",
    fill = NULL
  ) +
  theme_paper()

save_fig(fig5, "Fig5_regime_shares", h = 5.0)

# =============================================================================
# FIG 5: Heatmap de probabilidades de transición
# =============================================================================

fig4 <- trans_mat %>%
  ggplot(aes(x = to, y = from, fill = prob)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", prob)), size = LBL) +
  scale_fill_gradient2(low = "white", mid = "#fee090", high = "#d73027",
                       midpoint = 0.3, limits = c(0, 1),
                       name = "Transition\nprobability") +
  labs(x = "To regime", y = "From regime") +
  theme_paper() +
  theme(panel.grid = element_blank(),
        legend.position = "right",
        legend.key.height = unit(1.4, "lines"))

save_fig(fig4, "Fig4_transition_heatmap", h = 5.2)

# =============================================================================
# FIG 6: Trayectorias del gap de externalización 1994-2024
# =============================================================================

gap_annual <- panel_tapio %>%
  group_by(bloc, year) %>%
  summarise(
    gap_median = median(gap_rel, na.rm = TRUE),
    gap_q25    = quantile(gap_rel, 0.25, na.rm = TRUE),
    gap_q75    = quantile(gap_rel, 0.75, na.rm = TRUE),
    .groups    = "drop"
  )

fig6 <- gap_annual %>%
  ggplot(aes(x = year, y = gap_median, color = bloc, fill = bloc)) +
  geom_ribbon(aes(ymin = gap_q25, ymax = gap_q75), alpha = 0.15, color = NA) +
  geom_line(linewidth = 1.1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
  scale_color_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  scale_fill_manual(values  = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = NULL, y = "Externalisation gap [(MF−DMC)/MF]",
    color = NULL, fill = NULL
  ) +
  theme_paper()

save_fig(fig6, "Fig6_gap_trajectories", h = 5.0)

# =============================================================================
# FIG 7: Scatter eficiencia vs. gap (cuadrantes con medianas muestrales)
#
# Umbrales = medianas muestrales → completamente reproducibles y justificables
# Responde crítica del Reviewer 3 sobre umbrales arbitrarios (14%/47%)
# =============================================================================

threshold_x <- median(cty_summ$pct_R3,  na.rm = TRUE)  # % tiempo en SD+WD
threshold_y <- median(cty_summ$med_gap, na.rm = TRUE)  # mediana del gap relativo

# Etiquetas ISO a nombres cortos
cty_summ <- cty_summ %>%
  mutate(
    label = countrycode(iso3, "iso3c", "iso2c"),
    label = if_else(is.na(label), iso3, label)
  )

# ggrepel necesario para etiquetas sin solapamiento
if (!requireNamespace("ggrepel", quietly = TRUE)) {
  install.packages("ggrepel", repos = "https://cloud.r-project.org")
  library(ggrepel)
} else {
  library(ggrepel)
}

fig7 <- cty_summ %>%
  ggplot(aes(x = pct_R3, y = med_gap, color = bloc, label = label)) +
  annotate("rect", xmin = threshold_x, xmax = Inf, ymin = threshold_y, ymax = Inf,
           fill = "#EFF3FF", alpha = 0.5) +
  annotate("rect", xmin = -Inf, xmax = threshold_x, ymin = -Inf, ymax = threshold_y,
           fill = "#FEE5D9", alpha = 0.5) +
  geom_vline(xintercept = threshold_x, linetype = "dotted", color = "grey40") +
  geom_hline(yintercept = threshold_y, linetype = "dotted", color = "grey40") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_point(size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(size = LBL - 0.6, max.overlaps = 20,
                           min.segment.length = 0, seed = 42, show.legend = FALSE) +
  scale_color_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    x = "Time in efficiency-oriented regimes (SD+WD), % of years 1995–2024",
    y = "Median externalisation gap [(MF−DMC)/MF]",
    color = NULL
  ) +
  theme_paper()

save_fig(fig7, "Fig7_typology_scatter", h = 6.8)

# Guardar umbrales usados (para reportar en el paper)
write_csv(
  tibble(threshold_x_pct = threshold_x,
         threshold_y_gap  = threshold_y,
         note = "Sample medians. Fully reproducible from panel_tapio.csv."),
  file.path(TABS, "fig7_quadrant_thresholds.csv")
)

# =============================================================================
# FIG 9: Generada por 06_comtrade.R (4-panel figure con desglose por país y categoría)
# =============================================================================
cat("Fig9: generada por R/06_comtrade.R — omitida aquí para evitar duplicación.\n")

# =============================================================================
# FIG 8: Coeficientes de regresión con intervalos de confianza
# =============================================================================

fig8_data <- reg_coefs %>%
  filter(model == "M2_interaction") %>%
  filter(str_detect(term, "tapio_f")) %>%
  mutate(
    state      = str_extract(term, "SD|WD|EC|END|RD|RC|RND|SND"),
    state      = factor(state, levels = TAPIO_LEVELS),
    series     = if_else(str_detect(term, "eu_dummy"),
                         "EU (offset)", "MERCOSUR (baseline)"),
    series     = factor(series, levels = c("MERCOSUR (baseline)",
                                            "EU (offset)")),
    sig        = if_else(p.value < 0.05, "CI excludes 0", "CI includes 0")
  ) %>%
  filter(!is.na(state))

fig8 <- fig8_data %>%
  ggplot(aes(x = state, y = estimate, color = series, shape = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.45), linewidth = 0.7) +
  scale_color_manual(values = c("MERCOSUR (baseline)" = COLS$MERCOSUR,
                                 "EU (offset)"         = COLS$EU)) +
  scale_shape_manual(values = c("CI excludes 0" = 16, "CI includes 0" = 1)) +
  labs(
    x = "Tapio decoupling state",
    y = "Coefficient (pp, vs. EC)",
    color = NULL, shape = NULL
  ) +
  theme_paper()

save_fig(fig8, "Fig8_regression_coefs", h = 5.0)

# =============================================================================
# FIG 10: Robustez HMMs separados (Supplementary Material)
# =============================================================================

robustness_path <- file.path(TABS, "hmm_robustness_summary.csv")
if (file.exists(robustness_path)) {
  robust_sum <- read_csv(robustness_path, show_col_types = FALSE)

  fig10 <- robust_sum %>%
    filter(model %in% c("EU_only", "MERCOSUR_only")) %>%
    ggplot(aes(x = factor(regime), y = mean_dwell_time, fill = model)) +
    geom_col(position = position_dodge(0.7), width = 0.6, alpha = 0.85) +
    scale_fill_manual(values = c(EU_only       = COLS$EU,
                                  MERCOSUR_only = COLS$MERCOSUR),
                      labels = c(EU_only = "EU (separate HMM)",
                                 MERCOSUR_only = "MERCOSUR (separate HMM)")) +
    labs(
      x = "Regime", y = "Mean dwell time (years)",
      fill = NULL
    ) +
    theme_paper()

  save_fig(fig10, "Fig10_S_robustness_hmm", h = 4.8)
}

cat("\n05_figures.R completado.\n")
cat("Figuras guardadas en:", FIGS, "\n")
