# =============================================================================
# 10_revision_figures.R   (revisión JCP, 2026-09)
#   Fig5_rev_regression_coefs  — M2 con IC 95% CR2 (Satterthwaite); MERCOSUR y
#                                 neto UE (baseline + offset), en vez del offset
#                                 (R3#4: "ambiguous" columnas / grupo base).
#   FigS4_robustness_forest    — magnitudes e IC en todas las especificaciones (R2#6)
#   FigS5_global_gap           — gap_rel vs RTB/MF (R1) y distribución global (R2#8)
# =============================================================================

suppressMessages({library(tidyverse); library(patchwork); library(ggrepel)})

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")
FIGS <- file.path(BASE, "output/figures")
source(file.path(BASE, "R", "_paper_theme.R"))

COLS <- list(EU = "#2166AC", MERCOSUR = "#D6604D")
TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")

# --- Fig 5 (rev) ---------------------------------------------------------------
m2 <- read_csv(file.path(TABS, "regression_m2_cr2_all.csv"), show_col_types = FALSE) %>%
  filter(term %in% TAPIO_LEVELS | str_detect(term, ":net_EU$")) %>%
  mutate(state  = factor(str_remove(term, ":net_EU$"), levels = TAPIO_LEVELS),
         series = if_else(str_detect(term, "net_EU"), "EU (net: MERCOSUR + difference)",
                          "MERCOSUR (reference group)"),
         series = factor(series, levels = c("MERCOSUR (reference group)",
                                            "EU (net: MERCOSUR + difference)")),
         sig    = if_else(p_cr2 < 0.05, "CR2 p < 0.05", "CR2 p ≥ 0.05"))

fig5 <- ggplot(m2, aes(state, est, colour = series, shape = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(ymin = lo, ymax = hi), position = position_dodge(width = 0.5),
                  linewidth = 0.7) +
  scale_colour_manual(values = c("MERCOSUR (reference group)" = COLS$MERCOSUR,
                                 "EU (net: MERCOSUR + difference)" = COLS$EU)) +
  scale_shape_manual(values = c("CR2 p < 0.05" = 16, "CR2 p ≥ 0.05" = 1)) +
  labs(x = "Tapio decoupling state", y = "Δgap_rel (pp), relative to EC",
       colour = NULL, shape = NULL) +
  guides(colour = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) +
  theme_paper()
save_fig(fig5, "Fig5_rev_regression_coefs", h = 5.2)

# --- Fig S4: forest plot de robustez --------------------------------------------
tau_f <- file.path(TABS, "regression_tau_robustness.csv")   # generado por 11_revision_r3.R
tau_rows <- if (file.exists(tau_f)) {
  read_csv(tau_f, show_col_types = FALSE) %>% filter(tau != 0.2) %>%
    mutate(spec = paste0("Tapio tolerance τ = ", tau), units = "pp")
} else NULL
mag <- read_csv(file.path(TABS, "regression_robustness_magnitudes.csv"), show_col_types = FALSE) %>%
  bind_rows(tau_rows) %>%
  filter(units == "pp", term %in% c("SD", "net_EU_SD")) %>%
  mutate(term = recode(term, SD = "MERCOSUR strong decoupling",
                       net_EU_SD = "EU strong decoupling (net)"),
         spec = factor(spec, levels = rev(unique(spec))))
figS4 <- ggplot(mag, aes(est, spec, colour = term)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c("MERCOSUR strong decoupling" = COLS$MERCOSUR,
                                 "EU strong decoupling (net)" = COLS$EU)) +
  labs(x = "Coefficient (pp) with 95% CR2 (Satterthwaite) interval", y = NULL, colour = NULL) +
  theme_paper()
save_fig(figS4, "FigS4_robustness_forest", h = 6.2)

# --- Fig S5: global -------------------------------------------------------------
gp <- read_csv(file.path(PROC, "global_gap_panel.csv"), show_col_types = FALSE) %>%
  filter(year == 2021, !is.na(income)) %>%
  mutate(income = factor(income, levels = c("Low income", "Lower middle income",
                                            "Upper middle income", "High income")),
         sample = case_when(iso3 %in% c("ARG", "BRA", "PRY", "URY") ~ "MERCOSUR",
                            iso3 %in% c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
                                        "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
                                        "POL","PRT","ROU","SVK","SVN","ESP","SWE") ~ "EU",
                            TRUE ~ "Other")) %>%
  filter(!is.na(income), abs(gap_rel) < 3, abs(rtb_sh) < 3)
pA <- ggplot(gp, aes(rtb_sh, gap_rel)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey60", linetype = "dotted") +
  geom_hline(yintercept = 0, colour = "grey80") + geom_vline(xintercept = 0, colour = "grey80") +
  geom_point(data = filter(gp, sample == "Other"), colour = "grey70", size = 1.3) +
  geom_point(data = filter(gp, sample != "Other"), aes(colour = sample), size = 1.8) +
  geom_text_repel(data = filter(gp, iso3 %in% c("MLT", "IRL", "CYP", "URY", "NLD", "BGR")),
                  aes(label = iso3), size = LBL * 0.8, min.segment.length = 0) +
  scale_colour_manual(values = c(EU = COLS$EU, MERCOSUR = COLS$MERCOSUR)) +
  labs(x = "RTB/MF (net raw-material-equivalent imports / MF)",
       y = "gap_rel = (MF − DMC)/MF", colour = NULL, title = "A") +
  theme_paper()
pB <- ggplot(gp, aes(income, gap_rel)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_boxplot(outlier.size = 0.8, fill = "grey92") +
  scale_x_discrete(labels = c("Low", "Lower-\nmiddle", "Upper-\nmiddle", "High")) +
  labs(x = "World Bank income group", y = "gap_rel", title = "B") +
  theme_paper()
figS5 <- pA + pB + plot_layout(widths = c(1.4, 1))
save_fig(figS5, "FigS5_global_gap", h = 4.6)

cat("10_revision_figures.R completado.\n")
