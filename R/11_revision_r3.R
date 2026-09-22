# =============================================================================
# 11_revision_r3.R   (revisión JCP, 2026-09) — pedidos específicos del Reviewer 3
#   §3.3  robustez a la tolerancia de Tapio τ ∈ {0.1, 0.2, 0.3}
#   §4.1  estadísticos descriptivos por país (media, DE, mín, máx de gap_rel; % SD+WD)
#   §4.3  evidencia descriptiva del mecanismo: crecimiento de MF, DMC, DE y
#         exportaciones RME en años SD vs. resto, por bloque
# =============================================================================

suppressMessages({library(tidyverse); library(clubSandwich)})

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")
NONREF <- setdiff(TAPIO_LEVELS, "EC")
source(file.path(BASE, "R", "_inference_helpers.R"))

# Misma regla que 02_tapio.R (tapio_classify), vectorizada y con τ variable
tapio_tau <- function(g_mf, g_gdp, tau) {
  e <- g_mf / g_gdp
  case_when(
    is.na(g_mf) | is.na(g_gdp) ~ NA_character_,
    g_gdp == 0 & g_mf > 0 ~ "END", g_gdp == 0 ~ "WND",
    g_gdp > 0 & g_mf < 0 ~ "SD",
    g_gdp > 0 & e < 1 - tau ~ "WD",
    g_gdp > 0 & abs(e - 1) <= tau ~ "EC",
    g_gdp > 0 ~ "END",
    g_mf > 0 ~ "SND",
    e < 1 - tau ~ "WND",
    abs(e - 1) <= tau ~ "RC",
    TRUE ~ "RD")
}

pc <- read_csv(file.path(PROC, "panel_components.csv"), show_col_types = FALSE)
d <- pc %>% group_by(iso3) %>% arrange(year) %>%
  mutate(d_gap_rel = (gap_rel - lag(gap_rel)) * 100) %>% ungroup() %>%
  filter(!is.na(d_gap_rel), !is.na(tapio)) %>%
  mutate(eu = as.integer(bloc == "EU"))
stopifnot(nrow(d) == 899)
stopifnot(all(tapio_tau(d$g_mf, d$g_gdp, 0.2) == d$tapio))   # reproduce la clasificación

# --- §3.3 τ ---------------------------------------------------------------------
tau_rob <- map_dfr(c(0.1, 0.2, 0.3), function(tau) {
  dd <- d %>% mutate(tapio = tapio_tau(g_mf, g_gdp, tau))
  fit_cr2(dd$d_gap_rel, dd) %>%
    mutate(tau = tau, n_EC = sum(dd$tapio == "EC"), .before = 1)
})
print(tau_rob)
write_csv(tau_rob, file.path(TABS, "regression_tau_robustness.csv"))

# --- §4.1 descriptivos por país (panel 1994–2024; SD+WD sobre 1995–2024) ---------
pm <- read_csv(file.path(PROC, "panel_main.csv"), show_col_types = FALSE)
desc <- pm %>% group_by(bloc, iso3, country) %>%
  summarise(n = n(), gap_mean = mean(gap_rel), gap_sd = sd(gap_rel),
            gap_min = min(gap_rel), gap_median = median(gap_rel), gap_max = max(gap_rel),
            .groups = "drop") %>%
  left_join(pc %>% group_by(iso3) %>%
              summarise(sdwd_pct = 100 * mean(tapio %in% c("SD", "WD"))), by = "iso3") %>%
  arrange(bloc, country)
print(desc, n = 40)
write_csv(desc, file.path(TABS, "gap_descriptives_country.csv"))

# --- §4.3 mecanismo: crecimientos en años SD vs. no-SD ------------------------------
mech <- pc %>% group_by(iso3) %>% arrange(year) %>%
  mutate(g_dmc = dmc_mt / lag(dmc_mt) - 1, g_de = DE / lag(DE) - 1,
         g_rme_exp = RME_EXP / lag(RME_EXP) - 1, g_exp = EXP / lag(EXP) - 1) %>%
  ungroup() %>% filter(!is.na(g_dmc), year <= 2021) %>%
  mutate(state = if_else(tapio == "SD", "SD years", "Other years")) %>%
  group_by(bloc, state) %>%
  summarise(n = n(), across(c(g_mf, g_dmc, g_de, g_rme_exp, g_exp, g_gdp),
                            ~ 100 * median(.x, na.rm = TRUE)), .groups = "drop")
print(mech)
write_csv(mech, file.path(TABS, "mechanism_sd_years_growth.csv"))

cat("11_revision_r3.R completado.\n")
