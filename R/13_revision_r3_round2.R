# =============================================================================
# 13_revision_r3_round2.R   (revisión JCP, ronda 2, 2026-11) — Reviewer 3, punto 3
#   Cota cuantitativa del componente mecánico de la asociación SD -> Δgap_rel:
#   (1) multiplicador DMC/MF: medianas por bloque y país (1994–2024)
#   (2) contrafáctico mecánico: Δgap si el DMC hubiera quedado en su nivel de t−1
#       y sólo el MF se moviera como se observó,
#         Δgap_mech = 100 · (DMC_{t−1}/MF_{t−1} − DMC_{t−1}/MF_t)
#       y el resto (la respuesta del DMC), Δgap − Δgap_mech; M2 sobre ambos.
#       Por linealidad, los coeficientes de ambos suman los de la línea de base.
#   (3) la misma descomposición sin Uruguay 2014 (MF −54%).
# Salida: output/tables/mechanical_bound.csv, output/tables/dmc_mf_multiplier.csv
# =============================================================================

suppressMessages({library(tidyverse); library(clubSandwich)})

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")
NONREF <- setdiff(TAPIO_LEVELS, "EC")
source(file.path(BASE, "R", "_inference_helpers.R"))

pc <- read_csv(file.path(PROC, "panel_components.csv"), show_col_types = FALSE)

# --- (1) multiplicador DMC/MF ---------------------------------------------------
pm <- read_csv(file.path(PROC, "panel_main.csv"), show_col_types = FALSE) %>%
  mutate(r = dmc_mt / mf_mt)
mult <- bind_rows(
  pm %>% group_by(level = "bloc", unit = bloc) %>%
    summarise(n = n(), median = median(r), q25 = quantile(r, .25), q75 = quantile(r, .75),
              .groups = "drop"),
  pm %>% filter(bloc == "MERCOSUR") %>% group_by(level = "country", unit = iso3) %>%
    summarise(n = n(), median = median(r), q25 = quantile(r, .25), q75 = quantile(r, .75),
              .groups = "drop"))
print(mult)
write_csv(mult, file.path(TABS, "dmc_mf_multiplier.csv"))

# --- (2) contrafáctico mecánico -------------------------------------------------
d <- pc %>% group_by(iso3) %>% arrange(year) %>%
  mutate(d_gap_rel = (gap_rel - lag(gap_rel)) * 100,
         d_mech    = (lag(dmc_mt) / lag(mf_mt) - lag(dmc_mt) / mf_mt) * 100,
         d_rest    = d_gap_rel - d_mech) %>% ungroup() %>%
  filter(!is.na(d_gap_rel), !is.na(tapio)) %>%
  mutate(eu = as.integer(bloc == "EU"))
stopifnot(nrow(d) == 899)

run <- function(dd, sample) {
  map_dfr(c(baseline = "d_gap_rel", mechanical = "d_mech", dmc_response = "d_rest"),
          ~ fit_cr2(dd[[.x]], dd), .id = "component") %>%
    mutate(sample = sample, .before = 1)
}
mb <- bind_rows(run(d, "all"),
                run(d %>% filter(!(iso3 == "URY" & year == 2014)), "excl. URY 2014"))

# chequeos: línea de base de la Tabla 2 y aditividad
b <- mb %>% filter(sample == "all", component == "baseline", term == "SD")
stopifnot(abs(b$est - (-11.5165)) < 1e-3)
add <- mb %>% select(sample, component, term, est) %>%
  pivot_wider(names_from = component, values_from = est)
stopifnot(all(abs(add$baseline - add$mechanical - add$dmc_response) < 1e-8))

# share of the mechanical counterfactual offset by DMC movements
offset <- add %>% filter(term %in% c("SD", "net_EU_SD")) %>%
  mutate(offset_share = dmc_response / -mechanical)
print(mb, n = Inf, width = Inf)
print(offset)
write_csv(mb, file.path(TABS, "mechanical_bound.csv"))

# --- escenario «DMC sin cambio» en un año SD típico (medianas) -------------------
#   Δgap ≈ −r_{t−1} · x / (1 − x), con x la caída del MF y r = DMC/MF rezagado
med <- d %>% group_by(iso3) %>% arrange(year) %>% ungroup() %>%
  left_join(pc %>% group_by(iso3) %>% arrange(year) %>%
              mutate(r_lag = lag(dmc_mt) / lag(mf_mt)) %>% ungroup() %>%
              select(iso3, year, r_lag), by = c("iso3", "year")) %>%
  filter(tapio %in% c("SD", "EC")) %>%
  group_by(bloc, tapio) %>%
  summarise(n = n(), g_mf_pct = 100 * median(g_mf), r_lag = median(r_lag),
            d_gap_obs = median(d_gap_rel), d_gap_mech = median(d_mech), .groups = "drop") %>%
  mutate(scenario_dmc_unchanged = -r_lag * (-g_mf_pct / 100) / (1 + g_mf_pct / 100) * 100)
print(med)
write_csv(med, file.path(TABS, "mechanical_scenario_medians.csv"))
