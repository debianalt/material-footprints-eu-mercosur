# =============================================================================
# 07_decomposition_global.R   (revisión JCP, 2026-09)
#
# (1) Descomposición exacta del gap (respuesta a R1 / R2#1-2 / R3#1):
#       MF  = DE + RME_IMP − RME_EXP
#       DMC = DE + IMP     − EXP
#       MF − DMC = (RME_IMP − IMP) − (RME_EXP − EXP)          (flujos indirectos netos)
#       gap_rel  = RTB/MF − PTB/MF,   RTB = RME_IMP − RME_EXP,  PTB = IMP − EXP
#     gap_rel NO es "la fracción del MF cubierta por extracción importada neta"
#     (eso es RTB/MF = 1 − DE/MF, el indicador compañero). gap_rel es la parte del
#     footprint que DMC no registra: la extracción aguas arriba del comercio neto.
# (2) Evidencia física en RME: exportaciones RME (todos los destinos) de MERCOSUR.
# (3) Extensión global descriptiva + diagnóstico M2 global (R2#8).
#
# NO re-ejecuta 01_data_prep.R (re-descargaría el PIB del Banco Mundial);
# lee panel_regimes.csv tal cual y le agrega los componentes del GMFD.
# =============================================================================

suppressMessages({library(tidyverse); library(countrycode); library(fixest)})

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")
RAW  <- file.path(BASE, "data/raw")

mfa_raw <- read_csv(file.path(RAW, "mfa_filled.csv"), show_col_types = FALSE)
year_cols <- names(mfa_raw)[str_detect(names(mfa_raw), "^\\d{4}$")]

comp <- mfa_raw %>%
  filter(`Flow code` %in% c("DE", "IMP", "EXP", "RME_IMP", "RME_EXP", "MF", "DMC", "GDP")) %>%
  select(Country, flow = `Flow code`, all_of(year_cols)) %>%
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "v") %>%
  mutate(year = as.integer(year),
         iso3 = countrycode(Country, "country.name", "iso3c", warn = FALSE)) %>%
  filter(!is.na(iso3), year >= 1993, year <= 2024) %>%
  pivot_wider(id_cols = c(iso3, Country, year), names_from = flow,
              values_from = v, values_fn = mean) %>%
  mutate(across(c(DE, IMP, EXP, RME_IMP, RME_EXP, MF, DMC), ~ .x / 1e6))  # Mt

# -----------------------------------------------------------------------------
# 1. Descomposición sobre el panel del paper
# -----------------------------------------------------------------------------
panel <- read_csv(file.path(PROC, "panel_regimes.csv"), show_col_types = FALSE)
pc <- panel %>%
  left_join(comp %>% select(iso3, year, DE, IMP, EXP, RME_IMP, RME_EXP),
            by = c("iso3", "year")) %>%
  mutate(rtb_sh = (RME_IMP - RME_EXP) / mf_mt,
         ptb_sh = (IMP - EXP) / mf_mt,
         ind_imp_sh = (RME_IMP - IMP) / mf_mt,   # indirectos de importaciones
         ind_exp_sh = (RME_EXP - EXP) / mf_mt,   # indirectos de exportaciones
         id_resid = gap_rel - (rtb_sh - ptb_sh),
         mf_id_resid  = (DE + RME_IMP - RME_EXP - mf_mt) / mf_mt,
         dmc_id_resid = (DE + IMP - EXP - dmc_mt) / dmc_mt)

stopifnot(all(!is.na(pc$rtb_sh)))
cat(sprintf("Identidad gap = rtb_sh - ptb_sh: max |resid| = %.2e (n = %d)\n",
            max(abs(pc$id_resid)), nrow(pc)))
cat(sprintf("Identidad MF: max rel resid = %.2e ; DMC: %.2e\n",
            max(abs(pc$mf_id_resid)), max(abs(pc$dmc_id_resid))))

sign_agree <- mean(sign(pc$gap_rel) == sign(pc$rtb_sh))
cat(sprintf("Concordancia de signo gap_rel vs RTB/MF: %.1f%%\n", 100 * sign_agree))

# Tablas por país/bloque sobre panel_main (1994–2024), la misma ventana de las
# medianas del §3.1; la regresión usa panel_components (1995–2024, con estados Tapio).
pm <- read_csv(file.path(PROC, "panel_main.csv"), show_col_types = FALSE) %>%
  left_join(comp %>% select(iso3, year, DE, IMP, EXP, RME_IMP, RME_EXP), by = c("iso3", "year")) %>%
  mutate(rtb_sh = (RME_IMP - RME_EXP) / mf_mt, ptb_sh = (IMP - EXP) / mf_mt,
         ind_imp_sh = (RME_IMP - IMP) / mf_mt, ind_exp_sh = (RME_EXP - EXP) / mf_mt)
sign_agree <- mean(sign(pm$gap_rel) == sign(pm$rtb_sh))
cat(sprintf("Concordancia de signo (panel_main 1994–2024): %.1f%%\n", 100 * sign_agree))

country_decomp <- pm %>%
  group_by(bloc, iso3, country) %>%
  summarise(gap_med = median(gap_rel), rtb_med = median(rtb_sh),
            ptb_med = median(ptb_sh), ind_imp_med = median(ind_imp_sh),
            ind_exp_med = median(ind_exp_sh),
            sign_agree_pct = 100 * mean(sign(gap_rel) == sign(rtb_sh)),
            .groups = "drop") %>%
  arrange(bloc, gap_med)
print(country_decomp, n = 40)
write_csv(country_decomp, file.path(TABS, "gap_decomposition_country.csv"))

bloc_decomp <- pm %>%
  group_by(bloc) %>%
  summarise(n = n(), gap_med = median(gap_rel), rtb_med = median(rtb_sh),
            ptb_med = median(ptb_sh),
            sign_agree_pct = 100 * mean(sign(gap_rel) == sign(rtb_sh)),
            cor_gap_rtb = cor(gap_rel, rtb_sh), .groups = "drop") %>%
  bind_rows(tibble(bloc = "ALL", n = nrow(pm), gap_med = median(pm$gap_rel),
                   rtb_med = median(pm$rtb_sh), ptb_med = median(pm$ptb_sh),
                   sign_agree_pct = 100 * sign_agree,
                   cor_gap_rtb = cor(pm$gap_rel, pm$rtb_sh)))
print(bloc_decomp)
write_csv(bloc_decomp, file.path(TABS, "gap_decomposition_bloc.csv"))

write_csv(pc %>% select(-id_resid, -mf_id_resid, -dmc_id_resid),
          file.path(PROC, "panel_components.csv"))

# -----------------------------------------------------------------------------
# 2. Exportaciones físicas en RME de MERCOSUR (todos los destinos)
# -----------------------------------------------------------------------------
mcs_rme <- comp %>%
  filter(iso3 %in% c("ARG", "BRA", "PRY", "URY"), year >= 1994) %>%
  select(iso3, year, RME_EXP, EXP, DE) %>%
  bind_rows(comp %>% filter(iso3 %in% c("ARG", "BRA", "PRY", "URY"), year >= 1994) %>%
              group_by(year) %>%
              summarise(across(c(RME_EXP, EXP, DE), sum), .groups = "drop") %>%
              mutate(iso3 = "MERCOSUR")) %>%
  mutate(rme_exp_share_de = RME_EXP / DE)
write_csv(mcs_rme, file.path(TABS, "mercosur_rme_exports.csv"))
cat("\nMERCOSUR RME exports (Mt), años clave:\n")
print(mcs_rme %>% filter(year %in% c(1994, 2000, 2010, 2023, 2024)) %>% arrange(iso3, year), n = 30)

# -----------------------------------------------------------------------------
# 3. Global: distribución descriptiva + diagnóstico M2
# -----------------------------------------------------------------------------
inc <- WDI::WDI_data$country %>% as_tibble() %>%
  transmute(iso3 = iso3c, income = income, region = region) %>%
  filter(!is.na(income), income != "Aggregates")

glob <- comp %>%
  filter(!is.na(MF), !is.na(DMC), !is.na(GDP), MF > 0, DMC > 0, GDP > 0) %>%
  inner_join(inc, by = "iso3") %>%
  mutate(gap_rel = (MF - DMC) / MF, rtb_sh = (RME_IMP - RME_EXP) / MF) %>%
  arrange(iso3, year)

# 2021 = último año con identidades exactas en el GMFD (2022–2024 son estimaciones)
glob_2024 <- glob %>% filter(year == 2021)
cat(sprintf("\nGlobal 2021: %d países con gap_rel\n", nrow(glob_2024)))
glob_summary <- glob_2024 %>%
  mutate(income = factor(income, levels = c("Low income", "Lower middle income",
                                            "Upper middle income", "High income"))) %>%
  filter(!is.na(income)) %>%
  group_by(income) %>%
  summarise(n = n(), gap_q25 = quantile(gap_rel, .25), gap_med = median(gap_rel),
            gap_q75 = quantile(gap_rel, .75), share_pos = mean(gap_rel > 0),
            rtb_med = median(rtb_sh),
            sign_agree_pct = 100 * mean(sign(gap_rel) == sign(rtb_sh)), .groups = "drop")
print(glob_summary)
write_csv(glob_summary, file.path(TABS, "global_gap_by_income_2021.csv"))
write_csv(glob %>% select(iso3, Country, year, income, region, gap_rel, rtb_sh),
          file.path(PROC, "global_gap_panel.csv"))

# Diagnóstico M2 global (1995–2024): Tapio sobre MF y PIB del GMFD;
# grupo = signo de la mediana del gap del país (positivo vs negativo).
source_tapio <- function(g_mf, g_gdp, tau = 0.2) {
  out <- rep(NA_character_, length(g_mf))
  e <- g_mf / g_gdp
  up <- g_gdp > 0; dn <- g_gdp < 0
  out[up & g_mf < 0] <- "SD"
  out[up & g_mf >= 0 & e < 1 - tau] <- "WD"
  out[up & g_mf >= 0 & abs(e - 1) <= tau] <- "EC"
  out[up & g_mf >= 0 & e > 1 + tau] <- "END"
  out[dn & g_mf > 0] <- "SND"
  out[dn & g_mf <= 0 & e < 1 - tau] <- "WND"
  out[dn & g_mf <= 0 & abs(e - 1) <= tau] <- "RC"
  out[dn & g_mf <= 0 & e > 1 + tau] <- "RD"
  out
}
greg <- glob %>%
  filter(year >= 1994) %>%
  group_by(iso3) %>% arrange(year) %>%
  mutate(g_mf = MF / lag(MF) - 1, g_gdp = GDP / lag(GDP) - 1,
         d_gap = (gap_rel - lag(gap_rel)) * 100,
         tapio = source_tapio(g_mf, g_gdp),
         neg_gap = as.integer(median(gap_rel) < 0)) %>%
  ungroup() %>%
  filter(!is.na(d_gap), !is.na(tapio), is.finite(d_gap)) %>%
  mutate(tapio_f = factor(tapio, levels = c("SD","WD","EC","END","WND","RC","RD","SND")),
         pos_gap = 1L - neg_gap)
# winsorizar Δgap al 1–99% (economías diminutas con gap volátil)
q <- quantile(greg$d_gap, c(.01, .99))
greg <- greg %>% mutate(d_gap_w = pmin(pmax(d_gap, q[1]), q[2]))

mg <- feols(d_gap_w ~ i(tapio_f, ref = "EC") + i(tapio_f, pos_gap, ref = "EC") | iso3 + year,
            cluster = ~iso3, data = greg)
print(summary(mg))
tg <- broom::tidy(mg, conf.int = TRUE)
sd_neg <- tg %>% filter(term == "tapio_f::SD")
sd_off <- tg %>% filter(term == "tapio_f::SD:pos_gap")
glob_diag <- tibble(
  sample = "Global GMFD, 1995–2024, Δgap winsorised 1–99%",
  n_obs = nobs(mg), n_countries = n_distinct(greg$iso3),
  n_neg_gap_countries = n_distinct(greg$iso3[greg$neg_gap == 1]),
  sd_neg_gap = sd_neg$estimate, sd_neg_gap_lo = sd_neg$conf.low, sd_neg_gap_hi = sd_neg$conf.high,
  sd_pos_offset = sd_off$estimate, sd_pos_offset_p = sd_off$p.value,
  sd_pos_net = sd_neg$estimate + sd_off$estimate)

# Variante scale-free: Δlog(MF/DMC) elimina el multiplicador DMC/MF
# (Δgap ≈ (DMC/MF)·(g_MF − g_DMC)), mayor en países con gap negativo.
greg <- greg %>% group_by(iso3) %>% arrange(year) %>%
  mutate(d_lratio = (log(MF / lag(MF)) - log(DMC / lag(DMC))) * 100) %>% ungroup()
ql <- quantile(greg$d_lratio, c(.01, .99), na.rm = TRUE)
greg <- greg %>% mutate(d_lratio_w = pmin(pmax(d_lratio, ql[1]), ql[2]))
mgl <- feols(d_lratio_w ~ i(tapio_f, ref = "EC") + i(tapio_f, pos_gap, ref = "EC") | iso3 + year,
             cluster = ~iso3, data = greg)
tl <- broom::tidy(mgl, conf.int = TRUE)
sl <- tl %>% filter(term == "tapio_f::SD"); ol <- tl %>% filter(term == "tapio_f::SD:pos_gap")
glob_diag <- bind_rows(
  glob_diag %>% mutate(dv = "Δgap_rel (pp)", .before = 1),
  tibble(dv = "Δlog(MF/DMC) (pp, scale-free)",
         sample = glob_diag$sample, n_obs = nobs(mgl), n_countries = glob_diag$n_countries,
         n_neg_gap_countries = glob_diag$n_neg_gap_countries,
         sd_neg_gap = sl$estimate, sd_neg_gap_lo = sl$conf.low, sd_neg_gap_hi = sl$conf.high,
         sd_pos_offset = ol$estimate, sd_pos_offset_p = ol$p.value,
         sd_pos_net = sl$estimate + ol$estimate))
print(glob_diag, width = 200)
write_csv(glob_diag, file.path(TABS, "global_m2_diagnostic.csv"))

cat("\n07_decomposition_global.R completado.\n")
