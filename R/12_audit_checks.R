# =============================================================================
# 12_audit_checks.R   (revisión JCP, 2026-09) — chequeos de la auditoría adversarial
#   (1) Clasificación Tapio sobre DMC en vez de MF (¿asimetría mecánica?)
#   (2) Sensibilidad a la categoría de referencia y a outliers
#   (3) Tests de bloque con aleatorización de etiquetas de PAÍS (no country-years):
#       diferencia de % SD, distribución Tapio (χ²), cuota del régimen de contracción K=2
#   (4) Bootstrap de cuotas K=2 estratificado por bloque (B = 2000), condicional al modelo
# =============================================================================

suppressMessages({library(tidyverse); library(clubSandwich)})
set.seed(20260923)

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")
TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")
NONREF <- setdiff(TAPIO_LEVELS, "EC")
source(file.path(BASE, "R", "_inference_helpers.R"))

tapio_tau <- function(g_x, g_gdp, tau = 0.2) {
  e <- g_x / g_gdp
  case_when(is.na(g_x) | is.na(g_gdp) ~ NA_character_,
            g_gdp == 0 & g_x > 0 ~ "END", g_gdp == 0 ~ "WND",
            g_gdp > 0 & g_x < 0 ~ "SD", g_gdp > 0 & e < 1 - tau ~ "WD",
            g_gdp > 0 & abs(e - 1) <= tau ~ "EC", g_gdp > 0 ~ "END",
            g_x > 0 ~ "SND", e < 1 - tau ~ "WND", abs(e - 1) <= tau ~ "RC", TRUE ~ "RD")
}

pc <- read_csv(file.path(PROC, "panel_components.csv"), show_col_types = FALSE)
d <- pc %>% group_by(iso3) %>% arrange(year) %>%
  mutate(d_gap_rel = (gap_rel - lag(gap_rel)) * 100,
         g_dmc = dmc_mt / lag(dmc_mt) - 1) %>% ungroup() %>%
  filter(!is.na(d_gap_rel), !is.na(tapio)) %>%
  mutate(eu = as.integer(bloc == "EU"))
stopifnot(nrow(d) == 899)

# --- (1) Tapio sobre DMC --------------------------------------------------------
dd <- d %>% mutate(tapio = tapio_tau(g_dmc, g_gdp))
dmc_class <- fit_cr2(dd$d_gap_rel, dd) %>% mutate(check = "Tapio classified on DMC", .before = 1)
overlap <- d %>% filter(bloc == "MERCOSUR") %>%
  summarise(mf_sd = sum(tapio == "SD"), both_sd = sum(tapio == "SD" & dd$tapio[d$bloc == "MERCOSUR"] == "SD"))
print(dmc_class); print(overlap)

# --- (2) referencia y outliers ----------------------------------------------------
# (a) SD vs. todos los demás estados (dummy única × bloque)
Xsd <- function(df) {
  sd <- as.numeric(df$tapio == "SD")
  cbind(`(Intercept)` = 1, SD = sd, `SD:EU` = sd * df$eu,
        model.matrix(~ factor(df$iso3))[, -1], model.matrix(~ factor(df$year))[, -1])
}
fit_simple <- function(y, X, cl) {
  dfX <- as.data.frame(X[, -1]); names(dfX) <- make.names(names(dfX), unique = TRUE)
  mm <- lm(y ~ ., data = cbind(y = y, dfX))
  V <- vcovCR(mm, cluster = cl, type = "CR2")
  ct <- coef_test(mm, vcov = V, coefs = c("SD", "SD.EU"), test = "Satterthwaite")
  L <- matrix(0, 1, length(coef(mm))); colnames(L) <- names(coef(mm)); L[1, c("SD", "SD.EU")] <- 1
  ln <- linear_contrast(mm, vcov = V, contrasts = L, test = "Satterthwaite")
  tibble(term = c("SD", "SD:EU", "net_EU_SD"), est = c(ct$beta, ln$Est),
         se = c(ct$SE, ln$SE), df_satt = c(ct$df_Satt, ln$df),
         p_cr2 = c(ct$p_Satt, 2 * pt(-abs(ln$Est / ln$SE), ln$df)))
}
ref_all <- fit_simple(d$d_gap_rel, Xsd(d), d$iso3) %>% mutate(check = "Reference: all non-SD states", .before = 1)
d_noury14 <- d %>% filter(!(iso3 == "URY" & year == 2014))
drop_out <- fit_cr2(d_noury14$d_gap_rel, d_noury14) %>% mutate(check = "Drop Uruguay 2014 (MF −54%)", .before = 1)
q <- quantile(d$d_gap_rel, c(.01, .99))
wins <- fit_cr2(pmin(pmax(d$d_gap_rel, q[1]), q[2]), d) %>% mutate(check = "Δgap winsorised 1–99%", .before = 1)
ref_checks <- bind_rows(dmc_class, ref_all, drop_out, wins) %>% filter(term %in% c("SD", "SD:EU", "net_EU_SD"))
print(ref_checks)
write_csv(ref_checks, file.path(TABS, "regression_audit_checks.csv"))

# --- (3) aleatorización de etiquetas de país (C(31,4) = 31.465) --------------------
countries <- sort(unique(d$iso3)); combs <- combn(countries, 4)
obs_m <- c("ARG", "BRA", "PRY", "URY")
tp <- read_csv(file.path(PROC, "panel_tapio.csv"), show_col_types = FALSE) %>% filter(!is.na(tapio))
k2 <- read_csv(file.path(PROC, "panel_regimes_k2.csv"), show_col_types = FALSE)
tab_by <- function(set) {                        # estadísticos a nivel de bloque
  g <- tp$iso3 %in% set
  sd_diff <- mean(tp$tapio[!g] == "SD") - mean(tp$tapio[g] == "SD")
  ct <- table(g, factor(tp$tapio, levels = TAPIO_LEVELS))
  chi <- suppressWarnings(chisq.test(ct)$statistic)
  kg <- k2$iso3 %in% set
  k2_diff <- mean(k2$regime_viterbi[kg] == 1) - mean(k2$regime_viterbi[!kg] == 1)
  c(sd_diff, chi, k2_diff)
}
obs <- tab_by(obs_m)
perm <- t(apply(combs, 2, tab_by))
perm_res <- tibble(
  statistic = c("EU − MERCOSUR share of SD years", "χ² Tapio distribution (7 df)",
                "MERCOSUR − EU share in K=2 contraction regime"),
  observed = obs,
  p_perm = c(mean(abs(perm[, 1]) >= abs(obs[1])), mean(perm[, 2] >= obs[2]),
             mean(abs(perm[, 3]) >= abs(obs[3]))),
  n_assign = ncol(combs))
print(perm_res)
write_csv(perm_res, file.path(TABS, "bloc_permutation_tests.csv"))

# --- (4) bootstrap K=2 estratificado --------------------------------------------------
boot_share <- function(bl, B = 2000) {
  g <- k2 %>% filter(bloc == bl); cs <- unique(g$iso3)
  by_c <- split(g$regime_viterbi == 1, g$iso3)
  sh <- replicate(B, { s <- sample(cs, length(cs), replace = TRUE); mean(unlist(by_c[s])) })
  tibble(bloc = bl, share = mean(g$regime_viterbi == 1), lo = quantile(sh, .025), hi = quantile(sh, .975), B = B)
}
k2_boot <- bind_rows(boot_share("EU"), boot_share("MERCOSUR"))
print(k2_boot)
write_csv(k2_boot, file.path(TABS, "hmm_k2_bootstrap.csv"))
cat("12_audit_checks.R completado.\n")
