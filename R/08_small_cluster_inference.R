# =============================================================================
# 08_small_cluster_inference.R   (revisión JCP, 2026-09)
#
# Respuesta a R2 ("four MERCOSUR clusters… small-cluster inference") y R3#2
# ("label as descriptive, or country-level bootstrap"), y a R2#6 (magnitudes e
# intervalos entre especificaciones, no sólo signo).
#
# Parámetros de interés (modelo M2, referencia EC, MERCOSUR = grupo base):
#   b_SD      = coeficiente SD del grupo MERCOSUR
#   off_SD    = SD × EU (diferencia UE − MERCOSUR)
#   net_EU_SD = b_SD + off_SD
#
# Métodos:
#   (a) CR2 (Bell–McCaffrey) + gl de Satterthwaite (clubSandwich)
#   (b) Wild cluster bootstrap-t restringido (WCR), pesos Webb de 6 puntos,
#       B = 9999 (MacKinnon & Webb 2017/2018); implementado a mano.
#   (c) Inferencia por aleatorización exacta: la etiqueta "MERCOSUR" se asigna a
#       los C(31,4) = 31.465 subconjuntos de 4 países.
#   (d) Tabla de magnitudes: estimado + IC 95% CR2 en todas las especificaciones.
# =============================================================================

suppressMessages({library(tidyverse); library(clubSandwich)})
set.seed(20260922)

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")
NONREF <- setdiff(TAPIO_LEVELS, "EC")

pc <- read_csv(file.path(PROC, "panel_components.csv"), show_col_types = FALSE)

d <- pc %>%
  group_by(iso3) %>% arrange(year) %>%
  mutate(
    d_gap_rel = (gap_rel - lag(gap_rel)) * 100,
    d_gap_abs = gap_abs_mt - lag(gap_abs_mt),
    gap_dmc   = (mf_mt - dmc_mt) / dmc_mt,
    d_gap_dmc = (gap_dmc - lag(gap_dmc)) * 100,
    # scale-free: diferencia de tasas de crecimiento logarítmicas MF − DMC (pp).
    # Elimina el multiplicador DMC/MF que hace que el mismo shock mueva más el gap
    # en países con DMC > MF (Δgap ≈ (DMC/MF)·(g_MF − g_DMC)).
    d_lratio  = (log(mf_mt / lag(mf_mt)) - log(dmc_mt / lag(dmc_mt))) * 100,
    d_rtb_sh  = (rtb_sh - lag(rtb_sh)) * 100,
    d_ptb_sh  = (ptb_sh - lag(ptb_sh)) * 100
  ) %>% ungroup() %>%
  filter(!is.na(d_gap_rel), !is.na(tapio)) %>%
  mutate(tapio_f = factor(tapio, levels = TAPIO_LEVELS),
         eu = as.integer(bloc == "EU"))
stopifnot(nrow(d) == 899)

# --- matriz de diseño M2 (dummies explícitas; FE como dummies) -------------
design_m2 <- function(df, grp) {           # grp = 1 para el grupo "EU"/offset
  S  <- sapply(NONREF, function(s) as.numeric(df$tapio == s))
  colnames(S) <- NONREF
  SI <- S * grp; colnames(SI) <- paste0(NONREF, ":EU")
  FEc <- model.matrix(~ factor(df$iso3))[, -1, drop = FALSE]
  FEy <- model.matrix(~ factor(df$year))[, -1, drop = FALSE]
  X <- cbind(`(Intercept)` = 1, S, SI, FEc, FEy)
  X[, colSums(abs(X)) > 0, drop = FALSE]
}

fit_cr2 <- function(y, df, grp = df$eu, terms = c("SD", "SD:EU")) {
  X  <- design_m2(df, grp)
  ok <- !is.na(y)
  X <- X[ok, ]; y <- y[ok]; cl <- df$iso3[ok]
  keep <- !is.na(lm.fit(X, y)$coefficients)   # descarta columnas colineales
  X <- X[, keep]
  # clubSandwich necesita un objeto lm: lo reconstruimos sobre la matriz explícita
  dfX <- as.data.frame(X[, -1]); names(dfX) <- make.names(names(dfX), unique = TRUE)
  mm  <- lm(y ~ ., data = cbind(y = y, dfX))
  V   <- vcovCR(mm, cluster = cl, type = "CR2")
  cn  <- make.names(terms)
  ct  <- coef_test(mm, vcov = V, coefs = cn, test = "Satterthwaite")
  # coef_test devuelve en el orden del modelo, no en el de `terms`: mapear por nombre
  out <- tibble(term = terms[match(ct$Coef, cn)], est = ct$beta, se = ct$SE,
                df_satt = ct$df_Satt, p_cr2 = ct$p_Satt)
  # net EU = SD + SD:EU (contraste lineal)
  if (all(c("SD", "SD.EU") %in% names(coef(mm)))) {
    L <- matrix(0, 1, length(coef(mm))); colnames(L) <- names(coef(mm))
    L[1, c("SD", "SD.EU")] <- 1
    ln <- linear_contrast(mm, vcov = V, contrasts = L, test = "Satterthwaite")
    out <- bind_rows(out, tibble(term = "net_EU_SD", est = ln$Est, se = ln$SE,
                                 df_satt = ln$df,
                                 p_cr2 = 2 * pt(-abs(ln$Est / ln$SE), ln$df)))
  }
  out %>% mutate(lo = est - qt(.975, df_satt) * se, hi = est + qt(.975, df_satt) * se,
                 n = length(y), G = n_distinct(cl))
}

# -----------------------------------------------------------------------------
# (a) CR2 sobre el M2 principal
# -----------------------------------------------------------------------------
main_cr2 <- fit_cr2(d$d_gap_rel, d, terms = c("SD", "SD:EU", "RD", "RD:EU"))
cat("\n=== M2 principal, CR2 + Satterthwaite ===\n"); print(main_cr2)

# Todos los términos de M2 (para Tabla 2 y Fig. 5 con IC CR2)
m2_all <- fit_cr2(d$d_gap_rel, d, terms = c(NONREF, paste0(NONREF, ":EU")))
# netos UE por estado (base + offset) con CR2
dfX_all <- {
  X <- design_m2(d, d$eu); dfX <- as.data.frame(X[, -1])
  names(dfX) <- make.names(names(dfX), unique = TRUE); dfX }
mm_all <- lm(y ~ ., data = cbind(y = d$d_gap_rel, dfX_all))
V_all  <- vcovCR(mm_all, cluster = d$iso3, type = "CR2")
net_all <- map_dfr(NONREF, function(s) {
  L <- matrix(0, 1, length(coef(mm_all))); colnames(L) <- names(coef(mm_all))
  L[1, c(s, paste0(s, ".EU"))] <- 1
  ln <- linear_contrast(mm_all, vcov = V_all, contrasts = L, test = "Satterthwaite")
  tibble(term = paste0(s, ":net_EU"), est = ln$Est, se = ln$SE, df_satt = ln$df,
         p_cr2 = 2 * pt(-abs(ln$Est / ln$SE), ln$df),
         lo = ln$CI_L, hi = ln$CI_U, n = nrow(d), G = 31L)
})
# M1 y M3 con CR2 (para la Tabla 2)
X1 <- cbind(sapply(NONREF, function(s) as.numeric(d$tapio == s)))
fe <- model.matrix(~ factor(d$iso3) + factor(d$year))[, -1]
m1_lm <- lm(d$d_gap_rel ~ X1 + fe)
names(m1_lm$coefficients) <- make.names(names(m1_lm$coefficients))
ct1 <- coef_test(m1_lm, vcov = vcovCR(m1_lm, cluster = d$iso3, type = "CR2"),
                 coefs = paste0("X1", NONREF), test = "Satterthwaite")
eff <- as.numeric(d$tapio %in% c("SD", "WD", "EC"))
m3_lm <- lm(d$d_gap_rel ~ eff + I(eff * d$eu) + fe)
ct3 <- coef_test(m3_lm, vcov = vcovCR(m3_lm, cluster = d$iso3, type = "CR2"),
                 coefs = names(coef(m3_lm))[2:3], test = "Satterthwaite")
tab2 <- bind_rows(
  tibble(model = "M1", term = NONREF[match(sub("X1", "", ct1$Coef), NONREF)],
         est = ct1$beta, se = ct1$SE, df_satt = ct1$df_Satt, p_cr2 = ct1$p_Satt),
  m2_all %>% mutate(model = "M2") %>% select(model, term, est, se, df_satt, p_cr2),
  net_all %>% mutate(model = "M2") %>% select(model, term, est, se, df_satt, p_cr2),
  tibble(model = "M3", term = c("eff", "eff:EU"), est = ct3$beta, se = ct3$SE,
         df_satt = ct3$df_Satt, p_cr2 = ct3$p_Satt))
write_csv(tab2, file.path(TABS, "regression_table2_cr2.csv"))
write_csv(bind_rows(m2_all, net_all), file.path(TABS, "regression_m2_cr2_all.csv"))
print(tab2, n = 50)

# -----------------------------------------------------------------------------
# (b) Wild cluster restricted bootstrap-t, Webb weights
# -----------------------------------------------------------------------------
webb <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))

wcr_boot <- function(y, df, grp, term, B = 9999) {
  X  <- design_m2(df, grp)
  cl <- df$iso3; g <- as.integer(factor(cl)); G <- max(g)
  j  <- which(colnames(X) == term)
  XtXi <- solve(crossprod(X))
  v    <- X %*% XtXi[, j]                    # b_j = sum v_i y_i
  cr1  <- function(E) {                      # E: n x R residuos -> se_j
    S <- rowsum(as.vector(v) * E, g)         # G x R
    sqrt(colSums(S^2) * G / (G - 1) * (nrow(X) - 1) / (nrow(X) - ncol(X)))
  }
  b_hat <- sum(v * y); e_hat <- y - X %*% (XtXi %*% crossprod(X, y))
  t_hat <- b_hat / cr1(e_hat)
  Xr <- X[, -j]; br <- qr.coef(qr(Xr), y); br[is.na(br)] <- 0
  yr <- as.vector(Xr %*% br); ur <- y - yr
  W  <- matrix(sample(webb, G * B, replace = TRUE), G, B)[g, ]
  Ys <- yr + ur * W
  bs <- colSums(as.vector(v) * Ys)
  Es <- Ys - X %*% (XtXi %*% crossprod(X, Ys))
  ts <- bs / cr1(Es)
  tibble(term = term, t = t_hat, p_wcr = mean(abs(ts) >= abs(t_hat)), B = B)
}
wcr <- bind_rows(
  wcr_boot(d$d_gap_rel, d, d$eu, "SD"),
  wcr_boot(d$d_gap_rel, d, d$eu, "SD:EU"),
  wcr_boot(d$d_gap_rel, d, d$eu, "RD"),
  wcr_boot(d$d_gap_rel, d, d$eu, "RD:EU"))
cat("\n=== Wild cluster restricted bootstrap (Webb, B = 9999) ===\n"); print(wcr)

# -----------------------------------------------------------------------------
# (c) Aleatorización exacta: todas las asignaciones de 4 países como "MERCOSUR"
# -----------------------------------------------------------------------------
countries <- sort(unique(d$iso3))
combs <- combn(countries, 4)
Xbase_fe <- cbind(1, sapply(NONREF, function(s) as.numeric(d$tapio == s)),
                  model.matrix(~ factor(d$iso3))[, -1], model.matrix(~ factor(d$year))[, -1])
Sm <- sapply(NONREF, function(s) as.numeric(d$tapio == s))
y  <- d$d_gap_rel
ri_stat <- function(set) {
  grp <- as.numeric(!(d$iso3 %in% set))            # 1 = "EU" (offset), 0 = pseudo-MERCOSUR
  SI  <- Sm * grp
  # SD del grupo base identificado sólo si el pseudo-grupo tiene años SD
  if (sum(Sm[, "SD"] * (1 - grp)) == 0) return(c(NA_real_, NA_real_))
  X <- cbind(Xbase_fe, SI); X <- X[, colSums(abs(X)) > 0]
  b <- qr.coef(qr(X), y)
  c(b[2], b[2] + b[ncol(Xbase_fe) + 1])               # SD base, net "EU"
}
cat("\nAleatorización:", ncol(combs), "asignaciones...\n")
t0 <- Sys.time()
ri <- t(apply(combs, 2, ri_stat))
cat("tiempo:", format(Sys.time() - t0), "\n")
obs_set <- c("ARG", "BRA", "PRY", "URY")
obs <- ri_stat(obs_set)
valid <- !is.na(ri[, 1])
ri_res <- tibble(
  statistic = c("SD coefficient of the 4-country group", "SD offset (other 27 − group)"),
  observed  = c(obs[1], obs[2] - obs[1]),
  n_valid_assignments = sum(valid),
  rank_extreme = c(sum(ri[valid, 1] <= obs[1]), sum((ri[valid, 2] - ri[valid, 1]) >= obs[2] - obs[1])),
  p_one_sided  = rank_extreme / n_valid_assignments,
  p_two_sided  = c(mean(abs(ri[valid, 1] - mean(ri[valid, 1])) >= abs(obs[1] - mean(ri[valid, 1]))),
                   mean(abs((ri[valid, 2] - ri[valid, 1]) - mean(ri[valid, 2] - ri[valid, 1])) >=
                          abs((obs[2] - obs[1]) - mean(ri[valid, 2] - ri[valid, 1])))))
cat("\n=== Randomization inference ===\n"); print(ri_res)
write_csv(tibble(set = apply(combs, 2, paste, collapse = "-"), sd_group = ri[, 1],
                 net_other = ri[, 2]), file.path(PROC, "ri_distribution.csv"))

# ranking: ¿qué subconjuntos superan al observado? (¿son "exportadores"?)
top <- tibble(set = apply(combs, 2, paste, collapse = "-"), sd_group = ri[, 1]) %>%
  filter(!is.na(sd_group)) %>% arrange(sd_group) %>% head(15)
cat("\nLas 15 asignaciones con SD más negativo:\n"); print(top)

# -----------------------------------------------------------------------------
# (d) Magnitudes en todas las especificaciones (CR2 + Satterthwaite)
# -----------------------------------------------------------------------------
spec <- function(label, y, df, units = "pp") {
  fit_cr2(y, df) %>% mutate(spec = label, units = units, .before = 1)
}
nz1 <- d %>% filter(abs(g_gdp) >= 0.01)
nz05 <- d %>% filter(abs(g_gdp) >= 0.005)
eu15 <- d %>% filter(iso3 %in% c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA",
                                 "LUX","NLD","PRT","ESP","SWE","ARG","BRA","PRY","URY"))
d21 <- d %>% filter(year <= 2021)

mag <- bind_rows(
  spec("Baseline M2 (Δgap_rel)", d$d_gap_rel, d),
  spec("DV: Δ(MF−DMC)/DMC", d$d_gap_dmc, d),
  spec("DV: Δ(MF−DMC), Mt", d$d_gap_abs, d, "Mt"),
  spec("DV: Δlog(MF/DMC) (scale-free)", d$d_lratio, d),
  spec("Decomposition: ΔRTB/MF", d$d_rtb_sh, d),
  spec("Decomposition: ΔPTB/MF", d$d_ptb_sh, d),
  spec("Years ≤ 2021 (GMFD identities exact)", d21$d_gap_rel, d21),
  spec("Excl. |g_GDP| < 1%", nz1$d_gap_rel, nz1),
  spec("Excl. |g_GDP| < 0.5%", nz05$d_gap_rel, nz05),
  spec("EU-15 + MERCOSUR", eu15$d_gap_rel, eu15),
  map_dfr(obs_set, function(cc) {
    dd <- d %>% filter(iso3 != cc)
    spec(paste0("Leave out ", cc), dd$d_gap_rel, dd)
  })
)
# signo de d_ptb: gap = rtb − ptb ⇒ coef(gap) = coef(rtb) − coef(ptb)
cat("\n=== Magnitudes (CR2 + Satterthwaite) ===\n")
print(mag %>% select(spec, term, est, lo, hi, df_satt, p_cr2, n, G), n = 60, width = 200)
write_csv(mag, file.path(TABS, "regression_robustness_magnitudes.csv"))

write_csv(
  main_cr2 %>% left_join(wcr %>% select(term, p_wcr, B), by = "term"),
  file.path(TABS, "regression_small_cluster_inference.csv"))
write_csv(ri_res, file.path(TABS, "regression_randomization_inference.csv"))

cat("\n08_small_cluster_inference.R completado.\n")
