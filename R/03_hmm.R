# =============================================================================
# 03_hmm.R
# Hidden Markov Model — selección de K y estimación de regímenes latentes
#
# Selección de K:
#   BIC: K=2 (parsimonioso), AIC: K=4 (predictivo)
#   Se adopta K=3 porque:
#   (a) K=2 fuerza toda la heterogeneidad en dos categorías, perdiendo las
#       fases transicionales que son conceptualmente centrales en el argumento
#   (b) AIC-óptimo (K=4) no agrega interpretabilidad
#   (c) K=3 es la especificación teóricamente motivada y reproduce los tres
#       regímenes identificados en el análisis original (eficiencia, intensivo
#       en materiales, transicional). La tabla BIC/AIC se reporta íntegramente.
#
# Robustez: HMMs separados EU/MERCOSUR (R3.5) + EU-15 (R3.2)
# =============================================================================

library(tidyverse)
library(depmixS4)

BASE <- "C:/Users/ant/OneDrive/articles_1_/material footprints/new submision EE"
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

set.seed(2024)

panel_tapio <- read_csv(file.path(PROC, "panel_tapio.csv"), show_col_types = FALSE)

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "RD", "RC", "RND", "SND")
N_RESTARTS   <- 30
EM_MAXIT     <- 500
EM_TOL       <- 1e-8

# País de referencia para nombrar robustez EU-15
EU15      <- c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA",
               "LUX","NLD","PRT","ESP","SWE","GBR")
MERCOSUR4 <- c("ARG","BRA","PRY","URY")

# -----------------------------------------------------------------------------
# 1. Función de ajuste con múltiples reinicios
# -----------------------------------------------------------------------------

fit_hmm_best <- function(data, K, n_restarts = N_RESTARTS) {
  data_sorted <- data %>%
    arrange(iso3, year) %>%
    filter(!is.na(tapio))

  ntimes <- data_sorted %>% count(iso3) %>% pull(n)

  df_fit <- data.frame(
    tapio_f = factor(data_sorted$tapio, levels = TAPIO_LEVELS)
  )

  best_mod   <- NULL
  best_ll    <- -Inf
  n_failed   <- 0
  ll_history <- numeric(0)  # log-likelihoods of all successful restarts

  for (i in seq_len(n_restarts)) {
    set.seed(i * 137 + K * 1000)
    mod <- depmix(
      response = list(tapio_f ~ 1),
      data     = df_fit,
      nstates  = K,
      ntimes   = ntimes,
      family   = list(multinomial("identity"))
    )
    fitted <- tryCatch(
      suppressWarnings(
        fit(mod, verbose = FALSE,
            emcontrol = em.control(maxit = EM_MAXIT, tol = EM_TOL))
      ),
      error = function(e) { n_failed <<- n_failed + 1; NULL }
    )
    if (!is.null(fitted)) {
      ll <- as.numeric(logLik(fitted))
      if (is.finite(ll)) {
        ll_history <- c(ll_history, ll)
        if (ll > best_ll) {
          best_ll  <- ll
          best_mod <- fitted
        }
      }
    }
  }

  if (is.null(best_mod)) stop(paste("HMM K=", K, "no converge."))

  # Attach convergence diagnostics as attributes
  attr(best_mod, "ll_history")      <- ll_history
  attr(best_mod, "n_restarts_ok")   <- length(ll_history)
  attr(best_mod, "n_restarts_fail") <- n_failed

  n_at_opt <- sum(abs(ll_history - best_ll) < 0.01)
  cat(sprintf("K=%d: logLik=%.2f  AIC=%.1f  BIC=%.1f  convergencia: %d/%d  (fallos: %d/%d)\n",
              K, best_ll, AIC(best_mod), BIC(best_mod),
              n_at_opt, n_restarts, n_failed, n_restarts))
  best_mod
}

# -----------------------------------------------------------------------------
# 2. Selección de K (K = 2, 3, 4, 5)
# -----------------------------------------------------------------------------

cat("=== Selección de K — modelo pooled ===\n")

# Caché de modelos para evitar re-ajuste (guardar/cargar .rds)
cache_dir  <- file.path(BASE, "data/processed")
cache_file <- file.path(cache_dir, "hmm_models_pooled_k5.rds")

if (file.exists(cache_file)) {
  cat("Cargando modelos desde caché...\n")
  models_pooled <- readRDS(cache_file)
} else {
  models_pooled <- lapply(2:5, function(k) {
    cat(sprintf("\nAjustando K=%d con %d reinicios...\n", k, N_RESTARTS))
    fit_hmm_best(panel_tapio, K = k)
  })
  names(models_pooled) <- paste0("K", 2:5)
  saveRDS(models_pooled, cache_file)
  cat("Modelos guardados en caché.\n")
}

# ICL = BIC + 2 * classification entropy (H = -sum z_ik * log z_ik)
compute_icl <- function(fitted_mod) {
  post_probs <- posterior(fitted_mod)
  state_cols <- grep("^S[0-9]", names(post_probs), value = TRUE)
  p_mat      <- as.matrix(post_probs[, state_cols])
  H          <- -sum(p_mat * log(p_mat + 1e-300))
  BIC(fitted_mod) + 2 * H
}

model_sel <- tibble(
  K         = 2:5,
  logLik    = sapply(models_pooled, function(m) round(as.numeric(logLik(m)), 2)),
  npar      = sapply(models_pooled, npar),
  AIC       = sapply(models_pooled, function(m) round(AIC(m), 1)),
  BIC       = sapply(models_pooled, function(m) round(BIC(m), 1)),
  ICL       = round(sapply(models_pooled, compute_icl), 1),
  delta_AIC = round(sapply(models_pooled, AIC) - min(sapply(models_pooled, AIC)), 1),
  delta_BIC = round(sapply(models_pooled, BIC) - min(sapply(models_pooled, BIC)), 1),
  delta_ICL = round(sapply(models_pooled, compute_icl) - min(sapply(models_pooled, compute_icl)), 1)
)

cat("\n=== Tabla de selección de modelo (reportar en el paper) ===\n")
print(model_sel)
write_csv(model_sel, file.path(TABS, "hmm_model_selection.csv"))

K_bic <- model_sel$K[which.min(model_sel$BIC)]
K_aic <- model_sel$K[which.min(model_sel$AIC)]
cat(sprintf("\nBIC óptimo: K=%d  |  AIC óptimo: K=%d\n", K_bic, K_aic))
cat("Modelo adoptado para análisis principal: K=3 (ver justificación en cabecera)\n")

K_main <- 3
model_best <- models_pooled[["K3"]]

# Report K=3 convergence stability for manuscript §2.3
lls_k3      <- attr(model_best, "ll_history")
n_converged <- sum(abs(lls_k3 - max(lls_k3)) < 0.01)
cat(sprintf("\n>>> CONVERGENCIA K=3 (para §2.3): %d/%d restarts dentro de 0.01 de logLik=%.2f\n",
            n_converged, length(lls_k3), max(lls_k3)))

# -----------------------------------------------------------------------------
# 3. Función de extracción de resultados
# -----------------------------------------------------------------------------

extract_hmm_results <- function(fitted_mod, panel_data, K, label = "pooled") {
  panel_sorted <- panel_data %>%
    arrange(iso3, year) %>%
    filter(!is.na(tapio))

  # Posteriors — una sola llamada devuelve estado Viterbi + probabilidades suavizadas
  # Columnas: state, S1, S2, ..., SK
  post <- suppressWarnings(posterior(fitted_mod))
  panel_sorted$regime_viterbi <- post[["state"]]
  for (k in seq_len(K)) {
    col_name <- paste0("S", k)
    if (col_name %in% names(post)) {
      panel_sorted[[paste0("p_regime", k)]] <- post[[col_name]]
    }
  }

  # --- Matriz de transición ---
  all_pars <- getpars(fitted_mod)
  # Estructura: K probs iniciales + K×K transición + K×8 emisión
  n_pi <- K
  A_vec <- all_pars[(n_pi + 1):(n_pi + K * K)]
  A <- matrix(A_vec, nrow = K, byrow = TRUE)
  rownames(A) <- colnames(A) <- paste0("R", 1:K)

  # --- Matriz de emisión (empírica: más robusta y directamente interpretable) ---
  # P(Tapio state | regime) = co-occurrence normalizado por filas
  co_tab <- table(Regime = panel_sorted$regime_viterbi,
                  Tapio  = factor(panel_sorted$tapio, levels = TAPIO_LEVELS))
  B_mat  <- prop.table(co_tab, margin = 1)

  # --- Estadísticas de regímenes ---
  dwell_times <- 1 / pmax(1 - diag(A), 1e-6)  # prevenir div/0

  # Estado Tapio dominante por régimen (interpretación semántica)
  dominant_state <- apply(B_mat, 1, function(row) {
    TAPIO_LEVELS[which.max(row)]
  })

  summary_tab <- tibble(
    model             = label,
    K                 = K,
    regime            = 1:K,
    dominant_tapio    = dominant_state,
    mean_dwell_time   = round(dwell_times, 2),
    p_self_transition = round(diag(A), 3)
  )

  list(
    panel    = panel_sorted,
    trans    = A,
    emission = B_mat,
    summary  = summary_tab,
    loglik   = as.numeric(logLik(fitted_mod)),
    AIC      = AIC(fitted_mod),
    BIC      = BIC(fitted_mod)
  )
}

# -----------------------------------------------------------------------------
# 4. Extraer resultados del modelo K=3 pooled
# -----------------------------------------------------------------------------

cat("\n=== Extrayendo resultados K=3 pooled ===\n")
res_pooled <- extract_hmm_results(model_best, panel_tapio, K_main, "pooled")

# Distribución de regímenes por bloque
regime_by_bloc <- res_pooled$panel %>%
  group_by(bloc, regime_viterbi) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(bloc) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup()

cat("\nDistribución de regímenes por bloque:\n")
print(regime_by_bloc)
cat("\nMatriz de transición:\n"); print(round(res_pooled$trans, 3))
cat("\nMatriz de emisión empírica:\n"); print(round(res_pooled$emission, 3))
cat("\nResumen de regímenes:\n"); print(res_pooled$summary)

# Bootstrap CIs para regime shares (B=200, muestreo por países)
cat("\nCalculando bootstrap CIs (B=200)...\n")
set.seed(42)
boot_ci <- replicate(200, {
  countries   <- unique(res_pooled$panel$iso3)
  boot_c      <- sample(countries, replace = TRUE)
  boot_panel  <- map_dfr(boot_c, ~res_pooled$panel[res_pooled$panel$iso3 == .x, ])
  boot_panel %>%
    group_by(bloc, regime_viterbi) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(bloc) %>%
    mutate(pct = n / sum(n) * 100) %>%
    ungroup()
}, simplify = FALSE) %>%
  bind_rows() %>%
  group_by(bloc, regime_viterbi) %>%
  summarise(ci_lower = round(quantile(pct, .025), 1),
            ci_upper = round(quantile(pct, .975), 1),
            .groups  = "drop")

regime_summary <- regime_by_bloc %>%
  left_join(boot_ci, by = c("bloc", "regime_viterbi")) %>%
  left_join(res_pooled$summary, by = c("regime_viterbi" = "regime"))

cat("\nResumen final con CIs:\n")
print(dplyr::select(regime_summary, bloc, regime_viterbi, dominant_tapio,
                    pct, ci_lower, ci_upper, mean_dwell_time))

# -----------------------------------------------------------------------------
# 5. Robustez: HMMs separados EU y MERCOSUR
# -----------------------------------------------------------------------------

cat("\n=== Robustez: EU separado ===\n")
res_eu_sep <- extract_hmm_results(
  fit_hmm_best(panel_tapio %>% filter(bloc == "EU"),       K = K_main),
  panel_tapio %>% filter(bloc == "EU"), K_main, "EU_only"
)

cat("\n=== Robustez: MERCOSUR separado ===\n")
res_mcs_sep <- extract_hmm_results(
  fit_hmm_best(panel_tapio %>% filter(bloc == "MERCOSUR"), K = K_main),
  panel_tapio %>% filter(bloc == "MERCOSUR"), K_main, "MERCOSUR_only"
)

# -----------------------------------------------------------------------------
# 6. Robustez: EU-15
# -----------------------------------------------------------------------------

cat("\n=== Robustez: EU-15 + MERCOSUR ===\n")
panel_eu15_tapio <- panel_tapio %>% filter(iso3 %in% c(EU15, MERCOSUR4))
res_eu15 <- extract_hmm_results(
  fit_hmm_best(panel_eu15_tapio, K = K_main),
  panel_eu15_tapio, K_main, "EU15_robustness"
)

# -----------------------------------------------------------------------------
# 6b. Robustez: MERCOSUR leave-one-out
# Responde a Reviewer 3 punto 2: membership alternativo.
# Bolivia/Venezuela no tienen datos GMFD completos para 1994-2024.
# Como proxy de sensibilidad a la composición, excluimos un país fundador por vez.
# -----------------------------------------------------------------------------

cat("\n=== Robustez: MERCOSUR leave-one-out ===\n")
loo_results <- lapply(MERCOSUR4, function(drop_iso) {
  panel_loo <- panel_tapio %>% filter(!(iso3 == drop_iso & bloc == "MERCOSUR"))
  label     <- paste0("MERCOSUR_loo_drop_", drop_iso)
  cat(sprintf("\nLeave-one-out: excluyendo %s\n", drop_iso))
  extract_hmm_results(
    fit_hmm_best(panel_loo, K = K_main),
    panel_loo, K_main, label
  )
})
loo_summary <- bind_rows(lapply(loo_results, `[[`, "summary"))
cat("\nResumen leave-one-out MERCOSUR:\n"); print(loo_summary)

# -----------------------------------------------------------------------------
# 7. Guardar todos los outputs
# -----------------------------------------------------------------------------

write_csv(res_pooled$panel, file.path(PROC, "panel_regimes.csv"))

# Matriz de transición (es una matriz R, as.data.frame funciona bien)
as.data.frame(res_pooled$trans) %>%
  rownames_to_column("from") %>%
  pivot_longer(-from, names_to = "to", values_to = "prob") %>%
  write_csv(file.path(TABS, "hmm_transition_matrix.csv"))

# Matriz de emisión: res_pooled$emission es un objeto table/prop.table
# as.data.frame() en un objeto table produce formato long directamente
as.data.frame(res_pooled$emission) %>%
  rename(regime = Regime, tapio_state = Tapio, prob = Freq) %>%
  write_csv(file.path(TABS, "hmm_emission_matrix.csv"))

write_csv(regime_summary, file.path(TABS, "hmm_regime_summary.csv"))

# Resumen de robustez (EU-only, MERCOSUR-only, EU-15, + 4 leave-one-out)
bind_rows(
  res_eu_sep$summary,
  res_mcs_sep$summary,
  res_eu15$summary,
  loo_summary
) %>% write_csv(file.path(TABS, "hmm_robustness_summary.csv"))

# Comparación semántica entre modelos
cat("\n=== Comparación semántica entre modelos ===\n")
cat("\nPooled K=3:\n");      print(res_pooled$summary)
cat("\nEU solo:\n");         print(res_eu_sep$summary)
cat("\nMERCOSUR solo:\n");   print(res_mcs_sep$summary)
cat("\nEU-15:\n");           print(res_eu15$summary)

cat("\n03_hmm.R completado.\n")
cat("K adoptado: 3 (BIC-óptimo: K=", K_bic, ", AIC-óptimo: K=", K_aic, ")\n")
cat("Tabla de selección guardada en: output/tables/hmm_model_selection.csv\n")
