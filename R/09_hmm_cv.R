# =============================================================================
# 09_hmm_cv.R   (revisión JCP, 2026-09)
#
# Respuesta a R2 / R3#2: selección de K "más objetiva". BIC/ICL eligen K=2, AIC K=4
# y el paper adoptaba K=3 por interpretabilidad. Criterio predictivo fuera de
# muestra: validación cruzada bloqueada por país (leave-one-country-out, 31
# folds). Para cada fold y K se ajusta el HMM pooled en 30 países y se evalúa la
# log-verosimilitud de la secuencia del país excluido con esos parámetros.
# Regla fijada de antemano: el K con mayor log-verosimilitud predictiva total
# es el modelo principal.
# =============================================================================

suppressMessages({library(tidyverse); library(depmixS4)})

.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC <- file.path(BASE, "data/processed")
TABS <- file.path(BASE, "output/tables")

TAPIO_LEVELS <- c("SD", "WD", "EC", "END", "WND", "RC", "RD", "SND")
N_RESTARTS <- as.integer(Sys.getenv("CV_RESTARTS", "10"))
KS <- 2:5

pt <- read_csv(file.path(PROC, "panel_tapio.csv"), show_col_types = FALSE) %>%
  filter(!is.na(tapio)) %>% arrange(iso3, year)

mk <- function(dd, K) {
  depmix(list(tapio_f ~ 1),
         data = data.frame(tapio_f = factor(dd$tapio, levels = TAPIO_LEVELS)),
         nstates = K, ntimes = dd %>% count(iso3) %>% pull(n),
         family = list(multinomial("identity")))
}

fit_best <- function(dd, K) {
  best <- NULL; best_ll <- -Inf
  for (i in seq_len(N_RESTARTS)) {
    set.seed(i * 137 + K * 1000)
    f <- tryCatch(suppressWarnings(fit(mk(dd, K), verbose = FALSE,
                                       emcontrol = em.control(maxit = 500, tol = 1e-8))),
                  error = function(e) NULL)
    if (!is.null(f)) {
      ll <- as.numeric(logLik(f))
      if (is.finite(ll) && ll > best_ll) { best <- f; best_ll <- ll }
    }
  }
  best
}

# Log-verosimilitud del país excluido por algoritmo forward escalado, con los
# parámetros del modelo ajustado (getpars: K iniciales, K×K transición por filas,
# K×8 emisión por estado). No se usa setpars sobre un modelo del país excluido
# porque depmix descarta niveles no observados y cambia la longitud de parámetros.
heldout_ll <- function(fitted, dd, K) {
  p  <- getpars(fitted)
  pi0 <- p[1:K]
  A  <- matrix(p[(K + 1):(K + K * K)], nrow = K, byrow = TRUE)
  B  <- matrix(p[(K + K * K + 1):length(p)], nrow = K, byrow = TRUE)
  stopifnot(ncol(B) == length(TAPIO_LEVELS))
  B  <- (B + 1e-4) / rowSums(B + 1e-4)       # suavizado mínimo contra log(0)
  obs <- match(dd$tapio, TAPIO_LEVELS)
  alpha <- pi0 * B[, obs[1]]; ll <- log(sum(alpha)); alpha <- alpha / sum(alpha)
  for (t in seq_along(obs)[-1]) {
    alpha <- as.vector(alpha %*% A) * B[, obs[t]]
    ll <- ll + log(sum(alpha)); alpha <- alpha / sum(alpha)
  }
  ll
}

countries <- unique(pt$iso3)
folds_file <- file.path(PROC, "hmm_cv_folds.csv")
if (Sys.getenv("CV_REUSE") == "1" && file.exists(folds_file)) {
  cv <- read_csv(folds_file, show_col_types = FALSE)
} else {
res <- list()
t0 <- Sys.time()
for (cc in countries) {
  train <- pt %>% filter(iso3 != cc); test <- pt %>% filter(iso3 == cc)
  for (K in KS) {
    f <- fit_best(train, K)
    res[[length(res) + 1]] <- tibble(fold = cc, K = K,
                                     ll_test = if (is.null(f)) NA_real_ else heldout_ll(f, test, K),
                                     n_test = nrow(test))
  }
  cat(sprintf("fold %s listo (%s)\n", cc, format(Sys.time() - t0)))
}
cv <- bind_rows(res)
write_csv(cv, folds_file)
}

cv_sum <- cv %>%
  group_by(K) %>%
  summarise(total_ll_test = sum(ll_test), mean_ll_per_obs = sum(ll_test) / sum(n_test),
            n_folds = sum(!is.na(ll_test)), .groups = "drop") %>%
  mutate(delta_vs_best = total_ll_test - max(total_ll_test))
# ¿cuántos folds prefiere cada K?
wins <- cv %>% group_by(fold) %>% slice_max(ll_test, n = 1, with_ties = FALSE) %>% ungroup() %>% count(K, name = "folds_won")
cv_sum <- cv_sum %>% left_join(wins, by = "K") %>% mutate(folds_won = replace_na(folds_won, 0L))
# diferencia pareada vs. el ganador (se de la suma por folds)
best_K <- cv_sum$K[which.max(cv_sum$total_ll_test)]
paired <- cv %>% dplyr::select(fold, K, ll_test) %>% pivot_wider(names_from = K, values_from = ll_test, names_prefix = "K")
cv_sum$se_delta <- sapply(cv_sum$K, function(k) {
  dlt <- paired[[paste0("K", k)]] - paired[[paste0("K", best_K)]]
  sqrt(length(dlt)) * sd(dlt)
})
print(cv_sum)
cat(sprintf("\nK ganador por log-verosimilitud predictiva fuera de muestra: K = %d\n", best_K))
write_csv(cv_sum, file.path(TABS, "hmm_cv_selection.csv"))
cat("09_hmm_cv.R completado.\n")
