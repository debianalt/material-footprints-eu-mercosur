# =============================================================================
# _inference_helpers.R — diseño M2 explícito y ajuste CR2 + Satterthwaite.
# Sourced por 08_small_cluster_inference.R y 11_revision_r3.R.
# Requiere NONREF (estados Tapio no de referencia) y clubSandwich cargado.
# =============================================================================

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

