# =============================================================================
# 00_setup.R
# Verificación de entorno e instrucciones para ejecutar el pipeline
#
# CORRER ESTE SCRIPT PRIMERO antes de los demás
# =============================================================================

# --- Instalar paquetes faltantes ---
pkgs_needed <- c("tidyverse", "WDI", "depmixS4", "fixest", "countrycode",
                 "httr2", "zoo", "ggrepel", "broom", "patchwork",
                 "scales", "writexl", "clubSandwich")

pkgs_missing <- pkgs_needed[!pkgs_needed %in% installed.packages()[, 1]]

if (length(pkgs_missing) > 0) {
  cat("Instalando paquetes faltantes:", paste(pkgs_missing, collapse = ", "), "\n")
  install.packages(pkgs_missing, repos = "https://cloud.r-project.org")
}

cat("Todos los paquetes disponibles.\n\n")

# --- Verificar datos necesarios ---
# BASE derivado de la ubicación de este script (rename-proof)
.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")

gmfd_ok <- file.exists(file.path(BASE, "data/raw/mfa_filled.csv")) ||
           file.exists(file.path(BASE, "data/raw/gmfd_2024.csv"))
cat("=== Estado de los datos ===\n")
cat("GMFD data:            ", if (gmfd_ok) "OK" else "FALTA — ver instrucciones", "\n")
cat("COMTRADE API:         OK (public v1, no key required)\n\n")

if (!gmfd_ok) {
  cat("--- INSTRUCCIONES DATOS GMFD ---\n\n")
  cat("OPCION A (mas facil) — desde tu notebook Observable:\n")
  cat("  1. Abrir: https://observablehq.com/d/2a12e3be0c9c7894\n")
  cat("  2. Buscar la celda que hace referencia a 'mfa_filled.csv'\n")
  cat("  3. Hacer clic en el icono de adjunto/clip junto al nombre del archivo\n")
  cat("  4. Descargar -> guardar como: data/raw/mfa_filled.csv\n\n")
  cat("OPCION B — desde el sitio UNEP IRP:\n")
  cat("  1. Ir a: https://www.resourcepanel.org/global-material-flows-database\n")
  cat("  2. Descargar el CSV de la edicion 2024\n")
  cat("  3. Guardar como: data/raw/gmfd_2024.csv\n\n")
}

# --- Orden de ejecución ---
cat("=== ORDEN DE EJECUCION ===\n")
cat("1. source('R/01_data_prep.R')      # preparar datos (requiere mfa_filled.csv)\n")
cat("2. source('R/02_tapio.R')          # clasificacion Tapio\n")
cat("3. source('R/03_hmm.R')            # HMM + seleccion K  (aprox. 10-20 min)\n")
cat("4. source('R/04_gap_regression.R') # regresion de panel\n")
cat("5. source('R/05_figures.R')        # todas las figuras TIFF 300dpi\n")
cat("6. source('R/06_comtrade.R')       # Fig 8: flujos bilaterales (~40 min)\n")
cat("--- revisión JCP (2026-09) ---\n")
cat("7. source('R/07_decomposition_global.R')    # descomposición RTB/PTB + panel global\n")
cat("8. source('R/08_small_cluster_inference.R') # CR2, wild bootstrap, aleatorización (~5 min)\n")
cat("9. source('R/09_hmm_cv.R')                  # CV leave-one-country-out del HMM (~15 min)\n")
cat("10. source('R/10_revision_figures.R')       # Fig 5 (CR2), Fig S4, Fig S5\n\n")
cat("Outputs:\n")
cat("  output/tables/  -> tablas CSV para importar en Word\n")
cat("  output/figures/ -> figuras TIFF 300dpi + PNG preview\n")
