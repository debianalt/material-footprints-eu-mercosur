# =============================================================================
# 01_data_prep.R
# Material Footprint Decoupling — EU and MERCOSUR (1994-2024)
# New Submission: Ecological Economics
#
# Fuentes de datos:
#   - mfa_filled.csv: GMFD UNEP IRP (Observable attachment, descargado automáticamente)
#     Formato: Country | Flow name | Flow code | Flow unit | 1970 | ... | 2024
#     Flujos usados: MF (Material Footprint), DMC (Domestic Material Consumption)
#     Unidades: toneladas (t) → se convierten a Mt (millones de toneladas)
#   - WDI: GDP constante 2015 USD (World Bank) para tasas de crecimiento
# =============================================================================

library(tidyverse)
library(WDI)
library(countrycode)

# BASE derivado de la ubicación de este script (rename-proof)
.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
RAW  <- file.path(BASE, "data/raw")
PROC <- file.path(BASE, "data/processed")

# -----------------------------------------------------------------------------
# 1. Cargar mfa_filled.csv
# -----------------------------------------------------------------------------

mfa_path <- file.path(RAW, "mfa_filled.csv")

if (!file.exists(mfa_path)) {
  stop(paste0(
    "\n\nmfa_filled.csv no encontrado en: ", mfa_path,
    "\nDescargarlo desde https://observablehq.com/d/2a12e3be0c9c7894",
    "\n(ícono de adjunto junto a la celda mfa_filled)\n"
  ))
}

mfa_raw <- read_csv(mfa_path, show_col_types = FALSE)
cat("mfa_filled.csv cargado:", nrow(mfa_raw), "filas x", ncol(mfa_raw), "columnas\n")
cat("Flow codes disponibles:", paste(sort(unique(mfa_raw$`Flow code`)), collapse = ", "), "\n\n")

# -----------------------------------------------------------------------------
# 2. Extraer MF y DMC, pivotar a formato long, convertir a Mt
# -----------------------------------------------------------------------------

year_cols <- names(mfa_raw)[str_detect(names(mfa_raw), "^\\d{4}$")]
cat("Años en el archivo:", min(year_cols), "-", max(year_cols), "\n")

mfa_long <- mfa_raw %>%
  filter(`Flow code` %in% c("MF", "DMC")) %>%
  select(Country = Country,
         flow_code = `Flow code`,
         all_of(year_cols)) %>%
  pivot_longer(
    cols      = all_of(year_cols),
    names_to  = "year",
    values_to = "value_t"        # valores en toneladas
  ) %>%
  mutate(
    year     = as.integer(year),
    value_mt = value_t / 1e6,    # convertir toneladas → millones de toneladas (Mt)
    # ISO3 desde nombre de país (countrycode es muy preciso para este dataset)
    iso3 = countrycode(Country, origin = "country.name", destination = "iso3c",
                       warn = FALSE)
  ) %>%
  filter(!is.na(iso3), !is.na(value_mt), year >= 1993, year <= 2024)

# Pivotar MF y DMC a columnas separadas
gmfd <- mfa_long %>%
  pivot_wider(
    id_cols     = c(iso3, Country, year),
    names_from  = flow_code,
    values_from = value_mt,
    values_fn   = mean
  ) %>%
  rename(country = Country, mf_mt = MF, dmc_mt = DMC) %>%
  filter(!is.na(mf_mt), !is.na(dmc_mt))

cat("\nGMFD procesado:", nrow(gmfd), "obs,",
    n_distinct(gmfd$iso3), "países,",
    min(gmfd$year), "-", max(gmfd$year), "\n")

# Verificar cobertura de nuestros países clave
EU27 <- c("AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
          "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
          "POL","PRT","ROU","SVK","SVN","ESP","SWE")
MERCOSUR4 <- c("ARG","BRA","PRY","URY")
EU15 <- c("AUT","BEL","DNK","FIN","FRA","DEU","GRC","IRL","ITA",
          "LUX","NLD","PRT","ESP","SWE","GBR")

all_target <- c(EU27, MERCOSUR4)
missing    <- all_target[!all_target %in% unique(gmfd$iso3)]
if (length(missing) > 0) cat("Países faltantes en GMFD:", paste(missing, collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 3. GDP — World Bank, constante 2015 USD
# -----------------------------------------------------------------------------

cat("\nDescargando GDP del World Bank (constante 2015 USD)...\n")

gdp_raw <- WDI(
  indicator = "NY.GDP.MKTP.KD",
  country   = "all",
  start     = 1993,
  end       = 2024
)

gdp <- gdp_raw %>%
  filter(!is.na(NY.GDP.MKTP.KD)) %>%
  rename(gdp_2015usd = NY.GDP.MKTP.KD) %>%
  mutate(iso3 = toupper(iso3c)) %>%
  select(iso3, year, gdp_2015usd)

cat("GDP descargado:", nrow(gdp), "obs\n")

# -----------------------------------------------------------------------------
# 4. Merge y construcción del panel principal
# -----------------------------------------------------------------------------

panel <- gmfd %>%
  filter(iso3 %in% all_target) %>%
  inner_join(gdp, by = c("iso3", "year")) %>%
  filter(year >= 1994, year <= 2024) %>%
  mutate(
    bloc = case_when(
      iso3 %in% EU27      ~ "EU",
      iso3 %in% MERCOSUR4 ~ "MERCOSUR"
    ),
    # *** GAP DE EXTERNALIZACIÓN — DEFINICIÓN RELATIVA (única en todo el análisis) ***
    # (MF - DMC) / MF
    # Positivo: país importa más materiales de los que extrae (net importer)
    # Negativo: país exporta materiales incorporados en bienes (net exporter)
    gap_rel = (mf_mt - dmc_mt) / mf_mt,
    # Gap absoluto (solo para referencia, NO se usa como variable principal)
    gap_abs_mt = mf_mt - dmc_mt
  ) %>%
  filter(!is.na(gap_rel), !is.na(gdp_2015usd)) %>%
  arrange(iso3, year)

cat("\nPanel principal:", nrow(panel), "obs,",
    n_distinct(panel$iso3), "países,",
    n_distinct(panel$year), "años\n")

# Cobertura por bloque y país
coverage <- panel %>%
  group_by(bloc, iso3) %>%
  summarise(n_yr = n(), yr_min = min(year), yr_max = max(year), .groups = "drop")
cat("\nCobertura:\n"); print(coverage, n = 40)

# Advertir si hay huecos
expected_years <- length(1994:2024)
incomplete <- coverage %>% filter(n_yr < expected_years)
if (nrow(incomplete) > 0) {
  cat("\nPaíses con años incompletos (", expected_years, " esperados):\n")
  print(incomplete)
}

# -----------------------------------------------------------------------------
# 5. Panel EU-15 para robustez
# -----------------------------------------------------------------------------

panel_eu15 <- panel %>%
  filter(iso3 %in% c(EU15, MERCOSUR4)) %>%
  mutate(bloc_eu15 = if_else(iso3 %in% EU15, "EU15", "MERCOSUR"))

# -----------------------------------------------------------------------------
# 6. Estadísticas descriptivas del gap
# -----------------------------------------------------------------------------

gap_summary <- panel %>%
  group_by(bloc) %>%
  summarise(
    n          = n(),
    mf_mean    = mean(mf_mt),
    dmc_mean   = mean(dmc_mt),
    gap_mean   = mean(gap_rel),
    gap_median = median(gap_rel),
    gap_sd     = sd(gap_rel),
    gap_min    = min(gap_rel),
    gap_max    = max(gap_rel),
    .groups    = "drop"
  )

cat("\nEstadísticas gap_rel por bloque:\n")
print(gap_summary, width = 100)

# -----------------------------------------------------------------------------
# 7. Guardar
# -----------------------------------------------------------------------------

write_csv(panel,       file.path(PROC, "panel_main.csv"))
write_csv(panel_eu15,  file.path(PROC, "panel_eu15.csv"))
write_csv(gap_summary, file.path(PROC, "gap_summary_descriptive.csv"))

cat("\n01_data_prep.R completado.\n")
cat("Guardado: panel_main.csv (", nrow(panel), "obs)\n")
cat("Guardado: panel_eu15.csv (", nrow(panel_eu15), "obs)\n")
