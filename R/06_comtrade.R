# =============================================================================
# 06_comtrade.R
# Bilateral MERCOSUR → EU raw material exports (UN Comtrade public API v1)
#
# Endpoint: https://comtradeapi.un.org/public/v1/preview/C/A/HS
#   - Free, no subscription key required
#   - Constraint: 1 period × 1 HS chapter per call, max 500 records/response
#   - Strategy: loop 24 years × 29 HS chapters = 696 calls (~35–60 min)
#   - Each call: all 4 MERCOSUR reporters × EU-27 partners ≤ 108 rows/call
#
# Per-call cache: data/processed/comtrade_cache/{year}_{hs}.rds
#   Delete this folder to force a full re-download.
#
# Output:
#   data/processed/comtrade_eu_mercosur.csv  — annual bilateral flows (long)
#   output/tables/comtrade_summary.csv        — summary table for manuscript
#   output/figures/Fig8_comtrade_flows.png/.tiff
# =============================================================================

library(tidyverse)
library(httr2)
library(patchwork)

BASE      <- "C:/Users/ant/OneDrive/articles_1_/material footprints/new submision EE"
PROC      <- file.path(BASE, "data/processed")
TABS      <- file.path(BASE, "output/tables")
FIGS      <- file.path(BASE, "output/figures")
CACHE_DIR <- file.path(PROC, "comtrade_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Country codes (M49 numeric, as required by Comtrade API)
# -----------------------------------------------------------------------------

# MERCOSUR full members
MERCOSUR_M49  <- "32,76,600,858"   # ARG, BRA, PRY, URY
MERCOSUR_ISO  <- c(ARG=32, BRA=76, PRY=600, URY=858)

# EU-27 M49 codes
EU27_M49 <- paste(c(
   40,  # AUT
   56,  # BEL
  100,  # BGR
  191,  # HRV
  196,  # CYP
  203,  # CZE
  208,  # DNK
  233,  # EST
  246,  # FIN
  250,  # FRA
  276,  # DEU
  300,  # GRC
  348,  # HUN
  372,  # IRL
  380,  # ITA
  428,  # LVA
  440,  # LTU
  442,  # LUX
  470,  # MLT
  528,  # NLD
  616,  # POL
  620,  # PRT
  642,  # ROU
  703,  # SVK
  705,  # SVN
  724,  # ESP
  752   # SWE
), collapse = ",")

# HS chapters: 01–27 (primary commodities) + 44, 47 (wood/pulp)
HS_CODES <- c(sprintf("%02d", 1:27), "44", "47")
YEARS    <- 2000:2023

PUBLIC_URL <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"

# -----------------------------------------------------------------------------
# 2. Single-call fetch with per-call caching and retry on 429/5xx
# -----------------------------------------------------------------------------

fetch_one <- function(year, hs_code, sleep_sec = 3) {
  cache_path <- file.path(CACHE_DIR, sprintf("%d_%s.rds", year, hs_code))

  if (file.exists(cache_path)) {
    return(readRDS(cache_path))
  }

  resp <- tryCatch(
    request(PUBLIC_URL) |>
      req_url_query(
        reporterCode = MERCOSUR_M49,
        partnerCode  = EU27_M49,
        period       = as.character(year),
        cmdCode      = hs_code,
        flowCode     = "X"          # exports
      ) |>
      req_retry(
        max_tries    = 5,
        is_transient = \(r) resp_status(r) %in% c(429L, 500L, 503L),
        backoff      = \(i) min(2^i, 60)
      ) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE),
    error = function(e) {
      cat(sprintf("  ERROR %d/%s: %s\n", year, hs_code, conditionMessage(e)))
      NULL
    }
  )

  Sys.sleep(sleep_sec)

  result <- if (!is.null(resp) && !is.null(resp$data) && length(resp$data) > 0)
    as_tibble(resp$data)
  else
    NULL

  saveRDS(result, cache_path)
  result
}

# -----------------------------------------------------------------------------
# 3. Download loop
# -----------------------------------------------------------------------------

cat(sprintf(
  "Starting download: %d years × %d HS codes = %d calls\n",
  length(YEARS), length(HS_CODES), length(YEARS) * length(HS_CODES)
))
cat("Estimated time: 35–60 min at 3–5 sec/call\n")
cat("Per-call cache in:", CACHE_DIR, "\n\n")

all_results <- list()
total <- length(YEARS) * length(HS_CODES)
n_call <- 0L

for (yr in YEARS) {
  for (hs in HS_CODES) {
    n_call <- n_call + 1L
    if (n_call == 1L || n_call %% 100 == 0L)
      cat(sprintf("  [%d/%d] year=%d hs=%s\n", n_call, total, yr, hs))

    res <- fetch_one(yr, hs)
    if (!is.null(res) && nrow(res) > 0)
      all_results[[sprintf("%d_%s", yr, hs)]] <- res
  }
}

raw_data <- bind_rows(all_results)
cat(sprintf("\nTotal rows downloaded: %d\n", nrow(raw_data)))
cat("Columns:", paste(names(raw_data), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# 4. Clean and aggregate
#    Note: public API returns reporterCode/partnerCode as M49 integers;
#    reporterISO/partnerISO are NA (not populated by public endpoint).
#    netWgt is 0 for aggregate records; only primaryValue (USD FOB) is reliable.
# -----------------------------------------------------------------------------

# M49 → ISO3 lookup (only MERCOSUR reporters needed for grouping)
m49_to_iso3 <- c(
  "32"  = "ARG", "76"  = "BRA", "600" = "PRY", "858" = "URY",
  "40"  = "AUT", "56"  = "BEL", "100" = "BGR", "191" = "HRV",
  "196" = "CYP", "203" = "CZE", "208" = "DNK", "233" = "EST",
  "246" = "FIN", "250" = "FRA", "276" = "DEU", "300" = "GRC",
  "348" = "HUN", "372" = "IRL", "380" = "ITA", "428" = "LVA",
  "440" = "LTU", "442" = "LUX", "470" = "MLT", "528" = "NLD",
  "616" = "POL", "620" = "PRT", "642" = "ROU", "703" = "SVK",
  "705" = "SVN", "724" = "ESP", "752" = "SWE"
)

flows <- raw_data |>
  filter(!is.na(primaryValue), primaryValue > 0) |>
  mutate(
    reporter_iso3 = m49_to_iso3[as.character(reporterCode)],
    hs2           = substr(as.character(cmdCode), 1, 2),
    year          = as.integer(refYear),
    value_musd    = primaryValue / 1e6    # USD → million USD
  ) |>
  filter(!is.na(reporter_iso3)) |>        # keep only MERCOSUR reporters
  mutate(material_cat = case_when(
    hs2 %in% sprintf("%02d", 1:15)  ~ "Agricultural biomass",
    hs2 %in% c("44", "47")          ~ "Forest products",
    hs2 %in% c("25", "26")          ~ "Metal ores & minerals",
    hs2 == "27"                     ~ "Fossil fuels",
    hs2 %in% sprintf("%02d", 16:24) ~ "Processed foods/beverages",
    TRUE                            ~ "Other HS 01-27"
  ))

cat("Rows after filter:", nrow(flows), "\n")
cat("Reporters:", paste(sort(unique(flows$reporter_iso3)), collapse=", "), "\n")

# Annual aggregate by MERCOSUR reporter + material category
annual_by_country <- flows |>
  group_by(reporter_iso3, year, material_cat) |>
  summarise(value_musd = sum(value_musd, na.rm = TRUE), .groups = "drop")

# Total MERCOSUR → EU-27
annual_total <- flows |>
  group_by(year, material_cat) |>
  summarise(value_musd = sum(value_musd, na.rm = TRUE), .groups = "drop") |>
  mutate(reporter_iso3 = "MERCOSUR_total")

comtrade_long <- bind_rows(annual_by_country, annual_total)
write_csv(comtrade_long, file.path(PROC, "comtrade_eu_mercosur.csv"))
cat("Saved:", file.path(PROC, "comtrade_eu_mercosur.csv"), "\n")

# Summary table for manuscript
comtrade_summary <- annual_total |>
  group_by(year) |>
  summarise(total_value_musd = sum(value_musd, na.rm = TRUE), .groups = "drop")
write_csv(comtrade_summary, file.path(TABS, "comtrade_summary.csv"))
cat("Saved:", file.path(TABS, "comtrade_summary.csv"), "\n")

# -----------------------------------------------------------------------------
# 5. Figure 8 — EU–MERCOSUR raw material trade flows (value, USD million)
# -----------------------------------------------------------------------------

pal_cat <- c(
  "Agricultural biomass"      = "#4DAF4A",
  "Forest products"           = "#984EA3",
  "Metal ores & minerals"     = "#FF7F00",
  "Fossil fuels"              = "#A65628",
  "Processed foods/beverages" = "#377EB8",
  "Other HS 01-27"            = "#999999"
)

# Panel A: stacked area — total value by category
p_a <- annual_total |>
  filter(material_cat != "Other HS 01-27") |>
  ggplot(aes(year, value_musd, fill = material_cat)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = pal_cat, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL, y = "Export value (million USD)",
    title = "A. MERCOSUR primary commodity exports to EU-27 by category"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text     = element_text(size = 9))

# Panel B: total value over time
p_b <- comtrade_summary |>
  ggplot(aes(year, total_value_musd)) +
  geom_line(linewidth = 1.1, colour = "#2166AC") +
  geom_point(size = 2,       colour = "#2166AC") +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year", y = "Total export value (million USD)",
    title = "B. Total MERCOSUR → EU-27 primary exports"
  ) +
  theme_bw(base_size = 11)

# Panel C: by MERCOSUR country
p_c <- annual_by_country |>
  group_by(reporter_iso3, year) |>
  summarise(value_musd = sum(value_musd, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, value_musd, colour = reporter_iso3)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(
    values = c(ARG = "#D6604D", BRA = "#F4A582", PRY = "#92C5DE", URY = "#4393C3"),
    name   = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = "Year", y = "Export value (million USD)",
    title = "C. By country"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

fig8 <- (p_a | p_b) / p_c +
  plot_annotation(
    title   = "Figure 8. MERCOSUR→EU-27 primary commodity exports, HS 01–27 + 44, 47 (2000–2023)",
    caption = "Source: UN Comtrade public API v1 (2024). FOB export value in million USD.",
    theme   = theme(plot.title = element_text(face = "bold", size = 12))
  )

ggsave(file.path(FIGS, "Fig8_comtrade_flows.tiff"),
       fig8, width = 12, height = 8, dpi = 300, device = "tiff")
ggsave(file.path(FIGS, "Fig8_comtrade_flows.png"),
       fig8, width = 12, height = 8, dpi = 150)
cat("Saved Fig8_comtrade_flows.tiff/.png\n")

cat("\n06_comtrade.R completed.\n")
