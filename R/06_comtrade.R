# =============================================================================
# 06_comtrade.R
# Bilateral EU–MERCOSUR raw material trade flows (UN Comtrade+, 2000–2024)
#
# Requires a free API key from: https://comtradeplus.un.org/
#   1. Register at the URL above
#   2. Copy your primary key
#   3. EITHER set it for this session:  comtradr::set_primary_comtrade_key("YOUR_KEY")
#      OR add it permanently to .Renviron: COMTRADE_PRIMARY=YOUR_KEY
#      (run usethis::edit_r_environ() to open .Renviron, then restart R)
#
# Rate limits (free tier): 500 requests/hour, 250 records/response
# This script batches requests and sleeps between calls to stay within limits.
#
# Output:
#   data/processed/comtrade_eu_mercosur.csv  — annual bilateral flows (long)
#   output/tables/comtrade_summary.csv        — summary table for manuscript
#   output/figures/Fig8_comtrade_flows.png    — Fig. 8
# =============================================================================

library(tidyverse)
library(comtradr)

BASE  <- "C:/Users/ant/OneDrive/articles_1_/material footprints/new submision EE"
PROC  <- file.path(BASE, "data/processed")
TABS  <- file.path(BASE, "output/tables")
FIGS  <- file.path(BASE, "output/figures")

# -----------------------------------------------------------------------------
# 0. API key check
# -----------------------------------------------------------------------------

key_available <- tryCatch({
  # comtradr 1.0.x reads COMTRADE_PRIMARY from .Renviron automatically;
  # calling set_primary_comtrade_key() overrides for the session.
  # If the key is already set, ct_get_data() will use it.
  k <- Sys.getenv("COMTRADE_PRIMARY")
  nchar(k) > 0
}, error = function(e) FALSE)

if (!key_available) {
  stop(
    "\n\n",
    "  No Comtrade+ API key found.\n",
    "  1. Register (free) at: https://comtradeplus.un.org/\n",
    "  2. Copy your Primary Key from your profile page.\n",
    "  3. Add to .Renviron (run usethis::edit_r_environ()):\n",
    "        COMTRADE_PRIMARY=paste_your_key_here\n",
    "  4. Restart R and re-run this script.\n\n"
  )
}

cat("API key found. Proceeding with download.\n")

# -----------------------------------------------------------------------------
# 1. Define reporter/partner/commodity scope
# -----------------------------------------------------------------------------

# MERCOSUR full members
MERCOSUR_ISO <- c("ARG", "BRA", "PRY", "URY")

# EU-27 ISO3 codes (as per Comtrade country list)
EU27_ISO <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)

# HS chapters 01–27: primary commodities + raw materials
# We group into 5 material categories for the paper:
#   BIOTIC_AG  : HS 01–15 (live animals, food, raw agricultural materials, fats/oils)
#   FOREST     : HS 44, 47 (wood + pulp — fetched separately below)
#   MINERALS   : HS 25–26 (salt/stone + ores/metals)
#   FOSSIL     : HS 27 (mineral fuels)
#   OTHER_RAW  : HS 16–24 (prepared foods, beverages, tobacco — borderline)

# For simplicity in a first pass, fetch HS 01–27 as a block and HS 44,47 separately.
HS_RAW      <- sprintf("%02d", 1:27)           # 27 chapters
HS_FOREST   <- c("44", "47")
ALL_HS      <- c(HS_RAW, HS_FOREST)

YEARS       <- 2000:2023  # Comtrade+ coverage reliable through 2023; add 2024 if available

# Helper: batch a vector into chunks of size n
batch <- function(x, n) split(x, ceiling(seq_along(x) / n))

# -----------------------------------------------------------------------------
# 2. Download function with retry and rate-limit sleep
# -----------------------------------------------------------------------------

safe_ct_get <- function(reporter, partner, hs_codes, years,
                        flow = "export", sleep_sec = 3) {
  results <- list()
  year_batches <- batch(years, 5)  # 5 years per call

  for (yb in year_batches) {
    for (hs_batch in batch(hs_codes, 5)) {  # 5 HS chapters per call
      attempt <- tryCatch({
        ct_get_data(
          reporter         = reporter,
          partner          = partner,
          flow_direction   = flow,
          commodity_code   = hs_batch,
          start_date       = min(yb),
          end_date         = max(yb),
          freq             = "A",
          verbose          = FALSE
        )
      }, error = function(e) {
        cat("  ERROR:", conditionMessage(e), "\n")
        Sys.sleep(10)
        NULL
      })
      if (!is.null(attempt) && nrow(attempt) > 0) {
        results[[length(results) + 1]] <- attempt
      }
      Sys.sleep(sleep_sec)
    }
  }
  bind_rows(results)
}

# -----------------------------------------------------------------------------
# 3. Download MERCOSUR → EU exports (MERCOSUR as reporters)
#    Strategy: each MERCOSUR country reports its exports to each EU country.
#    This is the cleanest approach — avoids EU aggregate code uncertainty.
# -----------------------------------------------------------------------------

cache_file <- file.path(PROC, "comtrade_raw_cache.rds")

if (file.exists(cache_file)) {
  cat("Loading from cache:", cache_file, "\n")
  raw_data <- readRDS(cache_file)
} else {
  cat("Downloading MERCOSUR exports to EU-27 (HS 01-27 + 44,47)...\n")
  cat("Estimated calls:", length(MERCOSUR_ISO) * length(EU27_ISO) *
      ceiling(length(ALL_HS)/5) * ceiling(length(YEARS)/5), "\n")
  cat("This may take 20-40 minutes on the free tier.\n\n")

  # To reduce API load: fetch all EU countries in one call per MERCOSUR reporter
  # by passing all EU27 as partner vector (comtradr handles this).
  all_results <- list()
  for (reporter in MERCOSUR_ISO) {
    cat(sprintf("  Reporter: %s\n", reporter))
    res <- safe_ct_get(
      reporter  = reporter,
      partner   = EU27_ISO,
      hs_codes  = ALL_HS,
      years     = YEARS,
      flow      = "export",
      sleep_sec = 2
    )
    if (nrow(res) > 0) {
      all_results[[reporter]] <- res
      cat(sprintf("    Downloaded %d rows\n", nrow(res)))
    }
  }

  raw_data <- bind_rows(all_results)
  saveRDS(raw_data, cache_file)
  cat("Saved cache to:", cache_file, "\n")
}

cat("Total rows downloaded:", nrow(raw_data), "\n")
cat("Columns:", paste(names(raw_data), collapse=", "), "\n")

# -----------------------------------------------------------------------------
# 4. Clean and aggregate
# -----------------------------------------------------------------------------

# Comtrade+ column names (comtradr 1.0.x)
# Key columns: reporter_iso, partner_iso, ref_year, cmd_code, cmd_desc,
#              primary_value (USD), net_wgt (kg), qty_unit_abbr

flows <- raw_data %>%
  filter(!is.na(net_wgt), net_wgt > 0) %>%
  mutate(
    hs2    = substr(as.character(cmd_code), 1, 2),
    year   = as.integer(ref_year),
    wgt_mt = net_wgt / 1e9,      # kg → Mt
    value_musd = primary_value / 1e6  # USD → million USD
  ) %>%
  # Assign material category
  mutate(material_cat = case_when(
    hs2 %in% sprintf("%02d", 1:15)  ~ "Agricultural biomass",
    hs2 %in% c("44", "47")          ~ "Forest products",
    hs2 %in% c("25", "26")          ~ "Metal ores & minerals",
    hs2 == "27"                     ~ "Fossil fuels",
    hs2 %in% sprintf("%02d", 16:24) ~ "Processed foods/beverages",
    TRUE                            ~ "Other HS 01-27"
  ))

# Annual aggregate by MERCOSUR reporter + material category
annual_by_country <- flows %>%
  group_by(reporter_iso, year, material_cat) %>%
  summarise(
    wgt_mt     = sum(wgt_mt,     na.rm = TRUE),
    value_musd = sum(value_musd, na.rm = TRUE),
    .groups    = "drop"
  )

# Total MERCOSUR → EU annual (all countries summed)
annual_total <- flows %>%
  group_by(year, material_cat) %>%
  summarise(
    wgt_mt     = sum(wgt_mt,     na.rm = TRUE),
    value_musd = sum(value_musd, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(reporter_iso = "MERCOSUR_total")

comtrade_long <- bind_rows(annual_by_country, annual_total)
write_csv(comtrade_long, file.path(PROC, "comtrade_eu_mercosur.csv"))
cat("Saved:", file.path(PROC, "comtrade_eu_mercosur.csv"), "\n")

# Summary table for manuscript
comtrade_summary <- annual_total %>%
  group_by(year) %>%
  summarise(
    total_wgt_mt     = sum(wgt_mt,     na.rm = TRUE),
    total_value_musd = sum(value_musd, na.rm = TRUE),
    .groups          = "drop"
  )
write_csv(comtrade_summary, file.path(TABS, "comtrade_summary.csv"))

# -----------------------------------------------------------------------------
# 5. Figure 8 — EU–MERCOSUR raw material trade flows
# -----------------------------------------------------------------------------

library(ggplot2)
library(patchwork)

pal_cat <- c(
  "Agricultural biomass"     = "#4DAF4A",
  "Forest products"          = "#984EA3",
  "Metal ores & minerals"    = "#FF7F00",
  "Fossil fuels"             = "#A65628",
  "Processed foods/beverages"= "#377EB8",
  "Other HS 01-27"           = "#999999"
)

# Panel A: stacked area — total Mt by category
p_a <- annual_total %>%
  filter(material_cat != "Other HS 01-27") %>%
  ggplot(aes(year, wgt_mt, fill = material_cat)) +
  geom_area(alpha = 0.85) +
  scale_fill_manual(values = pal_cat, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  labs(
    x = NULL, y = "Net weight (Mt)",
    title = "A. MERCOSUR raw material exports to EU-27 by category"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9))

# Panel B: line — total flow and value on dual axis
p_b <- comtrade_summary %>%
  ggplot(aes(year, total_wgt_mt)) +
  geom_line(size = 1.1, colour = "#2166AC") +
  geom_point(size = 2, colour = "#2166AC") +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  labs(
    x = "Year", y = "Total net weight (Mt)",
    title = "B. Total MERCOSUR → EU raw material exports"
  ) +
  theme_bw(base_size = 11)

# Panel C: by MERCOSUR country
p_c <- annual_by_country %>%
  group_by(reporter_iso, year) %>%
  summarise(wgt_mt = sum(wgt_mt, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(year, wgt_mt, colour = reporter_iso)) +
  geom_line(size = 1) +
  scale_colour_manual(
    values = c("ARG"="#D6604D","BRA"="#F4A582","PRY"="#92C5DE","URY"="#4393C3"),
    name = NULL
  ) +
  scale_x_continuous(breaks = seq(2000, 2023, 5)) +
  labs(
    x = "Year", y = "Net weight (Mt)",
    title = "C. By country"
  ) +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

fig8 <- (p_a | p_b) / p_c +
  plot_annotation(
    title   = "Figure 8. EU–MERCOSUR bilateral raw material trade flows (HS 01–27, 44, 47)",
    caption = "Source: UN Comtrade+ (2024). Net weight in megatonnes (Mt).",
    theme   = theme(plot.title = element_text(face = "bold", size = 12))
  )

# Save
ggsave(file.path(FIGS, "Fig8_comtrade_flows.tiff"),
       fig8, width = 12, height = 8, dpi = 300, device = "tiff")
ggsave(file.path(FIGS, "Fig8_comtrade_flows.png"),
       fig8, width = 12, height = 8, dpi = 150)

cat("\n06_comtrade.R completed.\n")
cat("Outputs:\n")
cat("  data/processed/comtrade_eu_mercosur.csv\n")
cat("  output/tables/comtrade_summary.csv\n")
cat("  output/figures/Fig8_comtrade_flows.tiff/.png\n")