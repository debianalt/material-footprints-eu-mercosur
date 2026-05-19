# =============================================================================
# 06_comtrade.R  (v2 — rigorous rewrite)
# Bilateral MERCOSUR → EU raw material exports
# UN Comtrade public API v1  |  2000–2023  |  HS chapters 01–27 + 44, 47
#
# Changes from v1:
#   - normalise_one() fixes bind_rows type conflicts (reporterISO <logical> bug)
#   - Physical weight extracted from cache where isLeaf + netWgt reliable
#   - USD deflated to constant 2015 prices (US BLS CPI-U, 2015 = 100)
#   - Spearman rho correlation table (trade value vs. gap_rel, by country)
#   - Fig 8 updated to 4 panels including material export intensity overlay (D)
#
# All outputs generated from local cache — no new API calls if cache exists.
#
# Outputs:
#   data/processed/comtrade_eu_mercosur.csv  — long, value + weight + const USD
#   output/tables/comtrade_summary.csv        — annual totals (for 05_figures.R)
#   output/tables/comtrade_gap_correlation.csv — Spearman rho by country
#   output/figures/Fig8_comtrade_flows.tiff/.png — 4-panel figure
# =============================================================================

library(tidyverse)
library(httr2)
library(patchwork)

# BASE derivado de la ubicación de este script (rename-proof)
.a <- commandArgs(FALSE); .f <- sub("^--file=", "", .a[grep("^--file=", .a)])
BASE      <- if (length(.f)) normalizePath(file.path(dirname(.f), "..")) else normalizePath("..")
PROC      <- file.path(BASE, "data/processed")
TABS      <- file.path(BASE, "output/tables")
FIGS      <- file.path(BASE, "output/figures")
source(file.path(BASE, "R", "_paper_theme.R"))   # theme_paper(), LBL, WIDTH_IN, save_fig()
CACHE_DIR <- file.path(PROC, "comtrade_cache")
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. Country codes (M49 numeric, as required by Comtrade API)
# =============================================================================

MERCOSUR_M49 <- "32,76,600,858"

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

HS_CODES   <- c(sprintf("%02d", 1:27), "44", "47")
YEARS      <- 2000:2023
PUBLIC_URL <- "https://comtradeapi.un.org/public/v1/preview/C/A/HS"

# =============================================================================
# 2. US BLS CPI-U deflator (annual average, base 2015 = 100)
#    Source: U.S. Bureau of Labor Statistics, series CUUR0000SA0
# =============================================================================

cpi_tbl <- tibble(
  year     = 2000:2023,
  cpi_2015 = c(
    72.6,   # 2000
    74.7,   # 2001
    75.9,   # 2002
    77.6,   # 2003
    79.7,   # 2004
    82.4,   # 2005
    85.0,   # 2006
    87.5,   # 2007
    90.8,   # 2008
    90.5,   # 2009
    92.0,   # 2010
    94.9,   # 2011
    96.8,   # 2012
    98.3,   # 2013
    99.9,   # 2014
   100.0,   # 2015
   101.3,   # 2016
   103.4,   # 2017
   105.9,   # 2018
   107.9,   # 2019
   109.2,   # 2020
   114.3,   # 2021
   123.4,   # 2022
   128.5    # 2023
  )
)

# =============================================================================
# 3. Material category helper
# =============================================================================

classify_material <- function(hs2) {
  case_when(
    hs2 %in% sprintf("%02d", 1:15) ~ "Agricultural biomass",
    hs2 %in% c("44", "47")         ~ "Forest products",
    hs2 %in% c("25", "26")         ~ "Metal ores & minerals",
    hs2 == "27"                    ~ "Fossil fuels",
    hs2 %in% sprintf("%02d", 16:24)~ "Processed foods/beverages",
    TRUE                           ~ "Other HS 01-27"
  )
}

# =============================================================================
# 4. Column normalisation — fixes bind_rows type conflicts
#
# The Comtrade public API returns reporterISO/partnerISO as NA (populated
# only for keyed subscribers). When all values are NA, readRDS stores the
# column as <logical>; when some records have character values it stores as
# <character>. The same inconsistency affects isLeaf and isNetWgtEstimated
# across cache files. normalise_one() coerces every tibble to a fixed schema
# before bind_rows(), eliminating all type conflicts.
# =============================================================================

normalise_one <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  get_col <- function(df, col, type_fn, default) {
    if (col %in% names(df)) type_fn(df[[col]]) else rep(default, nrow(df))
  }

  tibble(
    reporterCode      = get_col(df, "reporterCode",      \(x) suppressWarnings(as.integer(x)), NA_integer_),
    cmdCode           = get_col(df, "cmdCode",            as.character,                          NA_character_),
    refYear           = get_col(df, "refYear",            \(x) suppressWarnings(as.integer(x)), NA_integer_),
    primaryValue      = get_col(df, "primaryValue",       \(x) suppressWarnings(as.numeric(x)),  NA_real_),
    netWgt            = get_col(df, "netWgt",             \(x) suppressWarnings(as.numeric(x)),  NA_real_),
    isNetWgtEstimated = get_col(df, "isNetWgtEstimated",  \(x) suppressWarnings(as.logical(x)),  NA),
    isLeaf            = get_col(df, "isLeaf",             \(x) suppressWarnings(as.logical(x)),  NA)
  )
}

# =============================================================================
# 5. Single-call fetch with per-call caching
# =============================================================================

fetch_one <- function(year, hs_code, sleep_sec = 3) {
  cache_path <- file.path(CACHE_DIR, sprintf("%d_%s.rds", year, hs_code))

  if (file.exists(cache_path)) {
    return(normalise_one(readRDS(cache_path)))
  }

  resp <- tryCatch(
    request(PUBLIC_URL) |>
      req_url_query(
        reporterCode = MERCOSUR_M49,
        partnerCode  = EU27_M49,
        period       = as.character(year),
        cmdCode      = hs_code,
        flowCode     = "X"
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

  raw_result <- if (!is.null(resp) && !is.null(resp$data) && length(resp$data) > 0)
    as_tibble(resp$data)
  else
    NULL

  saveRDS(raw_result, cache_path)   # save raw for future re-processing
  normalise_one(raw_result)         # return normalised version
}

# =============================================================================
# 6. Download / cache-read loop
# =============================================================================

total_calls <- length(YEARS) * length(HS_CODES)
cat(sprintf(
  "Loading %d years x %d HS codes = %d calls (from cache where available)\n",
  length(YEARS), length(HS_CODES), total_calls
))

all_results <- vector("list", total_calls)
n_call <- 0L

for (yr in YEARS) {
  for (hs in HS_CODES) {
    n_call <- n_call + 1L
    if (n_call == 1L || n_call %% 100L == 0L)
      cat(sprintf("  [%d/%d] year=%d hs=%s\n", n_call, total_calls, yr, hs))
    res <- fetch_one(yr, hs)
    if (!is.null(res) && nrow(res) > 0)
      all_results[[n_call]] <- res
  }
}

all_results <- Filter(Negate(is.null), all_results)
raw_data    <- bind_rows(all_results)   # all tibbles now have identical schema
cat(sprintf("Total rows loaded: %d\n", nrow(raw_data)))

# =============================================================================
# 7. Build value flows (with deflation to constant 2015 USD)
# =============================================================================

flows <- raw_data |>
  filter(!is.na(primaryValue), primaryValue > 0) |>
  mutate(
    reporter_iso3 = m49_to_iso3[as.character(reporterCode)],
    hs2           = substr(as.character(cmdCode), 1, 2),
    year          = as.integer(refYear),
    value_musd    = primaryValue / 1e6,
    material_cat  = classify_material(hs2)
  ) |>
  filter(!is.na(reporter_iso3)) |>
  left_join(cpi_tbl, by = "year") |>
  mutate(value_const_musd = value_musd / (cpi_2015 / 100)) |>
  select(reporter_iso3, hs2, year, material_cat, value_musd, value_const_musd,
         netWgt, isNetWgtEstimated, isLeaf)

cat(sprintf("Value rows after filter: %d\n", nrow(flows)))
cat("Reporters:", paste(sort(unique(flows$reporter_iso3)), collapse = ", "), "\n")

# =============================================================================
# 8. Physical weight — extracted where isLeaf TRUE and netWgt reliable
#
# isLeaf == TRUE identifies commodity-level (not chapter-level) records.
# isNetWgtEstimated == FALSE means the weight was reported, not imputed.
# This typically covers agricultural commodities and bulk minerals well.
# =============================================================================

weight_flows <- flows |>
  filter(
    isLeaf == TRUE,
    isNetWgtEstimated == FALSE,
    !is.na(netWgt), netWgt > 0
  ) |>
  mutate(weight_kt = netWgt / 1e6)

cat(sprintf("Weight-reliable rows: %d / %d (%.1f%% of value rows)\n",
    nrow(weight_flows), nrow(flows),
    100 * nrow(weight_flows) / nrow(flows)))

weight_by_country <- weight_flows |>
  group_by(reporter_iso3, year, material_cat) |>
  summarise(weight_kt = sum(weight_kt, na.rm = TRUE), .groups = "drop")

# =============================================================================
# 9. Annual aggregates by country and MERCOSUR total
# =============================================================================

annual_by_country <- flows |>
  group_by(reporter_iso3, year, material_cat) |>
  summarise(
    value_musd       = sum(value_musd,       na.rm = TRUE),
    value_const_musd = sum(value_const_musd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(weight_by_country, by = c("reporter_iso3", "year", "material_cat"))

annual_total <- flows |>
  group_by(year, material_cat) |>
  summarise(
    value_musd       = sum(value_musd,       na.rm = TRUE),
    value_const_musd = sum(value_const_musd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    weight_flows |>
      group_by(year, material_cat) |>
      summarise(weight_kt = sum(weight_kt, na.rm = TRUE), .groups = "drop"),
    by = c("year", "material_cat")
  ) |>
  mutate(reporter_iso3 = "MERCOSUR_total")

comtrade_long <- bind_rows(annual_by_country, annual_total)
write_csv(comtrade_long, file.path(PROC, "comtrade_eu_mercosur.csv"))
cat("Saved comtrade_eu_mercosur.csv:", nrow(comtrade_long), "rows\n")

# Weight coverage (% of total value covered by reliable-weight records)
coverage_check <- comtrade_long |>
  filter(reporter_iso3 == "MERCOSUR_total") |>
  group_by(year) |>
  summarise(
    value_total  = sum(value_const_musd, na.rm = TRUE),
    value_wt_cov = sum(if_else(!is.na(weight_kt), value_const_musd, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(coverage_pct = 100 * value_wt_cov / value_total)

cat(sprintf("Mean annual weight coverage: %.1f%% of total value\n",
    mean(coverage_check$coverage_pct, na.rm = TRUE)))

# Summary for 05_figures.R backward compatibility
comtrade_summary <- annual_total |>
  group_by(year) |>
  summarise(
    total_value_musd       = sum(value_musd,       na.rm = TRUE),
    total_value_const_musd = sum(value_const_musd, na.rm = TRUE),
    .groups = "drop"
  )
write_csv(comtrade_summary, file.path(TABS, "comtrade_summary.csv"))
cat("Saved comtrade_summary.csv\n")

# =============================================================================
# 10. Correlation analysis: bilateral trade flows vs. material externalization gap
#
# For each MERCOSUR country: Spearman rho between annual export value
# (constant USD) and gap_rel, in both levels and first differences.
# First differences reduce spurious correlation from shared trends.
# =============================================================================

panel_main <- read_csv(file.path(PROC, "panel_main.csv"), show_col_types = FALSE)

trade_annual <- comtrade_long |>
  filter(!reporter_iso3 %in% "MERCOSUR_total") |>
  group_by(iso3 = reporter_iso3, year) |>
  summarise(
    trade_musd       = sum(value_musd,       na.rm = TRUE),
    trade_const_musd = sum(value_const_musd, na.rm = TRUE),
    .groups = "drop"
  )

corr_panel <- panel_main |>
  filter(bloc == "MERCOSUR") |>
  select(iso3, year, gap_rel) |>
  inner_join(trade_annual, by = c("iso3", "year")) |>
  group_by(iso3) |>
  arrange(year) |>
  mutate(
    d_gap_rel    = gap_rel - lag(gap_rel),
    d_log_trade  = log(trade_const_musd / lag(trade_const_musd))
  ) |>
  ungroup() |>
  filter(!is.na(d_gap_rel), !is.na(d_log_trade), is.finite(d_log_trade))

# Spearman rho by country
corr_by_country <- corr_panel |>
  group_by(iso3) |>
  summarise(
    n         = n(),
    rho_level = cor(trade_const_musd, gap_rel,  method = "spearman", use = "complete.obs"),
    rho_diff  = cor(d_log_trade, d_gap_rel,     method = "spearman", use = "complete.obs"),
    p_level   = tryCatch(cor.test(trade_const_musd, gap_rel,
                  method = "spearman", exact = FALSE)$p.value, error = \(e) NA_real_),
    p_diff    = tryCatch(cor.test(d_log_trade,  d_gap_rel,
                  method = "spearman", exact = FALSE)$p.value, error = \(e) NA_real_),
    .groups = "drop"
  )

# Pooled MERCOSUR
pool_lev  <- cor.test(corr_panel$trade_const_musd, corr_panel$gap_rel,
                       method = "spearman", exact = FALSE)
pool_diff <- cor.test(corr_panel$d_log_trade, corr_panel$d_gap_rel,
                       method = "spearman", exact = FALSE)

corr_table <- bind_rows(
  corr_by_country,
  tibble(
    iso3      = "MERCOSUR_pooled",
    n         = nrow(corr_panel),
    rho_level = as.numeric(pool_lev$estimate),
    rho_diff  = as.numeric(pool_diff$estimate),
    p_level   = pool_lev$p.value,
    p_diff    = pool_diff$p.value
  )
)

write_csv(corr_table, file.path(TABS, "comtrade_gap_correlation.csv"))
cat("\nCorrelation table (trade flows vs. gap_rel):\n")
print(corr_table, digits = 3)

# =============================================================================
# 11. Figure 8 — 4-panel: commodity composition + trends + country + overlay
# =============================================================================

pal_cat <- c(
  "Agricultural biomass"      = "#4DAF4A",
  "Forest products"           = "#984EA3",
  "Metal ores & minerals"     = "#FF7F00",
  "Fossil fuels"              = "#A65628",
  "Processed foods/beverages" = "#377EB8",
  "Other HS 01-27"            = "#999999"
)

pal_country <- c(ARG = "#D6604D", BRA = "#F4A582", PRY = "#92C5DE", URY = "#4393C3")

recession_bands <- tibble(
  xmin = c(2008, 2020) - 0.4,
  xmax = c(2009, 2020) + 0.4
)

# Panel A: stacked area by category (constant 2015 USD)
p_a <- annual_total |>
  filter(material_cat != "Other HS 01-27") |>
  ggplot(aes(year, value_const_musd / 1000, fill = material_cat)) +
  geom_rect(
    data = recession_bands, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    alpha = 0.12, fill = "grey40"
  ) +
  geom_area(alpha = 0.88, colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = pal_cat, name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 5)) +
  scale_y_continuous(labels = scales::comma,
                     name   = "Export value (billion 2015 USD)") +
  labs(x = NULL, title = "A. By commodity category") +
  theme_paper()

# Panel B: total constant USD trend with ribbon
p_b <- comtrade_summary |>
  ggplot(aes(year, total_value_const_musd / 1000)) +
  geom_rect(
    data = recession_bands, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    alpha = 0.12, fill = "grey40"
  ) +
  geom_ribbon(aes(ymin = 0, ymax = total_value_const_musd / 1000),
              fill = "#2166AC", alpha = 0.20) +
  geom_line(linewidth = 1.1, colour = "#2166AC") +
  geom_point(size = 2.2, colour = "#2166AC") +
  scale_x_continuous(breaks = seq(2000, 2022, 5)) +
  scale_y_continuous(labels = scales::comma,
                     name   = "Total (billion 2015 USD)") +
  labs(x = NULL, title = "B. Total MERCOSUR exports to EU-27") +
  theme_paper()

# Panel C: by exporting country (constant USD)
p_c <- annual_by_country |>
  group_by(reporter_iso3, year) |>
  summarise(value_const_musd = sum(value_const_musd, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(year, value_const_musd / 1000, colour = reporter_iso3,
             shape = reporter_iso3)) +
  geom_rect(
    data = recession_bands, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    alpha = 0.12, fill = "grey40", colour = NA
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.0) +
  scale_colour_manual(values = pal_country, name = NULL) +
  scale_shape_manual( values = c(ARG = 16, BRA = 17, PRY = 15, URY = 18), name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2022, 5)) +
  scale_y_continuous(labels = scales::comma,
                     name   = "Export value (billion 2015 USD)") +
  labs(x = NULL, title = "C. By exporting country") +
  theme_paper()

# Panel D: export volume (bars) vs. material export intensity |gap_rel| (line)
gap_mercosur <- panel_main |>
  filter(bloc == "MERCOSUR", year >= 2000, year <= 2023) |>
  group_by(year) |>
  summarise(
    mean_abs_gap = mean(abs(gap_rel), na.rm = TRUE),   # use |gap_rel| — both series increase
    .groups = "drop"
  )

overlay_data <- comtrade_summary |>
  left_join(gap_mercosur, by = "year")

sf <- max(overlay_data$total_value_const_musd / 1000, na.rm = TRUE) /
      (max(overlay_data$mean_abs_gap, na.rm = TRUE) * 1.15)

p_d <- overlay_data |>
  ggplot(aes(year)) +
  geom_rect(
    data = recession_bands, inherit.aes = FALSE,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    alpha = 0.12, fill = "grey40"
  ) +
  geom_col(aes(y = total_value_const_musd / 1000),
           fill = "#2166AC", alpha = 0.35, width = 0.75) +
  geom_line(aes(y = mean_abs_gap * sf),
            colour = "#D6604D", linewidth = 1.25) +
  geom_point(aes(y = mean_abs_gap * sf),
             colour = "#D6604D", size = 2.2) +
  scale_x_continuous(breaks = seq(2000, 2022, 5)) +
  scale_y_continuous(
    name     = "Exports to EU-27 (billion 2015 USD)",
    labels   = scales::comma,
    sec.axis = sec_axis(
      transform = ~ . / sf,
      name      = "Mean |gap_rel|",
      labels    = scales::number_format(accuracy = 0.01)
    )
  ) +
  labs(x = "Year",
       title = "D. Export volume vs. material export intensity") +
  theme_paper() +
  theme(
    axis.title.y.right = element_text(colour = "#D6604D", size = 11),
    axis.text.y.right  = element_text(colour = "#D6604D", size = 9.5)
  )

fig8 <- (p_a | p_b) / (p_c | p_d) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        legend.box      = "vertical",
        legend.margin   = margin(2, 2, 2, 2),
        plot.margin     = margin(6, 18, 6, 6))

# Título descriptivo (sin prefijo "Figure 8." ni caption embebido: el caption
# completo con fuentes/métodos vive en la sección "Figure captions" del
# manuscrito, según convención Elsevier).
fig8 <- fig8 +
  plot_annotation(
    title = "MERCOSUR primary commodity exports to EU-27, 2000–2023",
    theme = theme(
      plot.title = element_text(face = "bold", size = 13, family = "sans")
    )
  )

# Ancho único 190mm (= todas las figuras); alto 9.5in para los 4 paneles
save_fig(fig8, "Fig8_comtrade_flows", h = 6.8)
cat("Saved Fig8_comtrade_flows.tiff/.png\n")

cat("\n06_comtrade.R completed.\n")
