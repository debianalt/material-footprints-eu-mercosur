# Replication Archive: The Relative Material Externalisation Gap — EU and MERCOSUR (1994–2024)

**Paper:** "The relative material externalisation gap: a consumption-based indicator of offshored material burdens in resource-efficiency monitoring, with evidence from the EU and MERCOSUR (1994–2024)"  
**Journal:** *Journal of Cleaner Production* (revised version, September 2026)  
**Authors:** Raimundo Elías Gómez, María Gabriela Miño  
**Affiliation:** Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET) / Facultad de Humanidades y Ciencias Sociales, Universidad Nacional de Misiones (UNaM), Argentina  

---

## What changed in the revision (v1.4.0)

- **Indicator interpretation.** gap_rel = (MF − DMC)/MF = RTB/MF − PTB/MF: the share of the material footprint that DMC does not record (net upstream extraction embodied in trade). The companion ratio RTB/MF = 1 − DE/MF (net imported extraction) is now reported (`07_decomposition_global.R`).
- **Small-cluster inference** for the four MERCOSUR clusters: CR2 + Satterthwaite, wild cluster restricted bootstrap (Webb weights, B = 9,999), exact randomisation inference over all C(31,4) = 31,465 assignments; robustness magnitudes with CR2 intervals (`08_small_cluster_inference.R`).
- **HMM model selection** by leave-one-country-out cross-validation: K = 2 is selected and is now the main specification; K = 3 is a sensitivity analysis (`09_hmm_cv.R`).
- **Global replication** on 158 countries (with alternative groupings, unwinsorised outcomes and a DMC placebo), Tapio-tolerance robustness, country descriptives and mechanism descriptives (`07_decomposition_global.R`, `11_revision_r3.R`).
- **Mechanical component** (second revision): DMC/MF multiplier medians; the gap change had DMC stayed at its previous-year level, with the DMC response as remainder; and the exact log decomposition 100·Δlog(MF/DMC) = 100·ΔlogMF − 100·ΔlogDMC, for the two-bloc panel (CR2) and the 158-country panel (`13_revision_r3_round2.R`). In percentage points the bloc contrast lies in the counterfactual; net of the multiplier, the DMC term accounts for about half to two-thirds of the global contrast (p = 0.07–0.11) and is not detectable in the two-bloc panel.
- **Tapio nomenclature** in the recessive branch corrected to Tapio (2005): ε < 1 − τ is *weak negative decoupling* (WND) and ε > 1 + τ is *recessive decoupling* (RD). The partition is unchanged, so no estimate changed.

---

## Contents

```
replication_repo/
├── README.md
├── R/
│   ├── 00_setup.R                   ← packages, run order
│   ├── 01_data_prep.R               ← GMFD + World Bank GDP → panel_main.csv
│   ├── 02_tapio.R                   ← Tapio (2005) classification
│   ├── 03_hmm.R                     ← pooled HMMs, K = 2–5, robustness HMMs
│   ├── 04_gap_regression.R          ← fixed-effects regressions (fixest), original robustness checks
│   ├── 05_figures.R                 ← original figures
│   ├── 06_comtrade.R                ← MERCOSUR→EU bilateral flows (UN Comtrade public API v1)
│   ├── 07_decomposition_global.R    ← RTB/PTB decomposition, MERCOSUR RME exports, 158-country panel
│   ├── 08_small_cluster_inference.R ← CR2, wild bootstrap, randomisation inference, magnitudes
│   ├── 09_hmm_cv.R                  ← leave-one-country-out cross-validation of K
│   ├── 10_revision_figures.R        ← Fig. 5 (CR2), Figs. S4–S5
│   ├── 11_revision_r3.R             ← τ robustness, country descriptives, mechanism descriptives
│   ├── 12_audit_checks.R            ← DMC placebo, reference/outlier checks, country-label permutation tests, K=2 bootstrap
│   ├── 13_revision_r3_round2.R      ← mechanical component: DMC/MF multiplier, DMC-held-fixed counterfactual, log decomposition (two-bloc and global)
│   ├── _inference_helpers.R         ← M2 design matrix + CR2 fit (used by 08 and 11)
│   └── _paper_theme.R               ← shared ggplot theme and export
├── data/processed/                  ← derived panels (see below)
├── output/tables/                   ← all CSV tables
├── output/figures/                  ← all figures as PNG (500 dpi TIFFs not included)
└── LICENSE
```

**Raw data.** The raw GMFD file (`data/raw/mfa_filled.csv`, 2024 edition, accessed 23 April 2026) cannot be redistributed; download it from resourcepanel.org. Scripts 01 and 07 need it. All other scripts run from the derived files in `data/processed/` (07 writes `global_reg_panel.csv`, which 13 reads; it is included).

**Reproducing the published numbers.** `01_data_prep.R` downloads World Bank GDP through the `WDI` package; later World Bank revisions can change GDP and hence Tapio states. To reproduce the paper exactly, start from the provided `data/processed/panel_main.csv` and run scripts 02 onwards.

---

## How to reproduce

Requirements: R ≥ 4.3; run `R/00_setup.R` to install `tidyverse`, `depmixS4`, `fixest`, `clubSandwich`, `patchwork`, `ggrepel`, `WDI`, `countrycode` and related packages.

```r
source("R/00_setup.R")
source("R/01_data_prep.R")               # optional: rebuilds panel_main.csv (see note above)
source("R/02_tapio.R")
source("R/03_hmm.R")                     # ~10 min (30 restarts); models cached in data/processed/*.rds
source("R/04_gap_regression.R")
source("R/05_figures.R")
source("R/06_comtrade.R")                # ~40 min without cache
source("R/07_decomposition_global.R")    # needs data/raw/mfa_filled.csv
source("R/08_small_cluster_inference.R") # ~5 min
source("R/09_hmm_cv.R")                  # ~15 min; CV_REUSE=1 re-summarises saved folds
source("R/10_revision_figures.R")
source("R/11_revision_r3.R")
source("R/12_audit_checks.R")          # ~5 min (31,465 permutations)
source("R/13_revision_r3_round2.R")
```

---

## Main output tables (revision)

| File | Contents (paper item) |
|---|---|
| `regression_table2_cr2.csv` | M1–M3 with CR2 standard errors (Table 2) |
| `regression_small_cluster_inference.csv` | CR2, wild bootstrap p-values (Table S11) |
| `regression_randomization_inference.csv` | Exact randomisation inference (Table S11 note) |
| `regression_robustness_magnitudes.csv` | Magnitudes and CR2 intervals across specifications (Table S10) |
| `regression_tau_robustness.csv` | Tapio tolerance τ = 0.1, 0.2, 0.3 (Table S10) |
| `gap_decomposition_country.csv`, `gap_decomposition_bloc.csv` | gap_rel vs RTB/MF and PTB/MF (Table S9) |
| `mercosur_rme_exports.csv` | MERCOSUR raw-material-equivalent exports (Section 3.6) |
| `global_m2_diagnostic.csv`, `global_gap_by_income_2021.csv` | 158-country replication (Table S12) |
| `hmm_cv_selection.csv` | Cross-validated log-likelihood, K = 2–5 (Table S1) |
| `hmm_k2_robustness.csv` | K = 2 regime shares by bloc (Table S7; main regime model) |
| `hmm_regime_summary.csv` | K = 3 regime shares (Table S13; sensitivity) |
| `gap_descriptives_country.csv` | Country descriptive statistics (Table S14) |
| `mechanism_sd_years_growth.csv` | Growth rates in strong-decoupling years (Table S15) |
| `comtrade_gap_correlation.csv` | Export–gap Spearman correlations (Table S16) |
| `regression_audit_checks.csv` | DMC mirror check, all-other-states reference, outlier and winsorised checks (Table S10) |
| `mechanical_bound.csv`, `mechanical_bound_reverse_order.csv`, `dmc_mf_multiplier.csv`, `mechanical_scenario_medians.csv` | Mechanical counterfactual and DMC response, multiplier medians, median scenario (Table S10, Section 4.4) |
| `log_decomposition.csv` | Exact log decomposition, two-bloc panel (Table S10) |
| `global_mechanical_bound.csv` | Counterfactual and log decompositions, 158 countries (Table S12) |
| `mercosur_state_counts.csv` | MERCOSUR strong-decoupling and expansive-coupling years by country (Table S10 note) |
| `bloc_permutation_tests.csv` | Country-label permutation tests for bloc differences (Sections 3.3, 3.5) |
| `hmm_k2_bootstrap.csv` | K = 2 bloc shares with within-bloc country bootstrap, B = 2,000 (Table S7) |

## Figures (file → paper)

| File | Paper |
|---|---|
| `Fig6_gap_trajectories.png` | Fig. 1 |
| `Fig7_typology_scatter.png` | Fig. 2 |
| `Fig1_tapio_distribution.png` | Fig. 3 |
| `Fig2_rolling_shares.png` | Fig. 4 |
| `Fig5_rev_regression_coefs.png` | Fig. 5 |
| `Fig9_comtrade_flows.png` | Fig. 6 |
| `Fig3_hmm_model_selection.png` | Fig. S1 |
| `Fig4_transition_heatmap.png` | Fig. S2 |
| `Fig10_S_robustness_hmm.png` | Fig. S3 |
| `FigS5_global_gap.png` | Fig. S4 |
| `FigS4_robustness_forest.png` | Fig. S5 |
| `Fig5_regime_shares.png` | Fig. S6 |

`Fig8_regression_coefs.png` is the original-submission version of Fig. 5 (conventional clustered intervals), kept for reference.

---

## Data sources

| Dataset | Source | Access |
|---|---|---|
| Global Material Flows Database (GMFD) | UNEP IRP, 2024 edition | https://www.resourcepanel.org/global-material-flows-database |
| World Bank GDP (NY.GDP.MKTP.KD) | World Development Indicators | `WDI::WDI()` R package |
| UN Comtrade bilateral flows | UN Statistics Division | https://comtradeapi.un.org/public/v1 (free, no key required) |

## License

Code: MIT License. Data (processed panels) and figures: CC BY 4.0. If using this archive, please cite the published paper.
