# Replication Archive: The Relative Material Externalisation Gap — EU and MERCOSUR (1994–2024)

**Paper:** "The relative material externalisation gap: a consumption-based indicator of offshored material burdens in resource-efficiency monitoring, with evidence from the EU and MERCOSUR (1994–2024)"  
**Journal:** *Journal of Cleaner Production* (under review)  
**Authors:** Raimundo Elías Gómez, María Gabriela Miño  
**Affiliation:** Consejo Nacional de Investigaciones Científicas y Técnicas (CONICET) / Facultad de Humanidades y Ciencias Sociales, Universidad Nacional de Misiones (UNaM), Argentina  

---

## Contents

```
replication_repo/
├── README.md               ← this file
├── R/
│   ├── 00_setup.R          ← install packages, define paths
│   ├── 01_data_prep.R      ← download GMFD + World Bank GDP; build panel
│   ├── 02_tapio.R          ← Tapio (2005) taxonomy, descriptive analysis
│   ├── 03_hmm.R            ← HMM K selection (K=2–5) + K=3 main + K=2 robustness
│   ├── 04_gap_regression.R ← fixed-effects panel regression (fixest)
│   ├── 05_figures.R        ← all figures (ggplot2 + patchwork)
│   └── 06_comtrade.R       ← Fig9_comtrade_flows (paper Fig. 7): MERCOSUR→EU bilateral flows (httr2 + public API v1)
├── data/
│   └── processed/
│       ├── panel_main.csv      ← 31 countries × 1994–2024; MF, DMC, GDP, gap_rel
│       ├── panel_tapio.csv     ← panel_main + Tapio state classifications
│       └── panel_regimes.csv   ← panel_tapio + Viterbi regime assignments
├── output/
│   ├── tables/             ← all CSV tables (see list below)
│   └── figures/            ← all figures as PNG (500 dpi TIFFs not included due to size)
└── LICENSE                 ← CC BY 4.0
```

**Note:** Raw GMFD data cannot be redistributed (see license at resourcepanel.org). The `data/processed/` files are derived and provided for convenience. To reproduce from scratch, run the scripts in order starting from `01_data_prep.R`, which downloads GMFD manually or via the URL in the script header, and World Bank GDP via the `WDI` R package.

---

## How to reproduce

### Requirements

- R ≥ 4.3.0
- Run `R/00_setup.R` first to install all required packages:
  - `tidyverse`, `depmixS4`, `fixest`, `ggplot2`, `patchwork`, `ggrepel`, `WDI`

### Steps

```r
source("R/00_setup.R")          # install packages, set BASE path
source("R/01_data_prep.R")      # builds data/processed/panel_main.csv
source("R/02_tapio.R")          # Tapio classification + tables
source("R/03_hmm.R")            # HMM models; slow (~10 min with 30 restarts)
source("R/04_gap_regression.R") # regression tables
source("R/05_figures.R")        # all figures except Fig9_comtrade_flows
source("R/06_comtrade.R")       # Fig9_comtrade_flows (paper Fig. 7; ~40 min without cache)
```

`06_comtrade.R` uses the UN Comtrade public API v1 (no key required). It loops 24 years × 29 HS chapters = 696 API calls and caches each response in `data/processed/comtrade_cache/`. Interrupted runs resume from cache automatically.

The HMM models are cached in `data/processed/hmm_models_pooled.rds` after first run. Delete this file to re-estimate from scratch.

---

## Output tables

| File | Contents |
|---|---|
| `tapio_state_distribution.csv` | Tapio state frequency by bloc (n, %, CI) |
| `tapio_country_summary.csv` | Country-level % SD+WD, median gap_rel, median elasticity |
| `tapio_rolling_shares.csv` | Rolling 5-yr SD/END shares by bloc and year |
| `hmm_model_selection.csv` | K=2–5 AIC/BIC/ICL/logLik (Table S1 in paper) |
| `hmm_k2_robustness.csv` | K=2 regime shares by bloc — K-invariance check (Table S7) |
| `hmm_emission_matrix.csv` | P(Tapio state \| regime) for K=3 pooled model |
| `hmm_transition_matrix.csv` | Transition probabilities for K=3 pooled model |
| `hmm_regime_summary.csv` | Regime shares + bootstrap CIs by bloc (Table 3) |
| `hmm_robustness_summary.csv` | EU-only, MERCOSUR-only, EU-15 HMM summaries |
| `regression_main.txt` | Formatted regression output (M1, M2, M3) |
| `regression_coefficients.csv` | Tidy coefficient table for M1 and M2 |
| `regression_robustness.csv` | EU-only and MERCOSUR-only sub-sample regressions |
| `regression_gap_denominator_robustness.csv` | SD effect under DMC-normalised and absolute-Mt gap (Table S6) |
| `fig7_quadrant_thresholds.csv` | Sample median thresholds used in Fig. 7 |

---

## Output figures

| File | Figure in paper |
|---|---|
| `Fig1_tapio_distribution.png` | Fig. 3: Tapio state distribution by bloc |
| `Fig2_rolling_shares.png` | Fig. 4: Rolling 5-yr SD/END shares |
| `Fig3_hmm_model_selection.png` | Fig. S1: HMM model selection (AIC/BIC/ICL) |
| `Fig4_transition_heatmap.png` | Fig. S2: Regime transition probability matrix |
| `Fig5_regime_shares.png` | Fig. 6: Regime time shares with CIs |
| `Fig6_gap_trajectories.png` | Fig. 1: Externalisation gap trajectories |
| `Fig7_typology_scatter.png` | Fig. 2: Country typology scatter (4 quadrants) |
| `Fig8_regression_coefs.png` | Fig. 5: Regression coefficient plot |
| `Fig9_comtrade_flows.png` | Fig. 7: MERCOSUR→EU-27 raw material exports |
| `Fig10_S_robustness_hmm.png` | Fig. S3: Robustness HMMs comparison |

Figures carry no embedded titles (Elsevier convention: the caption, including the title, lives in the manuscript). File names keep the original script numbering; the right column gives the figure number in the submitted paper.

---

## Data sources

| Dataset | Source | Access |
|---|---|---|
| Global Material Flows Database (GMFD) | UNEP IRP, 2024 edition | https://www.resourcepanel.org/global-material-flows-database |
| World Bank GDP (NY.GDP.MKTP.KD) | World Development Indicators | `WDI::WDI()` R package |
| UN Comtrade bilateral flows | UN Statistics Division | https://comtradeapi.un.org/public/v1 (free, no key required) |

---

## License

Code: MIT License  
Data (processed panel): CC BY 4.0  
Figures: CC BY 4.0  

If using this archive, please cite the published paper.
