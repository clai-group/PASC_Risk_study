# PASC Aging Study

> **The age paradox in post-infectious sequelae: physiological reserve outweighs chronological age in Long COVID susceptibility**

## Repository contents

| File | Description |
|------|-------------|
| `gee_analysis.R` | Primary GEE logistic regression model (odds ratios) |
| `specification_curve.R` | Specification curve robustness analysis |
| `mediation_analysis.R` | Causal mediation decomposition of the age effect |
| `simulate_demo_data.R` | Generates a synthetic dataset for code demonstration |
| `LICENSE` | MIT License |

---

## 1. System requirements

**R version.** Tested on R 4.4.0 (recommended >= 4.1.0). No non-standard hardware required.

**Operating systems tested.** macOS 13+ (Ventura), Windows 10/11, Ubuntu 22.04 LTS.

**R package dependencies**

| Package | Version tested | Purpose |
|---------|---------------|---------|
| `geepack` | 1.3.9 | `geeglm()` generalized estimating equations |
| `rms` | 6.7.x | `rcs()` restricted cubic splines for the Charlson index |
| `mediation` | 4.5.x | `mediate()` causal mediation with bootstrap CIs |
| `specr` | 1.0.0 | Specification curve analysis |
| `dplyr` | 1.1.x | Data manipulation |
| `ggplot2` | 3.4.x | Plotting (used internally by `specr`) |
| `stringr` | 1.5.x | String filtering on subset labels |

---

## 2. Installation guide

Install R from [https://cran.r-project.org](https://cran.r-project.org). RStudio is optional.

```r
install.packages(c("geepack", "rms", "mediation", "specr", "dplyr", "ggplot2", "stringr"))
```

Typical install time: ~3-5 minutes on a standard desktop with a broadband connection.

```bash
git clone https://github.com/clai-group/PASC_Risk_study.git
cd PASC_Risk_study
```

---

## 3. Demo

No data file is shipped with this repository. `simulate_demo_data.R` generates a
synthetic dataset on demand that matches the column structure of the study. The
outcome is drawn at random (`rbinom` with fixed probability): **no real
associations are encoded, and estimates from demo data must not be interpreted.**

### Step 1 - Simulate data

Adjust the two settings at the top of `simulate_demo_data.R` if desired:

```r
n_patients <- 100   # unique patients (each contributes 2 episodes)
seed       <- 42    # change for a different random draw
```

Then run:

```r
source("simulate_demo_data.R")
```

This writes `demo_data.csv` and prints a short summary.

### Step 2 - Run the analysis scripts

```r
demo <- read.csv("demo_data.csv")

# --- Primary GEE model ---
study_data <- demo
source("gee_analysis.R")
print(gee_results)

# --- Causal mediation ---
study_data <- demo
source("mediation_analysis.R")
print(mediation_results)

# --- Specification curve ---
specification_dat <- demo
source("specification_curve.R")
plot(results)
```

### Expected output

- `gee_analysis.R` returns `gee_results`, an 11-row data frame with columns
  `group`, `OR`, `CI_lower`, `CI_upper`, `P_value` (age, sex, the five
  non-reference race/ethnicity categories, vaccination, year-quarter, and the
  two severity categories).
- `mediation_analysis.R` returns `mediation_results`, with the ACME, ADE, total
  effect, and proportion mediated for the comorbidity and severity pathways,
  overall and within the <65 and >=65 age strata.
- `specification_curve.R` produces a `specr` specification curve of the odds
  ratio for `age_decade` across covariate and subgroup combinations, with the
  "Other" and "Unknown" race/ethnicity subsamples excluded.

### Expected run time

< 10-15 minutes total on a standard desktop (the bootstrap mediation and the
specification curve are the slowest steps). Run time increases with dataset size.

---

## 4. Instructions for use

### Running on your own data

1. Prepare an analysis-ready data frame with the columns described below.
2. Assign it to `study_data` (GEE and mediation scripts) or `specification_dat`
   (specification curve script).
3. Source the relevant script.

### Column reference

| Variable | Type | Values / Notes |
|----------|------|----------------|
| `EMPI` | integer | Patient cluster ID (one patient, one or more episodes) |
| `age_decade` | numeric | Age / 10 (continuous) |
| `sex_cd` | character | `"Female"` (reference), `"Male"` |
| `race_eth_combined` | character | `"Non-Hispanic White"` (reference), `"Non-Hispanic Black"`, `"Non-Hispanic Asian"`, `"Non-Hispanic Other"`, `"Hispanic"`, `"Unknown"` |
| `vaccination_status_binary` | integer | `0` = not fully vaccinated (reference), `1` = fully vaccinated |
| `yrqt` | character | Year-quarter label (human-readable) |
| `yrqt_num` | numeric | Year-quarter numeric index used in the models |
| `CHARLSON_INDEX` | numeric | Charlson Comorbidity Index (spline applied in the model) |
| `severity` | character | `"Non-hospitalized"` (reference), `"Hospitalized"`, `"ICU/Ventilation"` |
| `age_group` | character | `"<45 years old"`, `"45-65 years old"`, `">65 years old"` (specification-curve subsamples and categorical sensitivity) |
| `PASC.any` | integer | Binary outcome: `0` = no PASC, `1` = PASC (per infection episode) |

### Reproduction of manuscript results

| Manuscript output | Script | Object / Output |
|-------------------|--------|-----------------|
| Figure 1 - primary odds ratios | `gee_analysis.R` | `gee_results` |
| Figures 3-4 - mediation decomposition | `mediation_analysis.R` | `mediation_results` |
| Specification curve | `specification_curve.R` | `plot(results)` |

---

## Cohort construction and data processing

The analysis scripts above take an analysis-ready cohort as input. The upstream
cohort derivation (loyalty-score curation for data completeness and the
validated PASC phenotyping algorithm) is described in the manuscript Methods and
eMethods and follows the algorithm of Azhir et al. Because that data-processing
step operates on individual-level electronic health records containing protected
health information, held under institutional and data-use-agreement restrictions,
the cohort-construction code and the underlying data cannot be released publicly.
The synthetic dataset provided here reproduces the analytic column structure so
that the full analytic pipeline can be executed and inspected without access to
protected data.

---

## License

MIT - see `LICENSE` for details.
