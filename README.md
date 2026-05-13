# PASC Aging Study — Replication Code

> **The age paradox in post-infectious sequelae: physiological reserve outweighs chronological age in Long COVID susceptibility**

Replication code for the primary GEE model and specification curve robustness analysis.

---

## Repository contents

| File | Description |
|------|-------------|
| `gee_analysis.R` | Primary GEE model — odds ratios with 95% Wald CIs |
| `specification_curve.R` | Specification curve robustness analysis |
| `simulate_demo_data.R` | Generates a synthetic dataset for code demonstration |
| `LICENSE` | MIT License |

---

## 1. System requirements

### R version
Tested on **R 4.4.0** (recommended ≥ 4.1.0). No non-standard hardware required.

### Operating systems tested
- macOS 13+ (Ventura)
- Windows 10/11
- Ubuntu 22.04 LTS

### R package dependencies

| Package | Version tested | Purpose |
|---------|---------------|---------|
| `geepack` | 1.3.9 | `geeglm()` — generalized estimating equations |
| `rms` | 6.7.x | `rcs()` — restricted cubic splines |
| `specr` | 1.0.0 | Specification curve analysis |
| `dplyr` | 1.1.x | Data manipulation |
| `ggplot2` | 3.4.x | Plotting (used internally by `specr`) |
| `stringr` | 1.5.x | String filtering on subset labels |

---

## 2. Installation guide

### Install R
Download and install R from [https://cran.r-project.org](https://cran.r-project.org).  
RStudio (optional but recommended): [https://posit.co/download/rstudio-desktop](https://posit.co/download/rstudio-desktop)

### Install required packages

```r
install.packages(c("geepack", "rms", "specr", "dplyr", "ggplot2", "stringr"))
```

**Typical install time:** ~2–4 minutes on a standard desktop with a broadband connection.

### Clone this repository

```bash
git clone https://github.com/<your-username>/pasc-aging-study.git
cd pasc-aging-study
```

---

## 3. Demo

No data file is shipped with this repository. Instead, `simulate_demo_data.R` generates a synthetic dataset on demand that matches the column structure of the study. The outcome variable is purely random (`rbinom` with fixed probability) — **no real associations are encoded and estimates from demo data should not be interpreted.**

### Step 1 — Simulate data

Open `simulate_demo_data.R` and adjust the two settings at the top if desired:

```r
n_patients <- 100   # number of unique patients (each contributes 2 rows)
seed       <- 42    # change for a different random draw
```

Then run:

```r
source("simulate_demo_data.R")
```

This writes `demo_data.csv` to your working directory and prints a summary:

```
Simulation complete.
  Patients : 100
  Rows     : 200
  PASC rate: ~30%
  Output   : demo_data.csv
```

### Step 2 — Run the analysis scripts

```r
demo <- read.csv("demo_data.csv")

# --- GEE model ---
study_data <- demo
source("gee_analysis.R")
print(gee_results)

# --- Specification curve ---
specification_dat <- demo
source("specification_curve.R")
plot(results)
```

### Expected output

**`gee_analysis.R`** returns `gee_results`, a 9-row data frame with columns `group`, `OR`, `CI_lower`, `CI_upper`, `P_value`.

**`specification_curve.R`** produces a `specr` specification curve plot of the OR for `age_decade` across all covariate and subgroup combinations, with "Other" and "Unknown" race subsets excluded.

### Expected run time
< 10 minutes total (simulation + both analyses) on a standard desktop. Run time may increase substantially with larger datasets, particularly for the specification curve which fits models across all covariate and subgroup combinations.

---

## 4. Instructions for use

### Running on your own data

1. Prepare an analysis-ready data frame with the columns described below.
2. Assign it to `study_data` (GEE script) or `specification_dat` (specification curve script).
3. Source the relevant script.

### Column reference

| Variable | Type | Values / Notes |
|----------|------|----------------|
| `EMPI` | integer | Patient cluster ID |
| `age_decade` | numeric | Age ÷ 10 (continuous) |
| `sex_cd` | character | `"Female"`, `"Male"` |
| `race_group` / `race` | character | `"White"`, `"Black"`, `"Asian"`, `"Other"`, `"Unknown"` |
| `hispanic` | integer | `0` = Non-Hispanic, `1` = Hispanic |
| `vaccination_status_binary` / `Vaccination_status` | integer / character | `0`/`1` or `"Unvaccinated"`/`"Vaccinated"` |
| `yrqt` / `yrqt_num` | character / numeric | Year-quarter label and numeric index |
| `CHARLSON_INDEX` / `charlson_spline` | numeric | Charlson Comorbidity Index (rcs applied in model) |
| `severity` | character | `"Non-hospitalized"`, `"Hospitalized"`, `"ICU/Ventilation"` |
| `age_group` | character | `"<45 years old"`, `"45-65 years old"`, `">65 years old"` |
| `PASC.any` / `PASC` | integer | Binary outcome: `0` = no PASC, `1` = PASC |

### Reproduction of manuscript results

| Manuscript output | Script | Object / Output |
|-------------------|--------|-----------------|
| Table — primary OR estimates | `gee_analysis.R` | `gee_results` data frame |
| Figure — specification curve | `specification_curve.R` | `plot(results)` |

> **Note:** Full reproduction requires access to the study dataset, which contains protected health information and is not publicly available. The simulation script can be used to verify that the code runs correctly and produces output in the expected format.

---


## License

MIT — see `LICENSE` for details.
