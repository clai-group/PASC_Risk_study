# PASC Aging Study — Replication Code

> **The age paradox in post-infectious sequelae: physiological reserve outweighs chronological age in Long COVID susceptibility**

Replication code for the primary GEE model and specification curve robustness analysis.

---

## Repository contents

| File | Description |
|------|-------------|
| `gee_analysis.R` | Primary GEE model with odds ratios and 95%  CIs |
| `specification_curve.R` | Specification curve analysis across covariate and subgroup combinations |

---

## Dependencies

### R packages

| Package | Purpose |
|---------|---------|
| `geepack` | `geeglm()` — generalized estimating equations |
| `rms` | `rcs()` — restricted cubic splines for Charlson Index |
| `specr` | `setup()`, `specr()`, `plot()` — specification curve analysis |
| `dplyr` | Data manipulation |
| `ggplot2` | Plotting (used internally by `specr`) |
| `stringr` | String filtering on subset labels |

Install all at once:

```r
install.packages(c("geepack", "rms", "specr", "dplyr", "ggplot2", "stringr"))
```

---

## Usage

### 1. GEE model (`gee_analysis.R`)

Fits a marginal logistic GEE model predicting the binary outcome with an exchangeable correlation structure, clustered by patient ID. Outputs a tidy data frame (`gee_results`) with labeled odds ratios, 95% CIs, and Wald p-values.

```r
source("gee_analysis.R")
# Returns: gee_results — OR table ready for manuscript Table
```


### 2. Specification curve (`specification_curve.R`)

Runs all combinations of the specified controls and subgroups and plots the full specification curve. Subsets with "Other" or "Unknown" race categories are excluded from the plot.

```r
source("specification_curve.R")
# Returns: results$data with OR, or.conf.low, or.conf.high appended
# Generates: specification curve plot via plot(results)
```

**Robustness dimensions explored:**

- Controls: Charlson spline, severity, vaccination status, year-quarter (all combinations)
- Subgroups: sex, race/ethnicity, age group

---

## Data

The scripts expect a single analysis-ready data frame. Column names used across both scripts:

| Variable | Description |
|----------|-------------|
| `EMPI` | Patient cluster ID |
| `age_decade` | Age in decades (continuous) |
| `sex_cd` | Sex (`"Female"`, `"Male"`) |
| `race_group` / `race` | Race/ethnicity category |
| `hispanic` | Hispanic ethnicity indicator |
| `vaccination_status_binary` / `Vaccination_status` | Vaccination status |
| `yrqt` / `yrqt_num` | Year-quarter |
| `CHARLSON_INDEX` / `charlson_spline` | Charlson Comorbidity Index |
| `severity` | Hospitalization severity |
| `age_group` | Age group category |
| `PASC.any` / `PASC` | Binary outcome |

---

## License

MIT
