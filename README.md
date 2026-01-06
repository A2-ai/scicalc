
<!-- README.md is generated from README.Rmd. Please edit that file -->

# scicalc <a href="https://a2-ai.github.io/scicalc/"><img src="man/figures/logo.png" align="right" height="139" alt="scicalc website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/a2-ai/scicalc/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/a2-ai/scicalc/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

scicalc provides common calculations for PK/PD dataset preparation and
analysis, including renal function, body measurements, organ function
classification, and reproducible data I/O with hash verification.

## Installation

You can install the development version of `scicalc` from
[GitHub](https://github.com) with:

``` r
pak::pkg_install("a2-ai/scicalc")
```

## Features

### Renal Function

- **eGFR calculation** with multiple methods: CKD-EPI 2009, CKD-EPI
  2021, CKD-EPI 2021 with cystatin C, MDRD, and Schwartz (pediatric)
- **Absolute eGFR** conversion from relative (mL/min/1.73m²) to absolute
  (mL/min)
- **Creatinine clearance** using Cockcroft-Gault equation
- **Renal function classification** supporting both regulatory (FDA) and
  clinical (KDIGO) categories

### Body Measurements

- **BMI** calculation and categorization (underweight, normal,
  overweight, obese classes)
- **Body surface area** using DuBois or Mosteller methods
- **Ideal body weight** and adjusted ideal body weight

### Organ Function Classification

- **Hepatic function classification** using NCI-ODWG criteria (bilirubin
  and AST-based)
- **Renal function classification** with regulatory and clinical
  standards

### Age Classification

- Age categories: neonate, infant, child, adolescent, adult, elderly

### Data Helpers

- **Demographic conversions**: `is_female()`, `is_black()`,
  `is_asian()`, etc. for use in clinical equations
- **Unit conversions**: creatinine (mg/dL to µmol/L), bilirubin, albumin
- **Geometric statistics**: geometric mean, SD, and CV

### Reproducible Data I/O

- **Read/write with hash verification**: Automatically compute and
  display file hashes when reading or writing data
- **Multiple formats**: CSV, Parquet, SAS (sas7bdat), Excel, XPT, PZFX
- **Hash validation**: Read files only if they match an expected hash

## Quick Example

``` r
library(scicalc)
library(dplyr)

# Sample patient data
patients <- data.frame(
  ID = 1:4,
  SEX = c("Female", "Male", "Female", "Male"),
  RACE = c("White", "Black", "Asian", "White"),
  AGE = c(45, 62, 38, 71),
  WEIGHT = c(68, 85, 55, 78),
  HEIGHT = c(165, 178, 158, 172),
  CREAT = c(0.9, 1.4, 0.7, 1.8)
)

# Calculate derived values
patients <- patients %>%
  mutate(
    BSA = bsa(WEIGHT, HEIGHT),
    EGFR = egfr(is_female(SEX), is_black(RACE), AGE, CREAT),
    AEGFR = aegfr(EGFR, BSA),
    RFC = rfc(AEGFR)
  )

patients
#>   ID    SEX  RACE AGE WEIGHT HEIGHT CREAT      BSA      EGFR     AEGFR RFC
#> 1  1 Female White  45     68    165   0.9 1.749277  80.34339  81.23862   2
#> 2  2   Male Black  62     85    178   1.4 2.032001  56.82811  66.74842   2
#> 3  3 Female Asian  38     55    158   0.7 1.548984 113.45681 101.58541   1
#> 4  4   Male White  71     78    172   1.8 1.911017  39.74485  43.90352   3
```

## Documentation

See the [package website](https://a2-ai.github.io/scicalc/) for full
documentation.
