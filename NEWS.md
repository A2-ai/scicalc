# scicalc 0.4.0

## New features

* Categorical mapping functions are now configurable through options:
  - `racen()` recognizes six race categories (white, black, asian, american native, pacific islander, other/multiple). Set `options(scicalc.racen_config = )` to a named numeric vector to override any category's code and to add novel categories (matched before the built-ins).
  - `agec()` and `bmic()` accept fully custom bands via `options(scicalc.agec_config = )` / `options(scicalc.bmic_config = )` — a data frame with `label`, `min` (lower bound), and `code` columns that replaces the FDA/WHO defaults. Bands are half-open `[min, next min)` with an open-ended top band.
  - Any active configuration is listed in the package startup message.
* Added `is_missing_value()`, a predicate for the missing-value sentinel (`getOption("scicalc.missing_value")`, default `-999`), e.g. `dplyr::filter(df, !is_missing_value(x))`.
* Calculation functions now handle the missing-value sentinel in their inputs: a sentinel value is treated as missing during the computation and the sentinel is preserved in the result.
* `read_file_with_hash()` and `read_hashed_file()` gained a `reader` argument for file types they don't natively support (e.g. `reader = arrow::read_feather`); `write_file_with_hash()` gained a matching `writer` argument. For a known extension the custom function is ignored unless `force = TRUE`.

## Bug fixes

* `egfr()`: pass `method` by name; positional args are `sexf, raceb, age, creat, cystc, height`. Passing a method string positionally now errors instead of silently defaulting.
* `rfc()` now honors an explicitly-supplied `absolute_units`; the estimator's `scicalc_units` attribute is used only as a fallback when `absolute_units` is not given.
* `write_csv_with_hash()` and `write_parquet_with_hash()` were filtering `...` against the wrong function's arguments (`read_csv()`/`read_parquet()` instead of `write_csv()`/`write_parquet()`), silently dropping valid writer arguments like `eol` or `compression`.

## Changes

* The unit-tracking attribute is now `scicalc_units` rather than a generic `units` attribute. Computed quantities still carry it (`egfr()`/`aegfr()` as `"mL/min/1.73m^2"` / `"mL/min"`, `bmi()` `"kg/m^2"`, `bsa()` `"m^2"`, `ibw()`/`aibw()` `"kg"`, `crcl()` `"mL/min"`, `convert_alb/bili/creat()` `"g/dL"`/`"mg/dL"`), and `rfc()` reads it to infer relative-vs-absolute when `absolute_units` is not supplied. 
## Deprecations

* The individual eGFR equation functions (`ckdepi_2009_egfr()`, `ckdepi_2021_egfr()`, `ckdepi_2021_egfr_cystatin()`, `mdrd_egfr()`, `schwartz_egfr()`) and BSA equation functions (`dubois_bsa()`, `mosteller_bsa()`) are deprecated. Use `egfr(method = ...)` and `bsa(method = ...)` instead. They will become internal in 0.6.0.
* `read_csv_with_hash()`, `read_parquet_with_hash()`, `read_sas_with_hash()`, `read_xpt_with_hash()`, `read_excel_with_hash()`, and `read_pzfx_with_hash()` now warn on every call (previously a soft deprecation). Use `read_file_with_hash()`.
* `write_csv_with_hash()` and `write_parquet_with_hash()` are deprecated. Use `write_file_with_hash()` instead. They will become internal in 0.6.0.


# scicalc 0.3.0

## New features

* Added `aegfr()` function to convert relative eGFR (mL/min/1.73m^2) to absolute eGFR (mL/min).
* Added `ibw()` (ideal body weight) and `aibw()` (adjusted ideal body weight) functions.
* Added `bmic()` (BMI category) function.
* Added `agec()` (age category) function.
* Added `cor_df()` function that takes a dataframe and a vector of column names to compute all pairwise correlations.
* Added `is_female()` and `is_black()` helper functions for converting character sex/race to logical values.
* Added unit conversion functions: `convert_creat()`, `convert_bili()`, `convert_alb()`.
* Added `round_like()` function to round numeric values based on explicit or inferred significant digits from a reference value. (Experimental, API may change.)

## Improvements

* Computed quantities now include a `units` attribute for unit tracking:
  - eGFR functions: "mL/min/1.73m^2" (relative) or "mL/min" (absolute)
  - `crcl()`: "mL/min"
  - `bsa()`, `dubois_bsa()`, `mosteller_bsa()`: "m^2"
  - `bmi()`: "kg/m^2"
  - `ibw()`, `aibw()`: "kg"
  - `convert_creat()`, `convert_bili()`: "mg/dL"
  - `convert_alb()`: "g/dL"
* Category functions now include a `category_standard` attribute:
  - `rfc()`: "FDA" or "KDIGO"
  - `hfc()`: "NCI-ODWG"
  - `bmic()`: "WHO"
  - `agec()`: "FDA"
* `rfc()` can now infer `absolute_units` from the input's `units` attribute, allowing `rfc(AEGFR)` to work directly without specifying units.
* Updated `egfr()` equation default to use CKDEPI 2021 equation.
* Default hash algorithm changed from md5 to blake3 for `read_file_with_hash()` and related functions.
* Added recycling warning to all functions when inputs have different lengths.

## Deprecations

* `read_csv_with_hash()`, `read_parquet_with_hash()`, `read_sas_with_hash()`, `read_xpt_with_hash()`, `read_excel_with_hash()`, and `read_pzfx_with_hash()` are soft-deprecated in favor of `read_file_with_hash()` which auto-detects file type.

# scicalc 0.2.1

* Fixed edge case handling for hfc when bilirubin is exactly equal to the cut-off values of 1.5 or 3 times ULN

# scicalc 0.2.0

* Added a `NEWS.md` file to track changes to the package.
