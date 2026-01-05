# scicalc 0.3.0

## New features

* Added `aegfr()` function to convert relative eGFR (mL/min/1.73m^2) to absolute eGFR (mL/min).
* Added `ibw()` (ideal body weight) and `aibw()` (adjusted ideal body weight) functions.
* Added `bmic()` (BMI category) function.
* Added `agec()` (age category) function.
* Added `cor_df()` function that takes a dataframe and a vector of column names to compute all pairwise correlations.
* Added `is_female()` and `is_black()` helper functions for converting character sex/race to logical values.
* Added unit conversion functions: `convert_weight()`, `convert_height()`, `convert_creat()`, `convert_bili()`.

## Improvements

* eGFR functions now set a `units` attribute on results ("mL/min/1.73m^2" for relative, "mL/min" for absolute).
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
