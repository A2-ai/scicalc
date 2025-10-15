# scicalc 0.3.0

* Added recycling warning to all functions.
* Added `ibw` (ideal body weight) and `aibw` (adjusted ideal body weight) functions.
* Added `obesity_category` function
* Added `agec` (age category) function
* Added `cor_df` function that takes a dataframe and a vector of column names to compute all pairwise correlations.
* Updated `egfr` equation default to use CKDEPI 2021 equation.
* removed out of date pkgdown site

# scicalc 0.2.1

* Fixed edge case handling for hfc when bilirubin is exactly equal to the cut-off values of 1.5 or 3 times ULN

# scicalc 0.2.0

* Added a `NEWS.md` file to track changes to the package.
