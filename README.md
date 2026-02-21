# WHO Validation

This repository provides utilities that use RCPCH-modified WHO growth packages to
compute anthropometric measurements corresponding to requested z-scores.

Packages used (RCPCH forks/branches):

- `anthro` — WHO growth standards for children under 5 years (uses age in days).
- `anthroplus` — WHO growth references for children 5–19 years (uses age in months).

Both packages include RCPCH changes used here:

- `z_precision` support to control z-score decimal precision.
- `anthro_measurements` / `anthroplus_measurements` inverse-LMS functions to
  compute measurements from a requested z-score, with optional extreme-value
  correction (`correct_extreme`).

## Script helpers

The file `who-validation.R` exposes three helpers:

- `ensure_latest_packages(force = FALSE)`
  - Installs the `anthro` and `anthroplus` packages from the `z-to-measurement`
    branch on GitHub when they are missing. Pass `TRUE` to force reinstall.
- `load_random_dates(rd_path = "data-files/random_dates.csv")`
  - Reads `random_dates.csv` and returns a data.frame with computed
    `age_months` (integer) and `age_years`.
- `compute_measurements(df, measurement_method = "length", sex = NULL, requested_z = 2.5, measurement_precision = 2, correct_extreme = TRUE)`
  - Given a dataframe produced by `load_random_dates()`, computes the requested
    measurement for each row. Uses `anthro::anthro_measurements` for ages
    < 61 months (anthro expects age in days) and
    `anthroplus::anthroplus_measurements` for ages >= 61 months (expects months).
  - Returns the dataframe with a new column added:
    - `measurement_method = "length"` -> `height_in_cm`
    - `measurement_method = "weight"` -> `weight_kg`
    - `measurement_method = "bmi"` -> `bmi`
    - `measurement_method = "headc"` -> `headc`

`save_measurements_csv(df, file_path = "data-files/random_dates_with_measurements.csv", na_string = "", compress = FALSE)`
  - Writes the dataframe produced by `compute_measurements()` to CSV. If
    `compress = TRUE` the file is written gzipped and `file_path` will have
    `.gz` appended if not present.

- `compute_random_measurements(df = NULL, methods = c("length","weight","bmi","headc"), sexes = NULL, requested_z = 2.5, measurement_precision = 4, correct_extreme = TRUE, seed = NULL, special_default = FALSE)`
  - Convenience helper to generate randomized measurements for each row in a
    supplied `df` (a dataframe returned by `load_random_dates()`), or when
    `df` is omitted the function will load the default `data-files/random_dates.csv`.
  - Special default behaviour: when called with no explicit `methods`,
    `sexes` or `requested_z`, the function will attempt to allocate up to 800
    measurements (200 per method) while respecting age-restrictions:
    - `headc` only for age < 61 months
    - `weight` only for age <= 120 months
    The allocation balances sexes per-method and generates per-row
    `requested_z` values: roughly half near-zero in [-2.999, 2.999] and half
    extreme values split between (-8, -3) and (3, 8), rounded to 3 dp. Use
    `special_default = TRUE` to force this behaviour programmatically.

- `test_generated_measurements(input, save_csv = FALSE, csv_path = NULL, tol = 1e-3, verbose = TRUE)`
  - Validates a dataframe (or CSV path) produced by
    `compute_random_measurements()` using `anthro`/`anthroplus` z-score
    functions. Writes `created-csvs/validation_results.csv` on success or
    `created-csvs/validation_mismatches.csv` when mismatches are found and
    logs a success message or a warning respectively.

## Example

Run this example in an R session (after installing the modified packages or
calling `ensure_latest_packages(TRUE)`):

```r
# install or update packages if needed (uncomment to force reinstall)
# ensure_latest_packages(TRUE)

# show examples
head(result[c("start_date", "age_days", "age_months", "height_in_cm")])

# load and prepare the random dates dataset
df <- load_random_dates()

# compute heights corresponding to z = 2.5
head(result[c("start_date", "age_days", "age_months", "height_in_cm")])
```

## Notes

- The helpers call the measurement functions directly from the package namespaces
  and will warn and return NA values if the functions are not available.
- If you install or update the packages while an R session already has their
  namespaces loaded, you may need to restart the R session for the new code to
  be used. `ensure_latest_packages()` will install the packages but will not
  automatically restart R.

## A few extra things - When does WHO 2006 swap to 2007?

- Back in August 2025 the WHO removed all LMS values from the tables > 1826d (5y) [this commit](https://github.com/WorldHealthOrganization/anthro/commit/8b30b3581e93bc4efaab0ef3deaf40c1d993ea88). This now leaves a gap between the end of anthro and the start of anthroplus (which is in months and starts at 61 mths). @statist7 helpfully remembers that WHO deliberately tried to run the 2006 standard and 2007 reference close together (in [this paper](https://pubmed.ncbi.nlm.nih.gov/18026621/)) so although there are discrepancies between the two (see [this issue](https://github.com/WorldHealthOrganization/anthro/issues/64)), they are only small and it would be reasonable to interpolate between the end of 2006 and the start of 2007

In fact though I have found it easier simply to dig back in the Anthro history and replace the LMS values removed in that commit and run straight up to (but not including) 61 months in anthro, and step into anthroplus from 61 months onwards.

That is therefore what is implemented here (and i think better, since anthro is a standard, where as anthroplus is a reference).

## Data

Provide `data-files/random_dates.csv` with at least three columns: `start_date`,
`end_date`, and `age_days`. `load_random_dates()` expects this format.
