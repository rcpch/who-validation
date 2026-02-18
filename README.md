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

- `save_measurements_csv(df, file_path = "measurements.csv", na_string = "", compress = FALSE)`
  - Writes the dataframe produced by `compute_measurements()` to CSV. If
    `compress = TRUE` the file is written gzipped and `file_path` will have
    `.gz` appended if not present.

## Example

Run this example in an R session (after installing the modified packages or
calling `ensure_latest_packages(TRUE)`):

```r
# install or update packages if needed (uncomment to force reinstall)
# ensure_latest_packages(TRUE)

# load and prepare the random dates dataset
df <- load_random_dates()

# compute heights corresponding to z = 2.5
result <- compute_measurements(df, measurement_method = "length", requested_z = 2.5)

# show examples
head(result[c("start_date", "age_days", "age_months", "height_in_cm")])
```

## Notes

- The helpers call the measurement functions directly from the package namespaces
  and will warn and return NA values if the functions are not available.
- If you install or update the packages while an R session already has their
  namespaces loaded, you may need to restart the R session for the new code to
  be used. `ensure_latest_packages()` will install the packages but will not
  automatically restart R.

## Data

Provide `data-files/random_dates.csv` with at least three columns: `start_date`,
`end_date`, and `age_days`. `load_random_dates()` expects this format.
