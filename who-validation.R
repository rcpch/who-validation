if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# Helper: install latest anthro/anthroplus from their branches when requested.
ensure_latest_packages <- function(force = FALSE) {
  # force: if TRUE, always (re)install. If FALSE, install only when package missing.
  need_anthro <- force || !requireNamespace("anthro", quietly = TRUE)
  need_anthroplus <- force || !requireNamespace("anthroplus", quietly = TRUE)

  if (need_anthro) {
    message("Installing anthro from branch 'z-to-measurement'...")
    remotes::install_github("rcpch/anthro@z-to-measurement", dependencies = TRUE, upgrade = "never")
  }
  if (need_anthroplus) {
    message("Installing anthroplus from branch 'z-to-measurement'...")
    remotes::install_github("rcpch/anthroplus@z-to-measurement", dependencies = TRUE, upgrade = "never")
  }
  invisible(TRUE)
}

## first import helpers to load and prepare the random_dates dataset
load_random_dates <- function(rd_path = file.path("data-files", "random_dates.csv")) {
  if (!file.exists(rd_path)) stop("Missing file: data-files/random_dates.csv")
  df <- read.csv(rd_path, stringsAsFactors = FALSE)
  if (ncol(df) < 3) stop("data-files/random_dates.csv must contain at least three columns: start_date, end_date, age_days")
  names(df)[1:3] <- c("start_date", "end_date", "age_days")
  ## compute age in months from days using 30.4375 and round to integer
  df$age_months <- as.integer(round(df$age_days / 30.4375))
  df$age_years <- df$age_days / 365.25
  df
}

## compute measurements given a prepared dataframe from `load_random_dates()`
## measurement_method: one of "length","weight","bmi","headc"
compute_measurements <- function(df, measurement_method = "length", sex = NULL, requested_z = 2.5, measurement_precision = 2, correct_extreme = TRUE) {
  stopifnot(is.data.frame(df))
  if (!"age_months" %in% names(df)) stop("Dataframe must contain 'age_months' computed by load_random_dates()")

  # allow overriding sex column; otherwise use df$sex if present or default to 1
  if (!is.null(sex)) {
    sex_vec <- rep_len(sex, nrow(df))
  } else if ("sex" %in% names(df)) {
    sex_vec <- df$sex
  } else {
    sex_vec <- rep(1L, nrow(df))
  }

  out_col <- switch(measurement_method,
                    length = "height_in_cm",
                    weight = "weight_kg",
                    bmi = "bmi",
                    headc = "headc",
                    stop("Unsupported measurement_method"))

  height_in_cm <- rep(NA_real_, nrow(df))

  # anthroplus for age >= 61 months (expects months)
  idx_plus <- which(df$age_months >= 61)
  if (length(idx_plus) > 0) {
    nplus <- length(idx_plus)
    res_plus <- tryCatch({
      fn <- get("anthroplus_measurements", envir = asNamespace("anthroplus"))
      fn(
        sex = sex_vec[idx_plus],
        age_in_months = df$age_months[idx_plus],
        requested_z = requested_z,
        measurement_method = measurement_method,
        measurement_precision = measurement_precision,
        correct_extreme = correct_extreme
      )
    }, error = function(e) {
      warning("anthroplus_measurements unavailable: ", conditionMessage(e))
      # create NA data.frame with appropriate column name
      data.frame(tmp = rep(NA_real_, nplus))
    })
    # pick the first numeric column returned
    if (is.data.frame(res_plus) && ncol(res_plus) >= 1) {
      height_in_cm[idx_plus] <- as.numeric(res_plus[[1]])
    }
  }

  # anthro for age < 61 months (expects age in days)
  idx_anthro <- which(df$age_months < 61)
  if (length(idx_anthro) > 0) {
    nanth <- length(idx_anthro)
    res_anthro <- tryCatch({
      fn <- get("anthro_measurements", envir = asNamespace("anthro"))
      fn(
        sex = sex_vec[idx_anthro],
        age = df$age_days[idx_anthro],
        is_age_in_month = FALSE,
        requested_z = requested_z,
        measurement_method = measurement_method,
        measurement_precision = measurement_precision,
        correct_extreme = correct_extreme
      )
    }, error = function(e) {
      warning("anthro_measurements unavailable: ", conditionMessage(e))
      data.frame(tmp = rep(NA_real_, nanth))
    })
    if (is.data.frame(res_anthro) && ncol(res_anthro) >= 1) {
      height_in_cm[idx_anthro] <- as.numeric(res_anthro[[1]])
    }
  }

  df[[out_col]] <- height_in_cm
  df[['requested_z']] <- requested_z
  df
}


#' Save a measurements dataframe to CSV
#'
#' @param df A data.frame produced by `compute_measurements()`
#' @param file_path Path to write the CSV to
#' @param na_string String to use for NA values in CSV (default empty string)
#' @param compress If TRUE write a gzipped CSV (appends .gz if not present)
#' @return Invisibly returns the path written
save_measurements_csv <- function(df, file_path = file.path("created-csvs", "measurements.csv"), na_string = "", compress = FALSE) {
  stopifnot(is.data.frame(df))

  # ensure output directory exists
  out_dir <- dirname(file_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if (compress) {
    if (!grepl("\\.gz$", file_path)) file_path <- paste0(file_path, ".gz")
    con <- gzfile(file_path, "w")
    on.exit(close(con), add = TRUE)
    write.csv(df, con, row.names = FALSE, na = na_string)
  } else {
    write.csv(df, file_path, row.names = FALSE, na = na_string)
  }
  invisible(file_path)
}


##' Compute randomised measurements per-row
##'
#' For each row in `df`, randomly select a `measurement_method` and `sex` from
#' the provided sets and compute the requested z-score measurement. Returns the
#' input dataframe augmented with:
#' - `measurement_value`: numeric computed measurement
#' - `measurement_method`: method used for that row
#' - `sex_used`: sex value used for that row
#'
compute_random_measurements <- function(df,
                                        methods = c("length", "weight", "bmi", "headc"),
                                        sexes = NULL,
                                        requested_z = 2.5,
                                        measurement_precision = 2,
                                        correct_extreme = TRUE,
                                        seed = NULL) {
  stopifnot(is.data.frame(df))
  if (!"age_months" %in% names(df)) stop("Dataframe must contain 'age_months' computed by load_random_dates()")

  n <- nrow(df)
  if (is.null(sexes)) {
    if ("sex" %in% names(df)) sexes <- unique(na.omit(df$sex)) else sexes <- c(1L,2L)
  }

  # validate methods
  allowed_methods <- c("length","weight","bmi","headc")
  if (!all(methods %in% allowed_methods)) stop("methods must be a subset of: ", paste(allowed_methods, collapse=", "))

  if (!is.null(seed)) set.seed(seed)

  sampled_methods <- sample(methods, n, replace = TRUE)
  sampled_sexes <- sample(sexes, n, replace = TRUE)

  measurement_value <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    one_row <- df[i, , drop = FALSE]
    method_i <- sampled_methods[i]
    sex_i <- sampled_sexes[i]
    # compute single-row measurement
    res <- tryCatch({
      compute_measurements(one_row,
                           measurement_method = method_i,
                           sex = sex_i,
                           requested_z = requested_z,
                           measurement_precision = measurement_precision,
                           correct_extreme = correct_extreme)
    }, error = function(e) {
      warning("compute_measurements failed for row ", i, ": ", conditionMessage(e))
      NULL
    })

    if (is.data.frame(res) && nrow(res) >= 1) {
      # prefer the method-specific column name, fallback to first numeric
      out_name <- switch(method_i,
                         length = "height_in_cm",
                         weight = "weight_kg",
                         bmi = "bmi",
                         headc = "headc",
                         NULL)
      if (!is.null(out_name) && out_name %in% names(res)) {
        measurement_value[i] <- as.numeric(res[[out_name]])[1]
      } else {
        num_cols <- vapply(res, is.numeric, FALSE)
        if (any(num_cols)) {
          measurement_value[i] <- as.numeric(res[[ which(num_cols)[1] ]])[1]
        } else {
          measurement_value[i] <- NA_real_
        }
      }
    } else {
      measurement_value[i] <- NA_real_
    }
  }

  df$measurement_value <- measurement_value
  df$measurement_method <- sampled_methods
  df$sex_used <- sampled_sexes
  df$requested_z <- rep_len(requested_z, n)
  df$measurement_precision <- rep_len(as.integer(measurement_precision), n)
  df$correct_extreme <- rep_len(as.logical(correct_extreme), n)

  df
}





