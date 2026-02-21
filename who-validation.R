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
  ## compute age in months from days using 30.4375
  ## keep decimal months (do NOT round) so anthroplus can interpolate between months
  df$age_months <- df$age_days / 30.4375
  df$age_years <- df$age_days / 365.25
  df
}


  #' Validate generated measurements against anthro/anthroplus
  #'
  #' Reads a dataframe (or CSV path) produced by `compute_random_measurements`
  #' and verifies that the stored `observation_value` corresponds to the stored
  #' `requested_z` using `anthro` (age in days, under 61 months) or
  #' `anthroplus` (age in months, 61+ months). Returns a summary list and
  #' a data.frame with per-row results (recomputed_z, match logical, reason).
  test_generated_measurements <- function(input, save_csv = FALSE, csv_path = NULL, tol = 1e-3, verbose = TRUE) {
    # input: data.frame or path to CSV
    if (is.character(input) && length(input) == 1 && file.exists(input)) {
      df <- read.csv(input, stringsAsFactors = FALSE)
    } else if (is.data.frame(input)) {
      df <- input
    } else {
      stop("input must be a data.frame or path to an existing CSV")
    }

    if (save_csv) {
      if (is.null(csv_path)) csv_path <- file.path("created-csvs", "measurements_for_validation.csv")
      save_measurements_csv(df, csv_path)
      if (verbose) message("Wrote CSV for validation: ", csv_path)
    }

    if (!requireNamespace("anthro", quietly = TRUE) || !requireNamespace("anthroplus", quietly = TRUE)) {
      stop("Both 'anthro' and 'anthroplus' packages are required for validation")
    }

    safe_first_match <- function(df, candidates) {
      if (is.null(df)) return(NULL)
      cn <- colnames(df)
      for (c in candidates) if (c %in% cn) return(c)
      return(NULL)
    }

    n <- nrow(df)
    results <- data.frame(index = seq_len(n), recomputed_z = NA_real_, recomputed_source = NA_character_, match = NA, reason = NA_character_, stringsAsFactors = FALSE)

    for (i in seq_len(n)) {
      mth <- df$measurement_method[i]
      val <- as.numeric(df$observation_value[i])
      rz <- as.numeric(df$requested_z[i])
      age_m <- df$age_months[i]
      age_days <- df$age_days[i]
      sex <- if (!is.null(df$sex)) df$sex[i] else NA

      if (is.na(mth) || is.na(val) || is.na(rz) || is.na(sex)) {
        results$reason[i] <- "insufficient data"
        next
      }

      # choose package based on age
      if (!is.na(age_m) && age_m >= 61) {
        # anthroplus expects age_in_months
        res <- tryCatch({
          anthroplus::anthroplus_zscores(sex = rep(as.integer(sex), 1), age_in_months = age_m, height_in_cm = if (mth %in% c("length","bmi")) NA_real_ else NA_real_, weight_in_kg = NA_real_)
          NULL
        }, error = function(e) NULL)
        # We'll compute per-method by calling anthroplus_zscores for the single-row with appropriate argument(s)
        res <- tryCatch({
          if (mth == "length") {
            anthroplus::anthroplus_zscores(sex = rep(as.integer(sex),1), age_in_months = age_m, height_in_cm = val)
          } else if (mth == "weight") {
            anthroplus::anthroplus_zscores(sex = rep(as.integer(sex),1), age_in_months = age_m, weight_in_kg = val)
          } else if (mth == "headc") {
            # anthroplus does not provide OFC for over-5; mark unable
            res <- NULL
          } else if (mth == "bmi") {
            # anthroplus_zscores may provide bmi z when both height and weight provided;
            # we cannot compute BMI z from BMI alone here — fallback to measurement re-computation check below
            res <- NULL
          } else res <- NULL
          res
        }, error = function(e) NULL)

        if (!is.null(res) && is.data.frame(res)) {
          rdf <- as.data.frame(res)
          candidates <- switch(mth,
                               length = c("zlen","zlenh","zlen_cm","zhfa","zlen"),
                               weight = c("zwei","zwei_kg","zwa","zweight","zewi","zwei"),
                               headc = c("zhc","zhc_","zhc_cm","zofc","zofc_cm","zheadc"),
                               bmi = c("zbmi","zbmi_z","zbmi_kgm2","zbmia"),
                               character(0))
          zcol <- safe_first_match(rdf, candidates)
          if (!is.null(zcol)) {
            zval <- as.numeric(rdf[[zcol]])
            results$recomputed_z[i] <- round(zval, 3)
            results$recomputed_source[i] <- "anthroplus"
            results$match[i] <- abs(results$recomputed_z[i] - round(rz,3)) <= tol
            results$reason[i] <- if (results$match[i]) "ok" else "mismatch"
            next
          }
        }
      } else {
        # under 61 months: anthro (age in days)
        res <- tryCatch({
          if (mth == "length") {
            anthro::anthro_zscores(sex = rep(as.integer(sex),1), age = age_days, lenhei = val)
          } else if (mth == "weight") {
            anthro::anthro_zscores(sex = rep(as.integer(sex),1), age = age_days, weight = val)
          } else if (mth == "headc") {
            anthro::anthro_zscores(sex = rep(as.integer(sex),1), age = age_days, headc = val)
          } else if (mth == "bmi") {
            # anthro may not accept bmi directly; attempt but may fail
            res <- tryCatch(anthro::anthro_zscores(sex = rep(as.integer(sex),1), age = age_days, weight = NA_real_, lenhei = NA_real_, headc = NA_real_), error = function(e) NULL)
            res <- NULL
          } else res <- NULL
          res
        }, error = function(e) NULL)

        if (!is.null(res) && is.data.frame(res)) {
          rdf <- as.data.frame(res)
          candidates <- switch(mth,
                               length = c("zlen","zlenh","zlen_cm","zhfa","zlen"),
                               weight = c("zwei","zwei_kg","zwa","zweight","zewi","zwei"),
                               headc = c("zhc","zhc_","zhc_cm","zofc","zofc_cm","zheadc"),
                               bmi = c("zbmi","zbmi_z","zbmi_kgm2","zbmia"),
                               character(0))
          zcol <- safe_first_match(rdf, candidates)
          if (!is.null(zcol)) {
            zval <- as.numeric(rdf[[zcol]])
            results$recomputed_z[i] <- round(zval, 3)
            results$recomputed_source[i] <- "anthro"
            results$match[i] <- abs(results$recomputed_z[i] - round(rz,3)) <= tol
            results$reason[i] <- if (results$match[i]) "ok" else "mismatch"
            next
          }
        }
      }

      # If we reach here, direct z recomputation was not possible (e.g., BMI or OFC over-5)
      # Fallback: recompute measurement from requested_z using anthro/anthroplus and compare values
      fallback_ok <- FALSE
      if (!is.na(age_m) && age_m >= 61) {
        # anthroplus_measurements
        resm <- tryCatch({
          fn <- get("anthroplus_measurements", envir = asNamespace("anthroplus"))
          fn(sex = as.integer(sex), age_in_months = age_m, requested_z = rz, measurement_method = mth, measurement_precision = max(8L, as.integer(6)), correct_extreme = TRUE)
        }, error = function(e) NULL)
        if (is.data.frame(resm) && ncol(resm) >= 1) {
          meas <- as.numeric(resm[[1]])
          tol_val <- 1e-3
          if (!is.na(meas) && abs(meas - val) <= tol_val) {
            fallback_ok <- TRUE
            results$recomputed_z[i] <- round(rz,3)
            results$recomputed_source[i] <- "anthroplus(measure)"
            results$match[i] <- TRUE
            results$reason[i] <- "fallback-match"
          } else {
            results$reason[i] <- "fallback-mismatch"
          }
        } else {
          results$reason[i] <- "fallback-unavailable"
        }
      } else {
        # anthro_measurements for under-61
        resm <- tryCatch({
          fn <- get("anthro_measurements", envir = asNamespace("anthro"))
          fn(sex = as.integer(sex), age = as.integer(age_days), is_age_in_month = FALSE, requested_z = rz, measurement_method = mth, measurement_precision = max(8L, as.integer(6)), correct_extreme = TRUE)
        }, error = function(e) NULL)
        if (is.data.frame(resm) && ncol(resm) >= 1) {
          meas <- as.numeric(resm[[1]])
          tol_val <- 1e-3
          if (!is.na(meas) && abs(meas - val) <= tol_val) {
            fallback_ok <- TRUE
            results$recomputed_z[i] <- round(rz,3)
            results$recomputed_source[i] <- "anthro(measure)"
            results$match[i] <- TRUE
            results$reason[i] <- "fallback-match"
          } else {
            results$reason[i] <- "fallback-mismatch"
          }
        } else {
          results$reason[i] <- "fallback-unavailable"
        }
      }
    }

    summary <- list(total = n, checked = sum(!is.na(results$match)), matched = sum(results$match, na.rm = TRUE), mismatched = sum(!results$match & !is.na(results$match)), unable = sum(is.na(results$match)))
    if (verbose) {
      message(sprintf("Validation: checked=%d matched=%d mismatched=%d unable=%d", summary$checked, summary$matched, summary$mismatched, summary$unable))
      if (summary$mismatched > 0) {
        warning(sprintf("Validation failed: %d mismatched out of %d checked (unable=%d)", summary$mismatched, summary$checked, summary$unable))
      } else if (summary$checked > 0) {
        message(sprintf("Validation success: all %d checked rows matched (unable=%d)", summary$checked, summary$unable))
      } else {
        message(sprintf("Validation: no rows checked (unable=%d)", summary$unable))
      }
    }

    out_dir <- "created-csvs"
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

    if (summary$mismatched > 0) {
      # write only the failing rows
      out_path <- file.path(out_dir, "validation_mismatches.csv")
      fails <- results[!is.na(results$match) & results$match == FALSE, , drop = FALSE]
      write.csv(fails, out_path, row.names = FALSE, na = "")
      if (verbose) {
        warning(sprintf("Validation failed: %d mismatched out of %d checked (unable=%d). Wrote mismatches to: %s", summary$mismatched, summary$checked, summary$unable, out_path))
      }
    } else {
      # success — write full results for record
      out_path <- file.path(out_dir, "validation_results.csv")
      write.csv(results, out_path, row.names = FALSE, na = "")
      if (verbose) message(sprintf("Validation success: all %d checked rows matched (unable=%d). Wrote results to: %s", summary$checked, summary$unable, out_path))
    }

    list(summary = summary, results = results)
  }

## compute measurements given a prepared dataframe from `load_random_dates()`
## measurement_method: one of "length","weight","bmi","headc"
compute_measurements <- function(df, measurement_method = "length", sex = NULL, requested_z = 2.5, measurement_precision = 4, correct_extreme = TRUE) {
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
  # raw (high-precision) values to allow exact SDS re-computation
  height_in_cm_raw <- rep(NA_real_, nrow(df))

  # determine allowed rows for the requested measurement based on age
  # head circumference only for < 61 months, weight only for <= 120 months
  if (measurement_method %in% c("headc", "headcirc")) {
    allowed_rows <- which(df$age_months < 61)
  } else if (measurement_method %in% c("weight", "wei")) {
    allowed_rows <- which(!is.na(df$age_months) & df$age_months <= 120)
  } else {
    allowed_rows <- seq_len(nrow(df))
  }

  # anthroplus for age >= 61 months (expects decimal months and will interpolate between monthly LMS)
  idx_plus <- intersect(which(df$age_months >= 61), allowed_rows)
  if (length(idx_plus) > 0) {
    nplus <- length(idx_plus)
    res_plus <- tryCatch({
      fn <- get("anthroplus_measurements", envir = asNamespace("anthroplus"))
        fn(
          sex = sex_vec[idx_plus],
          age_in_months = df$age_months[idx_plus], # decimal months - anthroplus will interpolate
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
      # store rounded value
      height_in_cm[idx_plus] <- as.numeric(res_plus[[1]])
      # also compute and store a high-precision raw value
      res_plus_raw <- tryCatch({
        fn(
          sex = sex_vec[idx_plus],
          age_in_months = df$age_months[idx_plus],
          requested_z = requested_z,
          measurement_method = measurement_method,
          measurement_precision = max(8L, as.integer(measurement_precision) + 4L),
          correct_extreme = correct_extreme
        )
      }, error = function(e) {
        warning("anthroplus_measurements raw unavailable: ", conditionMessage(e))
        data.frame(tmp = rep(NA_real_, length(idx_plus)))
      })
      if (is.data.frame(res_plus_raw) && ncol(res_plus_raw) >= 1) {
        height_in_cm_raw[idx_plus] <- as.numeric(res_plus_raw[[1]])
      }
    }
  }

  # anthro for age < 61 months (expects age in days) and allowed by method
  idx_anthro <- intersect(which(df$age_months < 61), allowed_rows)
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
      # compute high-precision raw values from anthro
      res_anthro_raw <- tryCatch({
        fn(
          sex = sex_vec[idx_anthro],
          age = df$age_days[idx_anthro],
          is_age_in_month = FALSE,
          requested_z = requested_z,
          measurement_method = measurement_method,
          measurement_precision = max(8L, as.integer(measurement_precision) + 4L),
          correct_extreme = correct_extreme
        )
      }, error = function(e) {
        warning("anthro_measurements raw unavailable: ", conditionMessage(e))
        data.frame(tmp = rep(NA_real_, length(idx_anthro)))
      })
      if (is.data.frame(res_anthro_raw) && ncol(res_anthro_raw) >= 1) {
        height_in_cm_raw[idx_anthro] <- as.numeric(res_anthro_raw[[1]])
      }
    }
  }

  df[[out_col]] <- height_in_cm
  # expose raw unrounded observation when available
  df[['observation_value_raw']] <- height_in_cm_raw
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
#' - `observation_value`: numeric computed measurement
#' - `measurement_method`: method used for that row
#' - `sex`: sex value used for that row
#'
compute_random_measurements <- function(df = NULL,
                                        methods = c("length", "weight", "bmi", "headc"),
                                        sexes = NULL,
                                        requested_z = 2.5,
                                        measurement_precision = 4,
                                        correct_extreme = TRUE,
                                        seed = NULL,
                                        special_default = FALSE) {
  # If no dataframe provided, load the default random_dates CSV
  if (is.null(df)) {
    df <- load_random_dates()
  }
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

  sampled_methods <- rep(NA_character_, n)
  sampled_sexes <- rep(NA_integer_, n)
  requested_z_vec <- rep(NA_real_, n)

  # Special default behaviour when caller provided only `df` (no explicit
  # `methods`, `sexes` or `requested_z`). Produce up to 800 measurements
  # divided 200 per method (headc/weight restricted by age), balanced by sex
  # and with requested_z values per the requested distribution.
  special_mode <- special_default || (missing(methods) && missing(sexes) && missing(requested_z))
  if (special_mode) {
    methods_all <- c("length","weight","bmi","headc")
    # target total: up to 800 or n (whichever smaller)
    desired_total <- min(800L, n)
    per_method_target <- floor(desired_total / length(methods_all))

    pool <- seq_len(n)

    # helper to compute eligible indices for a method based on age rules
    allowed_indices_for_method <- function(mth) {
      if (mth %in% c("headc", "headcirc")) {
        which(df$age_months < 61)
      } else if (mth %in% c("weight", "wei")) {
        which(!is.na(df$age_months) & df$age_months <= 120)
      } else {
        seq_len(n)
      }
    }

    # allocate restrictive methods first to maximise usage
    methods_order <- c("headc","weight","bmi","length")
    allocated <- 0L
    counts_by_method <- setNames(integer(length(methods_all)), methods_all)

    for (mth in methods_order) {
      eligible <- intersect(pool, allowed_indices_for_method(mth))
      if (length(eligible) == 0) next
      take_n <- min(length(eligible), per_method_target)
      if (take_n > 0) {
        chosen <- sample(eligible, take_n)
        sampled_methods[chosen] <- mth
        pool <- setdiff(pool, chosen)
        counts_by_method[mth] <- counts_by_method[mth] + length(chosen)
        allocated <- allocated + length(chosen)
      }
    }

    # fill remaining desired slots (if any) taking from pool where possible
    while (allocated < desired_total && length(pool) > 0) {
      progressed <- FALSE
      for (mth in methods_order) {
        if (allocated >= desired_total || length(pool) == 0) break
        eligible <- intersect(pool, allowed_indices_for_method(mth))
        if (length(eligible) == 0) next
        chosen <- sample(eligible, 1)
        sampled_methods[chosen] <- mth
        pool <- setdiff(pool, chosen)
        counts_by_method[mth] <- counts_by_method[mth] + 1L
        allocated <- allocated + 1L
        progressed <- TRUE
      }
      if (!progressed) break
    }

    # For each method, assign sexes balanced as possible and produce requested_z
    for (mth in methods_all) {
      inds <- which(!is.na(sampled_methods) & sampled_methods == mth)
      m <- length(inds)
      if (m == 0) next
      # ensure sexes set: default to 1,2
      sex_levels <- if (!is.null(sexes)) sexes else c(1L,2L)
      k <- length(sex_levels)
      base_per <- floor(m / k)
      per_sex_counts <- rep(base_per, k)
      rem <- m - sum(per_sex_counts)
      if (rem > 0) for (i in seq_len(rem)) per_sex_counts[i] <- per_sex_counts[i] + 1L

      # build sex assignment vector and shuffle to distribute across inds
      sex_assign <- unlist(mapply(function(s, cnt) rep(s, cnt), sex_levels, per_sex_counts, SIMPLIFY = FALSE))
      if (length(sex_assign) > 1) sex_assign <- sample(sex_assign, length(sex_assign))
      sampled_sexes[inds] <- sex_assign

      # create requested_z values per sex subgroup
      pos <- 1L
      for (si in seq_along(sex_levels)) {
        cnt <- per_sex_counts[si]
        if (cnt == 0) next
        subgroup_inds <- inds[which(sampled_sexes[inds] == sex_levels[si])]
        s <- length(subgroup_inds)
        near_n <- floor(s / 2)
        ext_n <- s - near_n

        # near-zero SDS between -2.999 and +2.999
        near_vals <- if (near_n > 0) round(runif(near_n, min = -2.999, max = 2.999), 3) else numeric(0)

        # extreme SDS: split between negative and positive extremes
        neg_n <- floor(ext_n / 2)
        pos_n <- ext_n - neg_n
        neg_vals <- if (neg_n > 0) round(runif(neg_n, min = -7.999, max = -3.0001), 3) else numeric(0)
        pos_vals <- if (pos_n > 0) round(runif(pos_n, min = 3.0001, max = 7.999), 3) else numeric(0)

        ext_vals <- c(neg_vals, pos_vals)
        vals <- c(near_vals, ext_vals)
        if (length(vals) > 1) vals <- sample(vals, length(vals))
        # assign into requested_z_vec for subgroup indices
        if (length(vals) > 0) requested_z_vec[subgroup_inds] <- vals[seq_len(length(subgroup_inds))]
      }
    }
    # any remaining unassigned sexes for rows left as NA in sampled_sexes will remain NA
  } else {
    # fallback to previous random sampling behaviour when user has supplied parameters
    sampled_methods <- character(n)
    sampled_sexes <- sample(sexes, n, replace = TRUE)
    requested_z_vec <- rep_len(requested_z, n)
  }

  # choose a method per row from those allowed by age (only in non-special mode)
  if (!special_mode) {
  for (i in seq_len(n)) {
    age_i <- df$age_months[i]
    if (is.na(age_i)) {
      allowed_by_age <- methods
    } else if (age_i < 61) {
      allowed_by_age <- intersect(methods, c("length", "weight", "bmi", "headc"))
    } else if (age_i <= 120) {
      allowed_by_age <- intersect(methods, c("length", "weight", "bmi"))
    } else {
      allowed_by_age <- intersect(methods, c("length", "bmi"))
    }
    if (length(allowed_by_age) == 0) {
      sampled_methods[i] <- NA_character_
    } else {
      sampled_methods[i] <- sample(allowed_by_age, 1)
    }
  }
  }

  observation_value <- rep(NA_real_, n)
  observation_value_raw <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    one_row <- df[i, , drop = FALSE]
    method_i <- sampled_methods[i]
    sex_i <- sampled_sexes[i]
    # skip rows that were not allocated a method in the special default mode
    if (is.na(method_i) || is.na(sex_i)) {
      res <- NULL
    } else {
      # compute single-row measurement using per-row requested_z when present
      rz <- if (!is.na(requested_z_vec[i])) requested_z_vec[i] else requested_z
      res <- tryCatch({
        compute_measurements(one_row,
                             measurement_method = method_i,
                             sex = sex_i,
                             requested_z = rz,
                             measurement_precision = measurement_precision,
                             correct_extreme = correct_extreme)
      }, error = function(e) {
        warning("compute_measurements failed for row ", i, ": ", conditionMessage(e))
        NULL
      })
    }

    if (is.data.frame(res) && nrow(res) >= 1) {
      # prefer the method-specific column name, fallback to first numeric
      out_name <- switch(method_i,
                         length = "height_in_cm",
                         weight = "weight_kg",
                         bmi = "bmi",
                         headc = "headc",
                         NULL)
        if (!is.null(out_name) && out_name %in% names(res)) {
          observation_value[i] <- as.numeric(res[[out_name]])[1]
        }
        # capture high-precision raw value if returned by compute_measurements
        if ("observation_value_raw" %in% names(res)) {
          observation_value_raw[i] <- as.numeric(res[["observation_value_raw"]])[1]
      } else {
          num_cols <- vapply(res, is.numeric, FALSE)
          if (any(num_cols)) {
            # if out_name wasn't present, fall back to first numeric column for observation_value
            if (is.na(observation_value[i])) observation_value[i] <- as.numeric(res[[ which(num_cols)[1] ]])[1]
          } else {
            if (is.na(observation_value[i])) observation_value[i] <- NA_real_
          }
      }
    } else {
      observation_value[i] <- NA_real_
    }
  }

  df$observation_value <- observation_value
  # expose raw observation values when available
  df$observation_value_raw <- observation_value_raw
  df$measurement_method <- sampled_methods
  df$sex <- sampled_sexes
  df$requested_z <- requested_z_vec
  df$measurement_precision <- rep_len(as.integer(measurement_precision), n)
  df$correct_extreme <- rep_len(as.logical(correct_extreme), n)

  df
}





