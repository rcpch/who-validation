if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")

# import anthro package from rcpch repository, z-to-measurement branch - https://github.com/rcpch/anthro
# import anthroplus package from rcxpch repository: precision branch - https://github.com/rcpch/anthroplus
# runs a check to ensure the latest version and correct branch are used
if (!requireNamespace("anthro", quietly = TRUE) ||
    !"z_precision" %in% names(formals(anthro::anthro_zscores))) {
  remotes::install_github("rcpch/anthro@z-to-measurement", dependencies = TRUE, upgrade = "never")
  message("Installed anthro from GitHub; please restart R to use the new namespace")
  quit(save = "no")  
}
if (!requireNamespace("anthro", quietly = TRUE) ||
    !"z_precision" %in% names(formals(anthroplus::anthroplus_zscores))) {
  remotes::install_github("rcpch/anthroplus@precision", dependencies = TRUE, upgrade = "never")
  message("Installed anthroplus from GitHub; please restart R to use the new namespace")
  quit(save = "no")  
}


library(anthro)
library(anthroplus)

# as an experiment, run some calculations with each

# first import the data-files/random_dates file with the headings: start_date, end_date, age_days
rd_path <- file.path("data-files", "random_dates.csv")
if (!file.exists(rd_path)) stop("Missing file: data-files/random_dates.csv")
random_dates <- read.csv(rd_path, stringsAsFactors = FALSE)
if (ncol(random_dates) < 3) stop("data-files/random_dates.csv must contain at least three columns: start_date, end_date, age_days")
names(random_dates)[1:3] <- c("start_date", "end_date", "age_days")
## compute age in months from days using 30.4375 and round to integer
random_dates$age_months <- as.integer(round(random_dates$age_days / 30.4375))
random_dates$age_years <- random_dates$age_days / 365.25




