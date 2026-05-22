#!/usr/bin/env Rscript

required_packages <- c(
  "dplyr",
  "lmtest",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "tidyr",
  "TSA",
  "zoo"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R packages: ",
    paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source("R/paper_utils.R")
source("R/paper_data.R")
source("R/paper_models.R")

ensure_dir("results/tables")
ensure_dir("results/derived_data")
ensure_dir("results/logs")

message("Computing Table A1 ...")
table_a1 <- compute_table_a1()
readr::write_csv(table_a1, "results/tables/table_a1.csv")

message("Running bivariate manuscript analyses ...")
bivariate <- run_bivariate_analysis()

readr::write_csv(bivariate$table_1, "results/tables/table_1_ccf_significant_lags.csv")
readr::write_csv(bivariate$table_2, "results/tables/table_2_granger_causality.csv")
readr::write_csv(bivariate$ccf_detailed, "results/derived_data/bivariate_ccf_all_lags.csv")

write_run_log("results/logs/session_info_bivariate.txt", "scripts/bivariate_analysis.R")

message("Finished bivariate analysis.")
