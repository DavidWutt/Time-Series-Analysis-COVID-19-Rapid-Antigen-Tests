#!/usr/bin/env Rscript

required_packages <- c(
  "dplyr",
  "lmtest",
  "mgcv",
  "nlme",
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
ensure_dir("results/logs")

message("Running multivariate manuscript analyses ...")
full_models <- run_multivariate_full_models()
readr::write_csv(full_models$table_3, "results/tables/table_3_gamm_significant_terms.csv")
readr::write_csv(full_models$table_b2, "results/tables/table_b2_lasso_retained_lags.csv")

write_run_log("results/logs/session_info_multivariate.txt", "scripts/multivariate_analysis.R")

message("Finished multivariate analysis.")
