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

message("Running reduced multivariate model comparisons ...")
reduced_models <- run_multivariate_reduced_models()
readr::write_csv(reduced_models$reduced_models, "results/tables/table_b3_to_b8_reduced_models.csv")

write_run_log("results/logs/session_info_reduced_models.txt", "scripts/reduced_models_analysis.R")

message("Finished reduced models analysis.")
