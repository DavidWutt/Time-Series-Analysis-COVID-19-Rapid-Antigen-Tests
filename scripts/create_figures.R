#!/usr/bin/env Rscript

required_packages <- c(
  "cowplot",
  "dplyr",
  "ggplot2",
  "gridExtra",
  "purrr",
  "readr",
  "stringr",
  "tibble",
  "tidyr",
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
source("R/paper_figures.R")

ensure_dir("results/figures")
ensure_dir("results/logs")

message("Rendering Fig. 1 ...")
render_figure_1()

message("Rendering Fig. A1 ...")
render_figure_a1()

write_run_log("results/logs/session_info_figures.txt", "scripts/create_figures.R")

message("Finished figure generation.")
