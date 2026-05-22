ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  invisible(path)
}

write_run_log <- function(path, script_name) {
  ensure_dir(dirname(path))

  lines <- c(
    paste0("script: ", script_name),
    paste0("timestamp: ", format(Sys.time(), tz = "Europe/Berlin", usetz = TRUE)),
    "",
    capture.output(sessionInfo())
  )

  writeLines(lines, con = path)
  invisible(path)
}

`%||%` <- function(lhs, rhs) {
  if (length(lhs) == 0 || all(is.na(lhs))) {
    return(rhs)
  }
  lhs
}

paper_number <- function(value, digits = 3) {
  vapply(value, function(single_value) {
    if (is.na(single_value)) {
      return(NA_character_)
    }
    if (single_value < 0.001) {
      return("< 0.001")
    }
    formatC(single_value, format = "f", digits = digits)
  }, character(1))
}

vector_label <- function(values, empty = "none") {
  if (length(values) == 0 || all(is.na(values))) {
    return(empty)
  }
  paste(values, collapse = ", ")
}

lag_window_label <- function(max_lag) {
  vapply(max_lag, function(single_lag) {
    if (is.na(single_lag)) {
      return(NA_character_)
    }
    if (single_lag <= 1) {
      return("1")
    }
    paste0("1-", single_lag)
  }, character(1))
}

soft_threshold <- function(z, gamma) {
  sign(z) * pmax(abs(z) - gamma, 0)
}

safe_positive <- function(x, epsilon = 1e-7) {
  pmax(as.numeric(x), epsilon)
}

rolling_mean_7 <- function(x) {
  zoo::rollapply(
    x,
    width = 7,
    FUN = mean,
    align = "right",
    fill = NA_real_,
    na.rm = FALSE
  )
}
