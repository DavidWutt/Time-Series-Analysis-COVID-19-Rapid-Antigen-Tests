pair_transformation <- function(series, type) {
  series <- safe_positive(series)

  if (identical(type, "seasonal_log_diff")) {
    return(as.numeric(diff(diff(log(series), lag = 1), lag = 7)))
  }

  as.numeric(diff(log(series), lag = 1))
}

multivariate_transformation <- function(series) {
  as.numeric(diff(diff(log(safe_positive(series)), lag = 1), lag = 7))
}

multivariate_max_lag <- function() {
  21L
}

run_prewhiten_ccf <- function(x, y, max_lag = 21) {
  prewhitened <- TSA::prewhiten(x, y, plot = FALSE, lag.max = max_lag)
  ccf_obj <- prewhitened$ccf

  n_eff <- min(length(stats::na.omit(x)), length(stats::na.omit(y)))
  z_scores <- as.numeric(ccf_obj$acf) * sqrt(n_eff)
  p_values <- 2 * stats::pnorm(-abs(z_scores))

  tibble::tibble(
    lag = as.integer(as.numeric(ccf_obj$lag)),
    ccf = as.numeric(ccf_obj$acf),
    p_value = p_values,
    p_value_bh = stats::p.adjust(p_values, method = "BH")
  )
}

run_granger_windows <- function(x, y, lags) {
  if (length(lags) == 0) {
    return(
      tibble::tibble(
        lags_used = integer(),
        p_value = numeric(),
        p_value_bh = numeric()
      )
    )
  }

  windows <- sort(unique(abs(lags)))

  out <- purrr::map_dfr(windows, function(k) {
    test <- lmtest::grangertest(x, y, order = k)
    tibble::tibble(
      lags_used = k,
      p_value = test$`Pr(>F)`[[2]]
    )
  })

  out |>
    dplyr::mutate(p_value_bh = stats::p.adjust(p_value, method = "BH"))
}

run_bivariate_pair <- function(series_df, phase_label, target_var, transform_type) {
  x <- pair_transformation(series_df$quotient_mean_7, transform_type)
  y <- pair_transformation(series_df[[target_var]], transform_type)

  ccf_results <- run_prewhiten_ccf(x, y)
  significant_negative_lags <- ccf_results |>
    dplyr::filter(lag < 0, p_value_bh < 0.05) |>
    dplyr::arrange(lag)

  granger_results <- run_granger_windows(
    x = x,
    y = y,
    lags = significant_negative_lags$lag
  )

  list(
    ccf_detailed = ccf_results |>
      dplyr::mutate(phase = phase_label, target = target_var),
    table_1 = significant_negative_lags |>
      dplyr::transmute(
        phase = phase_label,
        target = target_var,
        lag = lag,
        adjusted_p_value = p_value_bh,
        adjusted_p_value_display = paper_number(p_value_bh)
      ),
    table_2 = granger_results |>
      dplyr::transmute(
        phase = phase_label,
        target = target_var,
        lags_used = lag_window_label(lags_used),
        adjusted_p_value = p_value_bh,
        adjusted_p_value_display = paper_number(p_value_bh)
      )
  )
}

format_bivariate_target_order <- function(data) {
  data |>
    dplyr::mutate(
      target = dplyr::recode(
        target,
        incidence = "7-day incidence",
        hospitalizations = "7-day incidence of hospitalizations",
        icu_ratio_mean_7 = "Ratio of occupied ICU beds"
      ),
      target_order = dplyr::case_when(
        target == "7-day incidence" ~ 1L,
        target == "7-day incidence of hospitalizations" ~ 2L,
        target == "Ratio of occupied ICU beds" ~ 3L,
        TRUE ~ 99L
      )
    ) |>
    dplyr::arrange(target_order, phase)
}

run_bivariate_analysis <- function() {
  targets <- list(
    incidence = "first_log_diff",
    hospitalizations = "first_log_diff",
    icu_ratio_mean_7 = "seasonal_log_diff"
  )

  phase_ids <- c(7L, 8L)

  results <- purrr::map(phase_ids, function(phase_id) {
    phase_df <- prepare_phase_series(phase_id)
    phase_label <- paste0("Phase ", phase_id)

    pair_results <- purrr::imap(targets, function(transform_type, target_var) {
      run_bivariate_pair(phase_df, phase_label, target_var, transform_type)
    })

    list(
      table_1 = dplyr::bind_rows(purrr::map(pair_results, "table_1")),
      table_2 = dplyr::bind_rows(purrr::map(pair_results, "table_2")),
      ccf_detailed = dplyr::bind_rows(purrr::map(pair_results, "ccf_detailed"))
    )
  })

  list(
    table_1 = dplyr::bind_rows(purrr::map(results, "table_1")) |>
      format_bivariate_target_order() |>
      dplyr::arrange(target_order, phase, lag) |>
      dplyr::select(-target_order),
    table_2 = dplyr::bind_rows(purrr::map(results, "table_2")) |>
      format_bivariate_target_order() |>
      dplyr::select(-target_order),
    ccf_detailed = dplyr::bind_rows(purrr::map(results, "ccf_detailed"))
  )
}

build_multivariate_input <- function(series_df) {
  tibble::tibble(
    quotient = multivariate_transformation(series_df$quotient_mean_7),
    incidence = multivariate_transformation(series_df$incidence),
    hospitalizations = multivariate_transformation(series_df$hospitalizations),
    icu = multivariate_transformation(series_df$icu_ratio_mean_7)
  )
}

create_lagged_design <- function(df_data, target_var, predictors, max_lag = multivariate_max_lag()) {
  lag_blocks <- purrr::map(predictors, function(var_name) {
    purrr::map_dfc(seq_len(max_lag), function(lag_n) {
      tibble::tibble(!!paste0(var_name, "_lag", lag_n) := dplyr::lag(df_data[[var_name]], lag_n))
    })
  })

  design <- dplyr::bind_cols(
    tibble::tibble(response = df_data[[target_var]]),
    dplyr::bind_cols(lag_blocks)
  ) |>
    stats::na.omit()

  list(
    response = as.numeric(design$response),
    predictors = as.matrix(dplyr::select(design, -response))
  )
}

standardize_design <- function(X, y) {
  x_means <- colMeans(X)
  x_sds <- apply(X, 2, stats::sd)
  x_sds[is.na(x_sds) | x_sds == 0] <- 1

  y_mean <- mean(y)
  y_centered <- y - y_mean
  X_scaled <- scale(X, center = x_means, scale = x_sds)

  list(
    X_scaled = X_scaled,
    y_centered = y_centered,
    x_means = x_means,
    x_sds = x_sds,
    y_mean = y_mean
  )
}

lambda_grid <- function(X, y, n_lambda = 50, min_ratio = 0.02) {
  std <- standardize_design(X, y)
  lambda_max <- max(abs(colMeans(std$X_scaled * std$y_centered)))

  if (!is.finite(lambda_max) || lambda_max <= 0) {
    return(seq(0.001, 0.05, length.out = n_lambda))
  }

  exp(seq(log(lambda_max), log(lambda_max * min_ratio), length.out = n_lambda))
}

fit_lasso_cd <- function(X, y, lambda, max_iter = 2000, tol = 1e-7) {
  std <- standardize_design(X, y)
  X_scaled <- std$X_scaled
  y_centered <- std$y_centered

  n <- nrow(X_scaled)
  p <- ncol(X_scaled)
  beta_scaled <- numeric(p)
  col_norms <- colSums(X_scaled ^ 2) / n

  for (iter in seq_len(max_iter)) {
    beta_old <- beta_scaled

    for (j in seq_len(p)) {
      residual_j <- y_centered - (X_scaled %*% beta_scaled) + X_scaled[, j] * beta_scaled[j]
      rho <- sum(X_scaled[, j] * residual_j) / n
      beta_scaled[j] <- soft_threshold(rho, lambda) / col_norms[j]
    }

    if (max(abs(beta_scaled - beta_old)) < tol) {
      break
    }
  }

  coefficients <- beta_scaled / std$x_sds
  intercept <- std$y_mean - sum((std$x_means / std$x_sds) * beta_scaled)

  list(
    intercept = intercept,
    coefficients = coefficients
  )
}

predict_lasso_cd <- function(model, newx) {
  as.numeric(model$intercept + as.matrix(newx) %*% model$coefficients)
}

fit_lasso_with_expanding_cv <- function(X, y, init_prop = 0.6, n_lambda = 50) {
  lambdas <- lambda_grid(X, y, n_lambda = n_lambda)
  n_rows <- nrow(X)
  init_window <- max(25, floor(init_prop * n_rows))
  test_indices <- seq.int(init_window + 1, n_rows)

  cv_errors <- matrix(NA_real_, nrow = length(test_indices), ncol = length(lambdas))

  for (i in seq_along(test_indices)) {
    test_idx <- test_indices[[i]]
    train_idx <- seq_len(test_idx - 1)

    X_train <- X[train_idx, , drop = FALSE]
    y_train <- y[train_idx]
    X_test <- X[test_idx, , drop = FALSE]
    y_test <- y[test_idx]

    for (lambda_idx in seq_along(lambdas)) {
      fit <- fit_lasso_cd(X_train, y_train, lambdas[[lambda_idx]])
      pred <- predict_lasso_cd(fit, X_test)
      cv_errors[i, lambda_idx] <- (y_test - pred) ^ 2
    }
  }

  mean_errors <- colMeans(cv_errors, na.rm = TRUE)
  best_lambda <- lambdas[[which.min(mean_errors)]]
  final_fit <- fit_lasso_cd(X, y, best_lambda)

  list(
    lambda = best_lambda,
    mean_errors = mean_errors,
    lambdas = lambdas,
    final_fit = final_fit
  )
}

selected_lag_map <- function(coefficient_names, coefficient_values) {
  selected <- coefficient_names[abs(coefficient_values) > 0]

  split <- stringr::str_match(selected, "^(.*)_lag([0-9]+)$")
  valid <- !is.na(split[, 1])
  split <- split[valid, , drop = FALSE]

  if (nrow(split) == 0) {
    return(list())
  }

  lag_df <- tibble::tibble(
    variable = split[, 2],
    lag = as.integer(split[, 3])
  ) |>
    dplyr::group_by(variable) |>
    dplyr::summarise(lags = list(sort(unique(lag))), .groups = "drop")

  stats::setNames(lag_df$lags, lag_df$variable)
}

make_gamm_dataset <- function(df_data, target_var, lag_map) {
  lag_columns <- purrr::imap(lag_map, function(lags, variable) {
    purrr::map_dfc(lags, function(lag_n) {
      tibble::tibble(!!paste0(variable, "_lag", lag_n) := dplyr::lag(df_data[[variable]], lag_n))
    })
  })

  out <- dplyr::bind_cols(
    tibble::tibble(!!target_var := df_data[[target_var]]),
    dplyr::bind_cols(lag_columns)
  ) |>
    stats::na.omit() |>
    dplyr::mutate(time = seq_len(dplyr::n()))

  out
}

fit_gamm_model <- function(df_data, target_var, lag_map, k_candidates = c(10, 5)) {
  lag_columns <- purrr::imap(lag_map, function(lags, variable) {
    purrr::map_chr(lags, function(lag_n) paste0(variable, "_lag", lag_n))
  }) |>
    unlist(use.names = FALSE)

  if (length(lag_columns) == 0) {
    stop("No lagged predictors were selected for the GAMM fit.")
  }

  data_for_fit <- make_gamm_dataset(df_data, target_var, lag_map)
  last_error <- NULL

  for (k_value in k_candidates) {
    formula_text <- paste(
      target_var,
      "~",
      paste(paste0("s(", lag_columns, ", k = ", k_value, ")"), collapse = " + ")
    )

    fit <- try(
      mgcv::gamm(
        stats::as.formula(formula_text),
        data = data_for_fit,
        correlation = nlme::corAR1(form = ~ time)
      ),
      silent = TRUE
    )

    if (!inherits(fit, "try-error")) {
      return(list(model = fit, k_used = k_value, data = data_for_fit))
    }

    last_error <- as.character(fit)
  }

  stop(last_error)
}

parse_term_label <- function(term_name) {
  clean <- stringr::str_remove_all(term_name, "^s\\(|\\)$")
  pieces <- stringr::str_match(clean, "^(.*)_lag([0-9]+)$")

  variable_label <- dplyr::recode(
    pieces[, 2],
    quotient = "Ratio",
    incidence = "Incidence",
    hospitalizations = "Hosp.",
    icu = "ICU"
  )

  paste(variable_label, "lag", pieces[, 3])
}

extract_term_table <- function(gamm_fit) {
  summary_fit <- summary(gamm_fit$model$gam)
  p_values <- summary_fit[["s.pv"]]
  term_names <- rownames(summary_fit[["s.table"]])
  adjusted <- stats::p.adjust(p_values, method = "BH")

  tibble::tibble(
    term = term_names,
    variable = stringr::str_match(stringr::str_remove_all(term_names, "^s\\(|\\)$"), "^(.*)_lag")[, 2],
    lag = as.integer(stringr::str_match(term_names, "_lag([0-9]+)")[, 2]),
    parameter = parse_term_label(term_names),
    p_value = p_values,
    p_value_bh = adjusted,
    p_value_display = paper_number(adjusted),
    significant = adjusted < 0.05
  ) |>
    dplyr::arrange(variable, lag)
}

reduced_model_definitions <- function() {
  list(
    list(label = "Incidence, Ratio, Hosp., ICU", predictors = c("incidence", "quotient", "hospitalizations", "icu")),
    list(label = "Incidence, Ratio, Hosp.", predictors = c("incidence", "quotient", "hospitalizations")),
    list(label = "Incidence, Ratio, ICU", predictors = c("incidence", "quotient", "icu")),
    list(label = "Ratio, Hosp., ICU", predictors = c("quotient", "hospitalizations", "icu")),
    list(label = "Incidence, Ratio", predictors = c("incidence", "quotient")),
    list(label = "Ratio, Hosp.", predictors = c("quotient", "hospitalizations")),
    list(label = "Ratio, ICU", predictors = c("quotient", "icu")),
    list(label = "Ratio", predictors = c("quotient")),
    list(label = "Hosp., ICU", predictors = c("hospitalizations", "icu")),
    list(label = "Incidence, Hosp., ICU", predictors = c("incidence", "hospitalizations", "icu"))
  )
}

multivariate_target_label <- function(target_var) {
  dplyr::recode(
    target_var,
    incidence = "7-day incidence",
    hospitalizations = "7-day incidence of hospitalizations",
    icu = "Ratio of occupied ICU beds"
  )
}

multivariate_variable_label <- function(variable) {
  dplyr::recode(
    variable,
    quotient = "Ratio of positive rapid antigen tests",
    incidence = "7-day incidence",
    hospitalizations = "7-day incidence of hospitalizations",
    icu = "Ratio of occupied ICU beds"
  )
}

run_multivariate_for_target <- function(df_data, phase_label, target_var, combo_label, predictors) {
  design <- create_lagged_design(
    df_data,
    target_var,
    predictors,
    max_lag = multivariate_max_lag()
  )
  lasso_fit <- fit_lasso_with_expanding_cv(design$predictors, design$response)
  lag_map <- selected_lag_map(colnames(design$predictors), lasso_fit$final_fit$coefficients)

  gamm_fit <- fit_gamm_model(df_data, target_var, lag_map)
  term_table <- extract_term_table(gamm_fit)
  summary_fit <- summary(gamm_fit$model$gam)

  list(
    lag_map = lag_map,
    gamm_fit = gamm_fit,
    term_table = term_table,
    r_squared = unname(summary_fit$r.sq),
    combo_label = combo_label,
    predictors = predictors,
    phase = phase_label,
    target = target_var
  )
}

run_multivariate_full_models <- function(phase_ids = c(7L, 8L)) {
  target_vars <- c("incidence", "hospitalizations", "icu")
  full_model_table_3 <- list()
  full_model_table_b2 <- list()

  for (phase_id in phase_ids) {
    phase_label <- paste0("Phase ", phase_id)
    message("Full models: ", phase_label)
    df_data <- build_multivariate_input(prepare_phase_series(phase_id))

    phase_full_results <- purrr::map(target_vars, function(target_var) {
      run_multivariate_for_target(
        df_data = df_data,
        phase_label = phase_label,
        target_var = target_var,
        combo_label = "Incidence, Ratio, Hosp., ICU",
        predictors = c("incidence", "quotient", "hospitalizations", "icu")
      )
    })
    names(phase_full_results) <- target_vars

    full_model_table_3[[phase_label]] <- dplyr::bind_rows(purrr::map(phase_full_results, function(result) {
      result$term_table |>
        dplyr::filter(significant) |>
        dplyr::transmute(
          phase = result$phase,
          target = multivariate_target_label(result$target),
          parameter,
          adjusted_p_value = p_value_bh,
          adjusted_p_value_display = p_value_display
        )
    }))

    full_model_table_b2[[phase_label]] <- dplyr::bind_rows(purrr::map(phase_full_results, function(result) {
      purrr::imap_dfr(result$lag_map, function(lags, variable) {
        tibble::tibble(
          phase = result$phase,
          target = multivariate_target_label(result$target),
          variable = multivariate_variable_label(variable),
          lags = vector_label(lags)
        )
      })
    }))
  }

  list(
    table_3 = dplyr::bind_rows(full_model_table_3),
    table_b2 = dplyr::bind_rows(full_model_table_b2)
  )
}

run_multivariate_reduced_models <- function(phase_ids = c(7L, 8L)) {
  target_vars <- c("incidence", "hospitalizations", "icu")
  reduced_tables <- list()
  combo_specs <- reduced_model_definitions()

  for (phase_id in phase_ids) {
    phase_label <- paste0("Phase ", phase_id)
    message("Reduced models: ", phase_label)
    df_data <- build_multivariate_input(prepare_phase_series(phase_id))

    reduced_rows <- purrr::map_dfr(target_vars, function(target_var) {
      purrr::map_dfr(combo_specs, function(spec) {
        result <- run_multivariate_for_target(
          df_data = df_data,
          phase_label = phase_label,
          target_var = target_var,
          combo_label = spec$label,
          predictors = spec$predictors
        )

        ratio_used <- result$lag_map[["quotient"]] %||% integer(0)
        ratio_sig <- result$term_table |>
          dplyr::filter(variable == "quotient", significant)

        tibble::tibble(
          phase = result$phase,
          target = result$target,
          predictors = result$combo_label,
          ratio_lags_used = vector_label(ratio_used),
          ratio_lags_significant = vector_label(ratio_sig$lag),
          ratio_p_values = vector_label(ratio_sig$p_value_display),
          r_squared = round(result$r_squared, 3),
          k_used = result$gamm_fit$k_used
        )
      })
    })

    reduced_tables[[phase_label]] <- reduced_rows
  }

  list(
    reduced_models = dplyr::bind_rows(reduced_tables)
  )
}

run_multivariate_analysis <- function() {
  full_models <- run_multivariate_full_models()
  reduced_models <- run_multivariate_reduced_models()

  list(
    table_3 = full_models$table_3,
    table_b2 = full_models$table_b2,
    reduced_models = reduced_models$reduced_models
  )
}
