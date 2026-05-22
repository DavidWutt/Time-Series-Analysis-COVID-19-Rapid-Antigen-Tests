phase_metadata <- function() {
  tibble::tribble(
    ~phase_id, ~phase_label, ~start_date,           ~end_date,             ~risk_level,
    0L,        "0",          as.Date("2020-02-25"), as.Date("2020-03-08"), "moderate",
    1L,        "1",          as.Date("2020-03-09"), as.Date("2020-05-17"), "medium",
    2L,        "2",          as.Date("2020-05-18"), as.Date("2020-09-27"), "medium",
    3L,        "3",          as.Date("2020-09-28"), as.Date("2021-02-28"), "very high",
    4L,        "4",          as.Date("2021-03-01"), as.Date("2021-06-13"), "very high",
    5L,        "5",          as.Date("2021-06-14"), as.Date("2021-08-01"), "medium",
    6L,        "6",          as.Date("2021-08-02"), as.Date("2021-12-26"), "medium",
    7L,        "7",          as.Date("2021-12-27"), as.Date("2022-05-29"), "very high",
    8L,        "8",          as.Date("2022-05-30"), as.Date("2023-01-22"), "medium",
    9L,        "9",          as.Date("2023-01-23"), as.Date("2023-09-29"), "moderate"
  )
}

figure_a1_window <- function() {
  list(
    start_date = as.Date("2020-02-25"),
    end_date = as.Date("2023-05-21")
  )
}

figure_a1_agrdt_window <- function() {
  list(
    start_date = as.Date("2021-03-01"),
    end_date = as.Date("2023-05-21")
  )
}

load_public_health_cleaned <- function(
  path = "Public_Health_Data_Sets/public_health_data_cleaned.csv"
) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols_only(
      test_date = readr::col_date(),
      result = readr::col_character(),
      gender = readr::col_character()
    )
  )
}

build_public_health_daily_from_cleaned <- function(
  cleaned_path = "Public_Health_Data_Sets/public_health_data_cleaned.csv"
) {
  cleaned <- readr::read_csv(
    cleaned_path,
    show_col_types = FALSE,
    col_types = readr::cols_only(
      test_date = readr::col_date(),
      result = readr::col_character()
    )
  )

  date_index <- tibble::tibble(
    date = seq.Date(min(cleaned$test_date), max(cleaned$test_date), by = "day")
  )

  daily <- cleaned |>
    dplyr::group_by(test_date) |>
    dplyr::summarise(
      count = dplyr::n(),
      count_positive = sum(result == "positive", na.rm = TRUE),
      count_negative = sum(result == "negative", na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::rename(date = test_date) |>
    dplyr::right_join(date_index, by = "date") |>
    dplyr::arrange(date) |>
    tidyr::replace_na(list(count = 0L, count_positive = 0L, count_negative = 0L)) |>
    dplyr::mutate(
      quotient = dplyr::if_else(count > 0, count_positive / count, 0),
      count_mean_7 = rolling_mean_7(count),
      count_positive_mean_7 = rolling_mean_7(count_positive),
      count_negative_mean_7 = rolling_mean_7(count_negative),
      quotient_mean_7 = dplyr::if_else(
        count_mean_7 > 0,
        count_positive_mean_7 / count_mean_7,
        0
      )
    )

  daily
}

load_public_health_daily <- function(
  path = "Public_Health_Data_Sets/public_health_data_count_daily.csv",
  cleaned_path = "Public_Health_Data_Sets/public_health_data_cleaned.csv"
) {
  if (file.exists(path)) {
    return(
      readr::read_csv(
        path,
        show_col_types = FALSE,
        col_types = readr::cols(
          test_date = readr::col_date(),
          count = readr::col_double(),
          count_positive = readr::col_double(),
          count_negative = readr::col_double(),
          quotient = readr::col_double(),
          count_mean_7 = readr::col_double(),
          count_positive_mean_7 = readr::col_double(),
          count_negative_mean_7 = readr::col_double(),
          quotient_mean_7 = readr::col_double()
        )
      ) |>
        dplyr::rename(date = test_date)
    )
  }

  build_public_health_daily_from_cleaned(cleaned_path)
}

load_incidence <- function(
  path = "RKI_Data_Sets/7-Tage-Inzidenz_der_COVID-19-Fälle_in_Deutschland/COVID-19-Faelle_7-Tage-Inzidenz_Deutschland.csv"
) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      Meldedatum = readr::col_date(),
      Altersgruppe = readr::col_character(),
      `Inzidenz_7-Tage` = readr::col_double()
    )
  ) |>
    dplyr::filter(Altersgruppe == "00+") |>
    dplyr::transmute(date = Meldedatum, incidence = `Inzidenz_7-Tage`)
}

load_hospitalizations <- function(
  path = "RKI_Data_Sets/COVID-19-Hospitalisierungen_in_Deutschland/Aktuell_Deutschland_COVID-19-Hospitalisierungen.csv"
) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      Datum = readr::col_date(),
      Bundesland = readr::col_character(),
      Altersgruppe = readr::col_character(),
      `7T_Hospitalisierung_Inzidenz` = readr::col_double()
    )
  ) |>
    dplyr::filter(Bundesland == "Bundesgebiet", Altersgruppe == "00+") |>
    dplyr::transmute(date = Datum, hospitalizations = `7T_Hospitalisierung_Inzidenz`) |>
    dplyr::arrange(date)
}

load_icu_ratio <- function(
  path = "RKI_Data_Sets/Intensivkapazitäten_und_COVID-19-Intensivbettenbelegung_in_Deutschland/Intensivregister_Deutschland_Kapazitaeten.csv"
) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(
      datum = readr::col_date(),
      bundesland_id = readr::col_character(),
      faelle_covid_aktuell = readr::col_double(),
      intensivbetten_belegt = readr::col_double(),
      intensivbetten_frei = readr::col_double()
    )
  ) |>
    dplyr::filter(bundesland_id == "00") |>
    dplyr::group_by(datum) |>
    dplyr::summarise(
      covid_cases_icu = sum(faelle_covid_aktuell, na.rm = TRUE),
      occupied_icu_beds = sum(intensivbetten_belegt, na.rm = TRUE),
      free_icu_beds = sum(intensivbetten_frei, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      icu_capacity = occupied_icu_beds + free_icu_beds,
      icu_ratio = dplyr::if_else(icu_capacity > 0, covid_cases_icu / icu_capacity, NA_real_),
      icu_ratio_mean_7 = rolling_mean_7(icu_ratio)
    ) |>
    dplyr::transmute(date = datum, icu_ratio, icu_ratio_mean_7)
}

prepare_national_series <- function(start_date, end_date) {
  date_index <- tibble::tibble(date = seq.Date(start_date, end_date, by = "day"))

  date_index |>
    dplyr::left_join(load_public_health_daily(), by = "date") |>
    dplyr::left_join(load_incidence(), by = "date") |>
    dplyr::left_join(load_hospitalizations(), by = "date") |>
    dplyr::left_join(load_icu_ratio(), by = "date") |>
    dplyr::mutate(
      across(
        c(count, count_positive, count_negative, quotient, count_mean_7,
          count_positive_mean_7, count_negative_mean_7, quotient_mean_7),
        ~ tidyr::replace_na(.x, 0)
      )
    )
}

prepare_phase_series <- function(phase_id) {
  phase <- phase_metadata() |>
    dplyr::filter(phase_id == !!phase_id)

  prepare_national_series(phase$start_date[[1]], phase$end_date[[1]]) |>
    dplyr::mutate(phase = paste0("Phase ", phase$phase_id[[1]]))
}

prepare_figure_a1_series <- function() {
  window <- figure_a1_window()
  ag_rdt_window <- figure_a1_agrdt_window()

  prepare_national_series(window$start_date, window$end_date) |>
    dplyr::mutate(
      quotient = dplyr::if_else(
        date >= ag_rdt_window$start_date & date <= ag_rdt_window$end_date,
        quotient,
        NA_real_
      ),
      quotient_mean_7 = tidyr::replace_na(quotient_mean_7, 0),
      quotient_mean_7 = dplyr::if_else(
        date >= ag_rdt_window$start_date & date <= ag_rdt_window$end_date,
        quotient_mean_7,
        NA_real_
      )
    )
}

compute_table_a1 <- function() {
  cleaned <- load_public_health_cleaned()
  phases <- phase_metadata() |>
    dplyr::filter(phase_id >= 4)

  total_start <- min(phases$start_date)
  total_end <- max(phases$end_date)

  summarise_group <- function(data, label, pct_denom) {
    total_n <- nrow(data)
    gender_table <- table(data$gender, useNA = "ifany")

    tibble::tibble(
      phase = label,
      number_of_observations = total_n,
      share_of_total_observations = total_n / pct_denom,
      negative_n = sum(data$result == "negative", na.rm = TRUE),
      positive_n = sum(data$result == "positive", na.rm = TRUE),
      female_n = unname(gender_table["female"] %||% 0),
      male_n = unname(gender_table["male"] %||% 0),
      diverse_n = unname(gender_table["diverse"] %||% 0),
      gender_na_n = unname(gender_table[is.na(names(gender_table))] %||% 0)
    ) |>
      dplyr::mutate(
        negative_share = negative_n / number_of_observations,
        positive_share = positive_n / number_of_observations,
        female_share = female_n / number_of_observations,
        male_share = male_n / number_of_observations,
        diverse_share = diverse_n / number_of_observations,
        gender_na_share = gender_na_n / number_of_observations
      )
  }

  cleaned_restricted <- cleaned |>
    dplyr::filter(test_date >= total_start, test_date <= total_end)

  rows <- purrr::map_dfr(seq_len(nrow(phases)), function(i) {
    phase <- phases[i, ]
    phase_data <- cleaned |>
      dplyr::filter(test_date >= phase$start_date, test_date <= phase$end_date)

    summarise_group(
      phase_data,
      paste0("Phase ", phase$phase_id),
      nrow(cleaned_restricted)
    )
  })

  dplyr::bind_rows(
    summarise_group(cleaned_restricted, "Phase 4 - Phase 9", nrow(cleaned_restricted)),
    rows
  )
}
