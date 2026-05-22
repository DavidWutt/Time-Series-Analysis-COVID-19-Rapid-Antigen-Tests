risk_palette <- c(
  moderate = "#d6e3d5",
  medium = "#f5ead0",
  high = "#edd9b5",
  `very high` = "#ead1d1"
)

indicator_palette <- c(
  quotient = "#295CFF",
  incidence = "#D95F02",
  hospitalizations = "#1B9E77",
  icu = "#C58B00"
)

figure_1_curve_palette <- c(
  "Ratio Ag-RDTs" = "blue",
  comparator = "red"
)

phase_background_data <- function(start_date, end_date) {
  phase_metadata() |>
    dplyr::filter(end_date >= !!start_date, start_date <= !!end_date) |>
    dplyr::mutate(
      risk_level = factor(
        risk_level,
        levels = c("moderate", "medium", "high", "very high")
      ),
      xmin = pmax(start_date, !!start_date),
      xmax = pmin(end_date, !!end_date),
      label_x = xmin + floor((xmax - xmin) / 2)
    )
}

figure_1_theme <- function(show_legend = FALSE, show_x_title = FALSE, show_x_text = TRUE) {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#D9D9D9", linewidth = 0.4),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      axis.title = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 10),
      axis.title.x = if (show_x_title) ggplot2::element_text(size = 12) else ggplot2::element_blank(),
      axis.text.x = if (show_x_text) ggplot2::element_text(size = 10) else ggplot2::element_blank(),
      axis.ticks.x = if (show_x_text) ggplot2::element_line() else ggplot2::element_blank(),
      axis.title.y.right = ggplot2::element_text(angle = 270, vjust = 1.15, size = 12),
      plot.title = ggplot2::element_text(size = 11, hjust = 0.5),
      plot.tag = ggplot2::element_text(face = "bold", size = 12),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.box.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.position = if (show_legend) "bottom" else "none",
      legend.box = "horizontal",
      legend.direction = "vertical",
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),
      plot.margin = ggplot2::margin(5.5, 12, 5.5, 5.5)
    )
}

build_figure_1_panel <- function(
  data,
  indicator_column,
  indicator_label,
  scale_factor,
  x_limits,
  y_limits_left,
  tag = NULL,
  title = NULL,
  show_legend = FALSE,
  show_x_title = FALSE,
  show_x_text = TRUE
) {
  plot_data <- data |>
    dplyr::mutate(
      indicator_scaled = .data[[indicator_column]] * scale_factor
    )

  ggplot2::ggplot(plot_data, ggplot2::aes(x = date)) +
    ggplot2::geom_line(
      ggplot2::aes(y = quotient_mean_7, color = "Ratio Ag-RDTs"),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = indicator_scaled, color = .env$indicator_label),
      linewidth = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "Ratio Ag-RDTs" = figure_1_curve_palette[["Ratio Ag-RDTs"]],
        stats::setNames(figure_1_curve_palette[["comparator"]], indicator_label)
      ),
      breaks = c("Ratio Ag-RDTs", indicator_label)
    ) +
    ggplot2::scale_y_continuous(
      name = "Ratio Ag-RDTs",
      limits = y_limits_left,
      sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = indicator_label)
    ) +
    ggplot2::scale_x_date(
      limits = x_limits,
      date_labels = "%b %Y",
      date_breaks = "2 months",
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title = title,
      tag = tag,
      x = if (show_x_title) "Date" else NULL
    ) +
    figure_1_theme(
      show_legend = show_legend,
      show_x_title = show_x_title,
      show_x_text = show_x_text
    )
}

assemble_figure_1_row <- function(left_plot, right_plot) {
  legend <- cowplot::get_legend(left_plot + ggplot2::theme(legend.position = "bottom"))

  row_panels <- cowplot::plot_grid(
    left_plot + ggplot2::theme(legend.position = "none"),
    right_plot + ggplot2::theme(legend.position = "none"),
    ncol = 2,
    align = "hv",
    axis = "tblr",
    rel_widths = c(1, 1)
  )

  cowplot::plot_grid(
    row_panels,
    legend,
    ncol = 1,
    rel_heights = c(1, 0.16)
  )
}

build_dual_axis_plot <- function(
  data,
  indicator_column,
  indicator_label,
  indicator_color,
  scale_factor,
  x_limits,
  y_limits_left,
  secondary_breaks = ggplot2::waiver(),
  tag = NULL,
  title = NULL,
  show_background = FALSE,
  show_phase_labels = FALSE
) {
  plot_data <- data |>
    dplyr::mutate(
      indicator_scaled = .data[[indicator_column]] * scale_factor
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date))

  if (show_background) {
    bg <- phase_background_data(x_limits[[1]], x_limits[[2]])

    p <- p +
      ggplot2::geom_rect(
        data = bg,
        ggplot2::aes(
          xmin = xmin,
          xmax = xmax,
          ymin = -Inf,
          ymax = Inf,
          fill = risk_level
        ),
        inherit.aes = FALSE,
        alpha = 0.95
      ) +
      ggplot2::geom_vline(
        data = bg,
        ggplot2::aes(xintercept = xmin),
        inherit.aes = FALSE,
        linewidth = 0.25,
        color = "grey45"
      )

    if (show_phase_labels) {
      p <- p +
        ggplot2::geom_text(
          data = bg,
          ggplot2::aes(x = label_x, y = y_limits_left[[2]] * 0.97, label = phase_label),
          inherit.aes = FALSE,
          size = 2.4,
          vjust = 1
        )
    }
  }

  p <- p +
    ggplot2::geom_line(
      ggplot2::aes(y = quotient_mean_7, color = "Ratio Ag-RDTs"),
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = indicator_scaled, color = .env$indicator_label),
      linewidth = 0.7,
      na.rm = TRUE
    ) +
    ggplot2::scale_color_manual(
      values = stats::setNames(
        c(indicator_palette[["quotient"]], indicator_color),
        c("Ratio Ag-RDTs", indicator_label)
      ),
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      name = "Ratio Ag-RDTs",
      limits = y_limits_left,
      sec.axis = ggplot2::sec_axis(~ . / scale_factor, name = indicator_label, breaks = secondary_breaks)
    ) +
    ggplot2::scale_x_date(
      limits = x_limits,
      date_labels = "%b %Y",
      date_breaks = "4 months",
      expand = c(0, 0)
    ) +
    ggplot2::labs(tag = tag, title = title, x = "Date") +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#E5E5E5", linewidth = 0.4),
      legend.position = "top",
      legend.box = "vertical",
      legend.text = ggplot2::element_text(size = 8),
      axis.title.y.right = ggplot2::element_text(angle = 270, vjust = 1.2),
      plot.tag = ggplot2::element_text(face = "bold", size = 11),
      plot.title = ggplot2::element_text(size = 10, hjust = 0.5)
    )

  if (show_background) {
    p <- p + ggplot2::scale_fill_manual(
      values = risk_palette,
      breaks = c("moderate", "medium", "high", "very high"),
      drop = FALSE,
      name = NULL
    )
  }

  p
}

render_figure_1 <- function(output_dir = "results/figures") {
  ensure_dir(output_dir)

  phase7 <- prepare_phase_series(7)
  phase8 <- prepare_phase_series(8)

  figure_data <- dplyr::bind_rows(phase7, phase8)
  readr::write_csv(figure_data, file.path(output_dir, "figure_1_data.csv"))

  row_a <- assemble_figure_1_row(
    build_figure_1_panel(
      phase7,
      "incidence",
      "7-Day Incidence",
      scale_factor = 0.0001,
      x_limits = c(min(phase7$date), max(phase7$date)),
      y_limits_left = c(0, 0.25),
      tag = "A",
      title = "Phase 7"
    ),
    build_figure_1_panel(
      phase8,
      "incidence",
      "7-Day Incidence",
      scale_factor = 0.0001,
      x_limits = c(min(phase8$date), max(phase8$date)),
      y_limits_left = c(0, 0.25),
      title = "Phase 8"
    )
  )

  row_b <- assemble_figure_1_row(
    build_figure_1_panel(
      phase7,
      "hospitalizations",
      "7-Day Incidence of Hospitalizations",
      scale_factor = 0.01,
      x_limits = c(min(phase7$date), max(phase7$date)),
      y_limits_left = c(0, 0.25),
      tag = "B"
    ),
    build_figure_1_panel(
      phase8,
      "hospitalizations",
      "7-Day Incidence of Hospitalizations",
      scale_factor = 0.01,
      x_limits = c(min(phase8$date), max(phase8$date)),
      y_limits_left = c(0, 0.25)
    )
  )

  row_c <- assemble_figure_1_row(
    build_figure_1_panel(
      phase7,
      "icu_ratio_mean_7",
      "Ratio Occupied ICU Beds",
      scale_factor = 1,
      x_limits = c(min(phase7$date), max(phase7$date)),
      y_limits_left = c(0, 0.30),
      tag = "C",
      show_x_title = TRUE
    ),
    build_figure_1_panel(
      phase8,
      "icu_ratio_mean_7",
      "Ratio Occupied ICU Beds",
      scale_factor = 1,
      x_limits = c(min(phase8$date), max(phase8$date)),
      y_limits_left = c(0, 0.30),
      show_x_title = TRUE
    )
  )

  figure <- cowplot::plot_grid(
    row_a,
    row_b,
    row_c,
    ncol = 1,
    rel_heights = c(1, 1, 1)
  )

  ggplot2::ggsave(
    file.path(output_dir, "figure_1.png"),
    plot = figure,
    width = 12.5,
    height = 11.5,
    dpi = 300,
    bg = "white"
  )
  ggplot2::ggsave(
    file.path(output_dir, "figure_1.pdf"),
    plot = figure,
    width = 12.5,
    height = 11.5,
    bg = "white"
  )
}

render_figure_a1 <- function(output_dir = "results/figures") {
  ensure_dir(output_dir)

  window <- figure_a1_window()
  figure_data <- prepare_figure_a1_series()
  readr::write_csv(figure_data, file.path(output_dir, "figure_a1_data.csv"))

  grob <- gridExtra::arrangeGrob(
    grobs = list(
      build_dual_axis_plot(
        figure_data, "incidence", "7-Day Incidence", indicator_palette[["incidence"]],
        scale_factor = 0.0001,
        x_limits = c(window$start_date, window$end_date),
        y_limits_left = c(0, 0.42),
        tag = "A",
        show_background = TRUE,
        show_phase_labels = TRUE
      ),
      build_dual_axis_plot(
        figure_data, "hospitalizations", "7-Day Incidence of Hospitalizations", indicator_palette[["hospitalizations"]],
        scale_factor = 0.01,
        x_limits = c(window$start_date, window$end_date),
        y_limits_left = c(0, 0.42),
        tag = "B",
        show_background = TRUE,
        show_phase_labels = TRUE
      ),
      build_dual_axis_plot(
        figure_data, "icu_ratio_mean_7", "Ratio Occupied ICU Beds", indicator_palette[["icu"]],
        scale_factor = 1,
        x_limits = c(window$start_date, window$end_date),
        y_limits_left = c(0, 0.42),
        tag = "C",
        show_background = TRUE,
        show_phase_labels = TRUE
      )
    ),
    ncol = 1
  )

  ggplot2::ggsave(
    file.path(output_dir, "figure_a1.png"),
    plot = grob,
    width = 13,
    height = 14,
    dpi = 300
  )
  ggplot2::ggsave(
    file.path(output_dir, "figure_a1.pdf"),
    plot = grob,
    width = 13,
    height = 14
  )
}
