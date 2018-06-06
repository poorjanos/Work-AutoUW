# Plotting helpers for report

plot_success <- function(df, use_facet = TRUE, facet_one = ".", facet_two = ".", fig_size = 10, x_label_size = 10) {
  # Plots success rates with optional faceting
  if (use_facet) {
    p <- ggplot(df, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
      geom_line() +
      geom_point_interactive(aes(tooltip = paste0(round(PCT * 100, 2), "%"))) +
      scale_y_continuous(labels = percent) +
      theme(
        axis.text.x = element_text(angle = 90, size = x_label_size),
        legend.position = "bottom"
      ) +
      labs(
        y = "Százlékos arány [%]",
        x = "Idõszak",
        colour = "Mutató"
      ) +
      facet_grid(paste(facet_one, "~", facet_two))
    ggiraph(code = print(p), width_svg = fig_size)
  } else {
    p <- ggplot(df, aes(IDOSZAK, PCT, group = MUTATO, colour = MUTATO)) +
      geom_line() +
      geom_point_interactive(aes(tooltip = paste0(round(PCT * 100, 2), "%"))) +
      scale_y_continuous(labels = percent) +
      theme(
        axis.text.x = element_text(angle = 90, size = x_label_size),
        legend.position = "bottom"
      ) +
      labs(
        y = "Százlékos arány [%]",
        x = "Idõszak",
        colour = "Mutató"
      )
    ggiraph(code = print(p), width_svg = fig_size)
  }
}


plot_error_freq <- function(df, use_facet = TRUE, facet_one = ".", facet_two = ".", fig_size = 10, x_label_size = 10, y_label_size = 10) {
  # Plots error frequencies with optional faceting
  if (use_facet) {
    p <- ggplot(df, aes(
      x = factor(df$HIBA, levels = unique(df$HIBA[order(df$GYAKORISAG)])),
      y = GYAKORISAG
    )) +
      geom_bar_interactive(stat = "identity", aes(tooltip = paste0(round(GYAKORISAG * 100, 2), "%"))) +
      theme(axis.text.x = element_text(size = x_label_size),
            axis.text.y = element_text(size = y_label_size)
            ) +
      scale_y_continuous(label = percent) +
      coord_flip() +
      labs(
        y = "Relatív gyakoriság",
        x = "Hiba"
      ) +
      facet_grid(paste(facet_one, "~", facet_two))
    ggiraph(code = print(p), width_svg = fig_size)
  } else {
    p <- ggplot(df, aes(
      x = factor(df$HIBA, levels = df$HIBA[order(df$GYAKORISAG)]),
      y = GYAKORISAG
    )) +
      geom_bar_interactive(stat = "identity", aes(tooltip = paste0(round(GYAKORISAG * 100, 2), "%"))) +
      theme(axis.text.x = element_text(size = x_label_size),
            axis.text.y = element_text(size = y_label_size)
      ) +
      scale_y_continuous(label = percent) +
      coord_flip() +
      labs(
        y = "Relatív gyakoriság",
        x = "Hiba"
      )
    ggiraph(code = print(p), width_svg = fig_size)
  }
}


plot_error_pattern_freq <- function(df, use_facet = TRUE, facet_one = ".", facet_two = ".",
                                    fig_width = 10, fig_height = 6, x_label_size = 10, y_label_size = 10) {
  # Plots error pattern frequencies with optional faceting
  if (use_facet) {
    p <- ggplot(df, aes(
      x = factor(df$HIBA_MINTA, levels = unique(df$HIBA_MINTA[order(df$GYAKORISAG)])),
      y = GYAKORISAG
    )) +
      geom_bar_interactive(stat = "identity", aes(tooltip = paste0(round(GYAKORISAG * 100, 2), "%"))) +
      theme(axis.text.x = element_text(size = x_label_size),
            axis.text.y = element_text(size = y_label_size)
      ) +
      scale_y_continuous(label = percent) +
      coord_flip() +
      labs(
        y = "Relatív gyakoriság",
        x = "Hibamintázat"
      ) +
      facet_grid(paste(facet_one, "~", facet_two))
    ggiraph(code = print(p), width_svg = fig_width, height_svg = fig_height)
  } else {
    p <- ggplot(df, aes(
      x = factor(df$HIBA_MINTA, levels = df$HIBA_MINTA[order(df$GYAKORISAG)]),
      y = GYAKORISAG
    )) +
      geom_bar_interactive(stat = "identity", aes(tooltip = paste0(round(GYAKORISAG * 100, 2), "%"))) +
      theme(axis.text.x = element_text(size = x_label_size),
            axis.text.y = element_text(size = y_label_size)
      ) +
      scale_y_continuous(label = percent) +
      coord_flip() +
      labs(
        y = "Relatív gyakoriság",
        x = "Hibamintázat"
      )
    ggiraph(code = print(p), width_svg = fig_width, height_svg = fig_height)
  }
}


plot_error_freq_ts <- function(df, subset_by, facet_by,  fig_width = 10, fig_height = 6,
                               x_label_size = 6) {
  # Plots time series of error causes
  p <- ggplot(
    df[df$MODTYP == subset_by, ],
    aes(
      x = IDOSZAK,
      y = GYAKORISAG,
      group = 1
    )
  ) +
    geom_line(size = 1) +
    geom_point_interactive(size = 1, shape = 15, aes(tooltip = paste0(round(GYAKORISAG * 100, 2), "%"))) +
    scale_y_continuous(label = percent) +
    theme(
      axis.text.x = element_text(angle = 90, size = x_label_size),
      strip.text.x = element_text(size = 12)
    ) +
    facet_wrap(facet_by, ncol = 8, labeller = label_wrap_gen(width = 20))
  ggiraph(code = print(p), width_svg = fig_width, height_svg = fig_height)
}
