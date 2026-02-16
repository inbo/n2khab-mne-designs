plot_sample_sizes <- function(ssizes, y_add = 0) {
  ssizes %>%
    mutate(
      relative_errormargin = relative_errormargin %>%
        scales::label_percent()(.) %>%
        fct()
    ) %>%
    ggplot(aes(
      x = m,
      y = n_tg_yearly,
      linetype = relative_errormargin,
      group = relative_errormargin
    )) +
    geom_line(colour = "white") +
    geom_point(aes(colour = cost_yearly)) +
    scale_colour_viridis_c(direction = -1, option = "A") +
    scale_x_continuous(breaks = 1:50) +
    scale_y_continuous(expand = if (missing(y_add)) waiver() else {
      expansion(add = y_add)
    }) +
    facet_wrap(~ scheme + target_variable, scales = "free_x") +
    labs(
      x = "Yearly repetitions per location",
      y = "Yearly number of locations per typegroup",
      colour = "Relative yearly cost",
      linetype = "Relative error margin"
    ) +
    theme(
      panel.background = element_rect(fill = "grey60"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey70")
    )
}


plot_m_to_errmarg <- function(df, facet_scales = "free_y") {
  df %>%
    ggplot(aes(
      x = m,
      y = relative_errormargin,
      linetype = target_variable,
      group = str_c(target_variable, n_tg)
    )) +
    geom_line(colour = "white") +
    geom_point(aes(colour = cost_yearly)) +
    geom_text(
      data = df %>% filter(m == 1),
      mapping = aes(x = m, y = relative_errormargin, label = round(n_tg_yearly)),
      colour = "white",
      hjust = -0.5
    ) +
    scale_colour_viridis_c(direction = -1, option = "A") +
    scale_x_continuous(breaks = 1:50) +
    facet_wrap(~ scheme, scales = facet_scales) +
    labs(
      x = "Yearly repetitions per location",
      y = "Relative error margin",
      colour = "Relative yearly cost",
    ) +
    theme(
      panel.background = element_rect(fill = "grey60"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_blank(),
      panel.grid.major.y = element_line(colour = "grey70")
    )
}


enforce_lowercase <- function(x) {
  if (knitr::is_latex_output()) {
    str_glue("\\lowercase{{{x}}}")
  } else {
    str_glue("{x}")
  }
}
