# Code to generate a plot with the relative errormargin & relative detectable
# difference obtained for a given number of locations per typegroup

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)
library(stringr)
library(purrr)

# chunk copied from index.Rmd, containing the spatial variance (log-scale):
modelvar <-
    tibble(
        scheme = c(rep("GW: 05.1_verdro_gw: terr", 2), rep("GW: 03.3_eutr_gw", 3)),
        target_variable = c("HG3", "LG3", "N-NH4", "N-NO3", "P-PO4"),
        stdev_spatial = c(0.4305, 0.34035, 0.57910, 0.6568, 0.49131)
    ) %>%
    mutate(var_spatial = stdev_spatial^2)

# criteria:
scenarios <-
    crossing(
        target_variable = c("HG3", "LG3"),
        nesting(
            rel_error = c(0.05, 0.1),
            scenario = c("good", "minimal")
        )
    ) %>%
    bind_rows(
        crossing(
            target_variable = c("N-NH4", "N-NO3", "P-PO4"),
            nesting(
                rel_error = c(0.1, 0.15),
                scenario = c("good", "minimal")
            )
        )
    ) %>%
    crossing(
        nesting(
            alpha = c(0.05, 0.1),
            pi = c(0.9, 0.8),
        )
    )

# graph of relative errormargin & relative detectable difference:
errors <-
    modelvar %>%
    inner_join(
        scenarios %>%
            select(-rel_error, -scenario) %>%
            distinct(),
        join_by(target_variable),
        relationship = "one-to-many",
        unmatched = "error"
    ) %>%
    # the target variable with spatially highest variability drives sample size
    filter(
        stdev_spatial == max(stdev_spatial),
        .by = scheme
    ) %>%
    nest(.by = alpha) %>%
    mutate(
        data = map2(alpha, data, function(al, df) {
            if (al == 0.05) {
                crossing(df, n = 300:1200)
            } else {
                crossing(df, n = 150:750)
            }
        })
    ) %>%
    unnest(data) %>%
    mutate(
        scheme = str_glue("{scheme} | {target_variable}"),
        stdev_mu = stdev_spatial / sqrt(n),
        log_error = qnorm(1 - alpha / 2) * stdev_mu,
        rel_error = 10^log_error - 1,
        # detectable difference for one-sided testing:
        log_detectable_diff = qnorm(1 - alpha) * stdev_mu + qnorm(pi) * stdev_mu,
        rel_detectable_diff = 10^log_detectable_diff - 1
    ) %>%
    select(scheme, n, alpha, pi, starts_with("rel_")) %>%
    pivot_longer(
      cols = starts_with("rel_"),
      names_to = "measure",
      values_to = "value"
    ) %>%
    mutate(
        measure = replace_values(
            measure,
            "rel_detectable_diff" ~ str_glue("rel_detectable_diff (power: {pi})")
        ) %>%
            fct_relevel("rel_error")
    ) %>%
    select(-pi)

scenarios_relerror <-
    modelvar %>%
    inner_join(
        scenarios %>%
            select(-alpha, -pi) %>%
            distinct(),
        join_by(target_variable),
        relationship = "one-to-many",
        unmatched = "error"
    ) %>%
    # the target variable with spatially highest variability drives sample size
    filter(
        stdev_spatial == max(stdev_spatial),
        .by = scheme
    ) %>%
    mutate(scheme = str_glue("{scheme} | {target_variable}")) %>%
    select(scheme, rel_error, scenario)

p <-
    ggplot() +
    geom_line(
        data = errors %>%
            rename(`Type I error` = alpha),
        aes(
            x = n,
            y = value,
            linetype = measure,
            group = measure
        )
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, 0.025),
      labels = scales::label_percent()
    ) +
    facet_grid(
      scheme ~ `Type I error`,
      scales = "free",
      labeller = labeller(`Type I error` = label_both)
    ) +
    geom_hline(
        data = scenarios_relerror,
        colour = "grey60",
        aes(yintercept = rel_error, )
    ) +
    geom_text(
        data = scenarios_relerror %>%
            crossing(`Type I error` = c(0.05, 0.1)) %>%
            mutate(x = ifelse(`Type I error` == 0.05, 300, 150)),
        hjust = 0,
        vjust = -0.5,
        colour = "grey60",
        aes(y = rel_error, label = scenario, x = x)
    ) +
    scale_x_continuous(breaks = seq(0, 2000, 100)) +
    labs(
      x = "Number of locations per typegroup",
      caption = str_c(
          "'minimal' and 'good' refer to the information quality of the relative error:\n",
          "minimal = considered as the minimum quality to be achieved; ",
          "good = considered as good quality"
      ),
      linetype = "Quality measure"
    ) +
    theme(
      legend.position = "top",
      legend.title.position = "top",
      plot.caption = element_text(hjust = 0)
    )

p

ggsave("relerror_and_reldetdiff.png", p, width = 11, height = 10)



