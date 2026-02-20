# Code to generate a plot with the relative errormargin & relative detectable
# difference obtained for a given number of locations per typegroup

library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

# chunk copied from index.Rmd, containing the spatial variance (log-scale):
modelvar <-
    tibble(target_variable = c("HG3",
                               "LG3",
                               "N-NH4",
                               "N-NO3",
                               "P-PO4"),
           stdev_spatial = c(0.4305,
                             0.34035,
                             0.57910,
                             0.6568,
                             0.49131)
    ) %>%
    mutate(var_spatial = stdev_spatial^2)

# criteria, not plotted at the moment:
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
    )

# graph of relative errormargin & relative detectable difference:
p <-
    modelvar %>%
    crossing(n = 150:500) %>%
    mutate(
        stdev_mu = stdev_spatial / sqrt(n),
        log_error = qnorm(1 - 0.1 / 2) * stdev_mu,
        rel_error = 10^log_error - 1,
        # detectable difference for one-sided testing (alpha = 0.1, pi = 0.8):
        log_detectable_diff = qnorm(1 - 0.1) * stdev_mu + qnorm(0.8) * stdev_mu,
        rel_detectable_diff = 10^log_detectable_diff - 1
    ) %>%
    select(target_variable, n, starts_with("rel_")) %>%
    pivot_longer(
      cols = starts_with("rel_"),
      names_to = "measure",
      values_to = "value"
    ) %>%
    mutate(measure = fct_rev(measure)) %>%
    ggplot(aes(
      x = n,
      y = value,
      colour = target_variable,
      group = target_variable
    )) +
    geom_line() +
    scale_y_continuous(
      breaks = seq(0, 1, 0.025),
      labels = scales::label_percent()
    ) +
    facet_wrap(~measure)

p

ggsave("relerror_and_reldetdiff.png", p, width = 11, height = 8)



