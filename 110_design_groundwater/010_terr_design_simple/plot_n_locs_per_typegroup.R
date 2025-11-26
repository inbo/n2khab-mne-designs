# Code to generate a plot with the number of locations per typegroup

# First, run index.Rmd at least until the definition of the function
# give_me_some_samplesize()

minqual_extended3 <-
    expand.grid(target_variable = c("HG3", "LG3"),
                alpha = seq(0.05, 0.1, 0.05),
                relative_errormargin = seq(0.05, 0.1, 0.05)) %>%
    rbind(
        expand.grid(target_variable = c("N-NH4", "N-NO3", "P-PO4"),
                    alpha = seq(0.05, 0.1, 0.05),
                    relative_errormargin = seq(0.1, 0.15, 0.05))
    ) %>%
    mutate(logscale_errormargin = log10(1 + relative_errormargin),
           var_mu = (logscale_errormargin / qnorm(1 - alpha/2))^2) %>%
    as_tibble()
var_spat_extended3 <-
    modelvar %>%
    mutate(n_types = 1) %>%
    select(-stdev_spatial) %>%
    as_tibble()
some_scenarios3 <-
    give_me_some_samplesize(minqual = minqual_extended3,
                            var_spat = var_spat_extended3,
                            n_years = 6)

p <-
    some_scenarios3 %>%
    mutate(
        n_locs = ifelse(str_detect(target_variable, "G3"),
                        n_locs_per_year,
                        n_locs_per_year * 6) %>%
            ceiling(),
        alpha = factor(alpha),
        rel_error_label = str_c("rel_error = ", relative_errormargin * 100, "%") %>% fct()
    ) %>%
    ggplot(aes(x = target_variable, y = n_locs, fill = alpha, label = n_locs)) +
    geom_col(position = "dodge") +
    geom_text(
        size = 3,
        vjust = -0.3,
        position = position_dodge(width = 0.9)
    ) +
    scale_y_continuous(breaks = seq(0, 1500, 250)) +
    scale_fill_viridis_d(begin = 0.6, end = 0.8) +
    facet_wrap(~ rel_error_label, nrow = 1) +
    labs(
        y = "Number of locations per typegroup",
        fill = "Type I error"
    )

p

ggsave("n_locs_per_typegroup.png", p, width = 11, height = 6)

## minimum detectable difference (expressed relative to a reference value), in a
## one-sided test for a relative error margin of 15%, type I error = 0.1 and
## power = 0.8

# coefficient to multiply the error margin in the log scale, to achieve the MDF
qnorm(0.8)/qnorm(0.9) + 1 # 1.66
# relative MDF
10^(log10(1.15)*1.66) - 1
