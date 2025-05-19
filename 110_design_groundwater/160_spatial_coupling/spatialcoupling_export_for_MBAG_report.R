
void <- suppressPackageStartupMessages
library("dplyr")     |> void() # our favorite data wrangling toolbox
library("arrow")     |> void() # writing parquet files
library("ggplot2")   |> void() # a visualization toolbox

# to quick-load the meta data
# source("./auxiliary_data_queries.R")

# uniform regression tools and plot helpers
source("./regression_tools.R")

# TODO bring along:
# - regression_tools.R
# - ./data/bootstrapping_dm_*
# - ./cache/regression
# turn into:
# - {r} bootstrap_results["veen", "sigma"]
# - {r} bootstrap_results["heavy", "sigma"]
# - {r} threshold["heavy", "threshold"]
# - {r} bootstrap_results["light", "sigma"]
# - {r} threshold["heavy", "threshold"]
# plus figure


soilclasses <- c("heavy", "light", "peat")
soilclass_colors <- c(
  "heavy" = "sienna",
  "light" = "burlywood",
  "peat" = "darkseagreen",
  "unknown" = "slategray"
)

local_cache_folder <- "cache_" # TODO move this to a shared place or distribute .RData


bootstrap_data <- arrow::read_parquet("./data/bootstrapping_dm_soilclass.parquet")



combine_path <- function(filepath) here::here(local_cache_folder, "regression", filepath)
stitch_filepath <- function(label, sc, reg_var, extension = "parquet") {
  # examples:
  #   label = "allin", "slopefilter", "maaiveld"
  #   sc = "heavy", "light", "peat"
  #   reg_var = "dw", "dm"
  #
  return(paste0(
    sprintf("%s_%s_%s", label, reg_var, sc),
    ".", extension,
    collapse = ""
  ))
}


load_plotdata <- function(
    label = c("slopefilter", "allin", "maaiveld"),
    soilclass = c("heavy", "light", "peat"),
    regression_variable = c("dw", "dm")
  ) {
  data_file <- combine_path(
    stitch_filepath(label, soilclass, regression_variable,
    extension = "parquet")
  )

  return(arrow::read_parquet(data_file))
}


load_fitdata <- function(
    label = c("slopefilter", "allin", "maaiveld"),
    soilclass = c("heavy", "light", "peat"),
    regression_variable = c("dw", "dm")
  ) {
  data_file <- combine_path(
    stitch_filepath(label, soilclass, regression_variable,
    extension = "rds")
  )
  return(readRDS(data_file))
}



data_m <- list(
  "heavy" = load_fitdata("maaiveld", "heavy", "dm"),
  "light" = load_fitdata("maaiveld", "light", "dm"),
  "peat" = load_fitdata("maaiveld", "peat", "dm")
)

get_regression_data <- function(sc) {
  x <- data_m[[sc]]$regx
  y <- data_m[[sc]]$regy
  df <- data.frame(
    "sc" = sc,
    "x" = x,
    "y" = y
  )
  return(df)
}

all_rdata <- bind_rows(lapply(soilclasses, FUN = get_regression_data))
write.csv(all_rdata, "cache/report/regdata.csv")



ref_boots <- bootstrap_data %>%
  filter(i == 0)
sub_boots <- bootstrap_data %>%
  filter(conv == 0, i > 0, if_all(everything(), ~ !is.na(.x)))


# sub_boots %>%
#     ggplot(aes(x = threshold, fill = soilclass)) +
#     geom_histogram(bins = 256, width = 1.05, color = NA, alpha = 0.67, position = "identity") +
#     geom_vline(xintercept = ref_boots$threshold) +
#     xlab("threshold to 1cm difference") +
#     scale_fill_manual(values = soilclass_colors) +
#     scale_color_manual(values = soilclass_colors) +
#     xlim(0, 32) +
#     theme_bw()


threshold_quantiles <- sub_boots %>%
  summarize(
    threshold_q02 = quantile(threshold, c(0.02)),
    threshold_median = quantile(threshold, c(0.50)),
    threshold_q98 = quantile(threshold, c(0.98)),
    sigma_q02 = quantile(sigma, c(0.02)),
    sigma_median = quantile(sigma, c(0.50)),
    sigma_q98 = quantile(sigma, c(0.98)),
    .by = soilclass
  )
# knitr::kable(threshold_quantiles, digits = 1)


write.csv(threshold_quantiles, "cache/report/quantiles.csv")
