
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


soilclasses <- c("heavy", "light", "peat")
soilclass_colors <- c(
  "heavy" = "sienna",
  "light" = "burlywood",
  "peat" = "darkseagreen",
  "unknown" = "slategray"
)


local_cache_folder <- "cache"
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


# g <- ggplot(NULL)
# for (sc in names(data_m)) {
#   regression_result <- data_m[[sc]]
#   g <- add_regression_to_plot(g, regression_result, color = soilclass_colors[sc])
# }
# g + xlab("distance (m)") + ylab("mean absolute difference (mMaaiveld)") +
#   ylim(0, 1.6) +
#   theme_minimal()
#
#
# knitr::kable(
#     t(sapply(names(data_m),
#       FUN = function(sc) calculate_limit(data_m[[sc]])
#     )),
#     digits = 1
#   )


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
    q02 = quantile(threshold, c(0.02)),
    median = quantile(threshold, c(0.50)),
    q98 = quantile(threshold, c(0.98)),
    .by = soilclass
  )
# knitr::kable(threshold_quantiles, digits = 1)
