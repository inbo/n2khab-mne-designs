
## some uniform colors
# https://r-charts.com/colors/

soilclass_colors <- c(
  "heavy" = "sienna",
  "light" = "burlywood",
  "peat" = "darkseagreen",
  "unknown" = "slategray"
)

ecoregion_colors <- c(
  "zuidoostelijke heuvelzone" = "darkorchid",
  "Kempen" = "sienna",
  "polders en de getijdenschelde" = "slategray",
  "kustduinen" = "burlywood",
  "Pleistocene riviervalleien" = "deepskyblue",
  "zuidwestelijke heuvelzone" = "coral",
  "Midden-Vlaamse overgangsgebieden" = "gold",
  "krijt-leemgebieden" = "honeydew3",
  "cuesta's" = "palevioletred",
  "grindrivieren" = "darkseagreen",
  "westelijke interfluvia" = "hotpink"
)

# wrap a regression function to generate residuals
# the result is the parameter to be minimized.
wrap_target_function <- function(x, y, regressor, params) {
  predictions <- regressor(x, params)
  differences <- y - predictions
  return(sqrt(mean(differences^2)))
}

distweighted_target_function <- function(x, y, regressor, params) {
  predictions <- regressor(x, params)
  differences <- y - predictions
  #differences <- log(1.0+y) - log(1.0+predictions)
  # differences <- 1000*differences * (1+1/x^2)
  # return(sqrt( sum(differences^2)/sum((1+1/x^2)) ))
  differences <- 1000.*differences / sqrt(x)
  return(sqrt( mean(differences^2) ))
}

# this can turn regression output into a usable function.
create_prediction_function <- function(regressor, results) {
  fcn <- function (x) {
    regressor(x, results$par)
  }

  return(fcn)
}



# turns an optimization result of the 4-parameter Matérn
# into a parameter array for a zero-fixed 4-parameter Matérn
#    scale <- parameters[1] # related to the difference parameter
#    sigma <- parameters[2] # related to actual range; turning point
#    nugget <- parameters[3] # nugget (zero intercept)
#    nu <- parameters[4] # shape parameter
shift_nugget_matern4p <- function(orsl){
  par <- orsl$par
  # WRONG: # par[1] <- par[1] - par[3]
  # no need to shift the SCALE down by the nugget
  par[3] <- 0 # no nugget
  # par[4] <- 1. # regular shape
  return(par)
}

# turns an optimization result of the *3-parameter* Matérn
# into a parameter array for a zero-fixed 4-parameter Matérn
shift_nugget_matern3p <- function(orsl){
  par <- orsl$par
  par[3] <- 0 # no nugget
  par[4] <- 1. # regular shape
  return(par)
}


# caclulating the distance at which a given difference threshold is broken,
# slightly adjusting the Matérn outcome.
calculate_limit <- function(
      orsl,
      threshold = 0.01,
      fit_fcn = matern_function,
      prep_fcn = NULL
    ) {

  if (is.null(prep_fcn)) {
    par <- orsl$par
  } else {
    par <- prep_fcn(orsl)
  }

  test_x <- seq(0., par[2], length.out = 1001)
  test_y <- fit_fcn(test_x, par)
  # plot(test_x, test_y)
  exceeds <- which(test_y > threshold)
  if (0 == length(exceeds)) return(NA)
  limit <- test_x[min(exceeds)]
  return(limit)
}



# a uniform way to print results
print_regression_results <- function(orsl, label = "", indicate_threshold = TRUE) {
  par <- paste(round(orsl$par, 4), collapse = ", ")
  conv <- orsl$convergence
  eps <- orsl$value


  print(
    glue::glue("{label}: conv {conv} at ({par}), mse {round(eps, 1)}")
  )

  if (indicate_threshold) {
    threshold <- calculate_limit(
      orsl,
      threshold = 0.01,
      fit_fcn = matern_function,
      prep_fcn = shift_nugget_matern4p
    )
    print(
      glue::glue("==> Threshold of dw > 1cm reached at {round(threshold, 3)} m distance.")
    )

  } else {
    print(glue::glue("==> Sigma range is {round(orsl$par[2], 1)} m."))
  }
}

# note: the `epsilon` can be calculated manually with the formula:
#   sum((predictor_function(x) - y)^2)


# Finally, a quick histogram plot of residuals.
plot_residuals_histogram <- function (x, y, predictor_function, ...) {
  residuals <- predictor_function(x) - y
  ggplot(NULL, aes(x = residuals)) +
    geom_histogram(...) +
    geom_vline(xintercept = 0, color = "darkgrey") +
    theme_bw()
}






gauss_function <- function(x, parameters) {
  scale <- parameters[1] # height
  mu <- 0 # mean, always zero here
  sigma <- parameters[2] # standard deviation
  nugget <- parameters[3] # total height

  # This parametrization ensures that nugget < sill if scale > 0
  sill <- scale + nugget

  # the raw, unscaled gaussian kernel
  gauss <- exp(-((x-mu)^2)/(2*sigma^2))

  # the bell should point downwards:
  result <- sill - scale * gauss

  return(result)
}



matern_function <- function(d, parameters) {
  scale <- parameters[1] # related to semivariance
  sigma <- parameters[2] # related to actual range; turning point
  nugget <- parameters[3]
  nu <- parameters[4]

  sill <- scale + nugget
  z <- sqrt(2 * nu) * d / sigma
  K <- suppressWarnings(besselK(z, nu))
  matern <- (z)^nu * K / (2^(nu - 1) * gamma(nu))
  result <- sill - scale * matern

  result[is.na(result)] <- 0
  return(result)
}



#_______________________________________________________________________________
# Matern-specific

fit_matern <- function(x, y, distweighted = FALSE, ...) {
  target_fcn <- wrap_target_function
  if (distweighted) {
    target_fcn <- distweighted_target_function
  }
  optimizer_results <- optim(
    fn = function(params) {
      target_fcn(x, y, matern_function, params)
    },
    ...
  )

  # store everything in one list;
  # do I sense a smidgen of OOP here? No, not really.
  optimizer_results$fcn <- matern_function
  optimizer_results$regx <- x
  optimizer_results$regy <- y
  optimizer_results$regn <- length(y)

  # wrap the function with the optimized parameters
  optimizer_results$predict <- create_prediction_function(
    matern_function,
    optimizer_results
  )


  return(optimizer_results)
}


plot_matern <- function(optimizer_results,
                        y_label = "mean absolute difference (m)",
                        color = "darkgrey") {

  # retrieve everything
  fcn <- optimizer_results$fcn
  regx <- optimizer_results$regx
  regy <- optimizer_results$regy
  predict <- optimizer_results$predict

  # extract parameters
  scale <- optimizer_results$par[1]
  range <- optimizer_results$par[2]
  nugget <- optimizer_results$par[3]
  sill <- scale + nugget


  # plotting
  plotx <- regx # seq(0, extent, length.out = 2*extent + 1)
  plotx <- plotx[plotx>0]

  g <- ggplot(NULL, aes(x = regx, y = regy)) +
    geom_vline(xintercept = range, color = "grey") +
    geom_hline(yintercept = nugget, color = "grey") +
    geom_hline(yintercept = sill, color = "grey") +
    geom_point(size = 2.5, colour = color, fill = "white", alpha = 0.2) +
    geom_line(aes(x = plotx, y = predict(plotx)),
              color = color, linewidth = 1.2) +
    labs(title = paste0("Matérn regression: ",
      paste(round(optimizer_results$par, 4), collapse = ", "))) +
    xlab("distance (m)") + ylab(y_label) +
    theme_minimal()

  return(g)
}


trafo <- function(x) x

regression_by_soilclass <- function(
    regression_data,
    sc, reg_var,
    maxx = extent, maxy = NULL,
    label = "",
    return_fit = FALSE,
    ...) {


  stopifnot("arrow" = require("arrow"),
            "dplyr" = require("dplyr"))

  diff_sc <- regression_data %>% filter(soilclass == sc)

  diff_sc <- diff_sc %>%
    filter(ds > 0, ds < maxx) %>%
    select(ds, !!reg_var)


  regression_storage_path <- here::here("cache", "regression")
  sink_file <- glue::glue("{label}_{reg_var}_{sc}.parquet")
  write_parquet(diff_sc, sink = here::here(regression_storage_path, sink_file))

  x <- diff_sc$ds
  y <- trafo(diff_sc %>% pull(!!reg_var))

  y <- y[x > 0]
  x <- x[x > 0]
  y <- y[x <= maxx]
  x <- x[x <= maxx]

  if (is.null(maxy)) maxy <- max(y)
  x <- x[y <= maxy]
  y <- y[y <= maxy]

  x <- x[!is.na(y)]
  y <- y[!is.na(y)]

  # standardization helps function comprehension
  # x <- x/max(x)
  # y <- y / mean(y)


  matern_fit <- fit_matern(x, y,
    method = "L-BFGS-B", # "Nelder-Mead" # "L-BFGS-B"
    ...
  )

  if (return_fit) return(matern_fit)

  sink_file <- glue::glue("{label}_{reg_var}_{sc}.rds")
  print(sink_file)
  saveRDS(matern_fit, file = here::here(regression_storage_path, sink_file))

  sink_file <- glue::glue("{label}_{reg_var}_{sc}_binned_observations.csv")
  regdat <- extract_input_data_table(matern_fit)
  write.csv2(regdat, file = here::here(regression_storage_path, sink_file))

  sink_file <- glue::glue("{label}_{reg_var}_{sc}_regression_values.csv")
  regresult <- extract_data_table_matern_regression(matern_fit)
  write.csv2(regresult, file = here::here(regression_storage_path, sink_file))


  print_regression_results(matern_fit, label = "regression:")

  ylabel <- "mean water level difference (pair-averaged)"
  sc_color <- soilclass_colors[sc]
  # if ("semivar" reg_var) ylabel <- "semivariance (non-pair-averaged)"

  g <- plot_matern(matern_fit, y_label = ylabel, color = sc_color)

  x_excl <- diff_sc$ds
  y_excl <- trafo(diff_sc %>% pull(!!reg_var))

  y_excl <- y_excl[x_excl <= maxx]
  x_excl <- x_excl[x_excl <= maxx]
  x_excl <- x_excl[y_excl > maxy]
  y_excl <- y_excl[y_excl > maxy]
  g <- g + geom_point(aes(x = x_excl, y = y_excl),
      size = 2.5, colour = "red", alpha = 0.2)
  return(g)
}




add_regression_to_plot <- function(
      h,
      optimizer_results,
      color = "black",
      skip_threshold = FALSE
    ) {

  # optimizer_results <- reference

  # retrieve everything
  fcn <- optimizer_results$fcn
  regx <- optimizer_results$regx
  regy <- optimizer_results$regy
  predict <- optimizer_results$predict

  # extract parameters
  scale <- optimizer_results$par[1]
  range <- optimizer_results$par[2]
  nugget <- optimizer_results$par[3]
  sill <- scale + nugget
  threshold <- calculate_limit(
    optimizer_results,
    threshold = 0.01,
    prep_fcn = shift_nugget_matern4p
  )

  if (skip_threshold) {
    threshold = NA
  }

  # plotting
  maxx <- max(regx)
  plotx <- seq(0, maxx, length.out = maxx + 1)[]
  # plotx <- regx # seq(0, extent, length.out = 2*extent + 1)
  plotx <- plotx[plotx>0]

  h <- h +
    geom_vline(xintercept = threshold, color = color, alpha = 1.0) +
    geom_vline(xintercept = range, color = color, alpha = 1.0) +
    geom_hline(yintercept = nugget, color = color, alpha = 1.0) +
    geom_hline(yintercept = sill, color = color, alpha = 1.0) +
    geom_point(aes(x = regx, y = regy),
      size = 2.5, color = color, alpha = 0.6) +
    geom_line(aes(x = plotx, y = predict(plotx)),
              color = color, linewidth = 1.2)

  return(h)

}



extract_input_data_table <- function(regression, units = "m") {
  data.frame(
    regression$regx,
    regression$regy
  ) %>%
  magrittr::set_colnames(c(
    glue::glue("mean bin distance (m)"),
    glue::glue("mean abs difference ({units})")
  )) %>%
  return()
}

extract_data_table_matern_regression <- function(regression, units = "m") {
  matern_parameters <- regression$par
  # message(max(regression$regx))

  scale  <- matern_parameters[1] # related to semivariance
  sigma  <- matern_parameters[2] # related to actual range; turning point
  nugget <- matern_parameters[3] # variance at zero distance
  nu     <- matern_parameters[4] # shape parameter; here: soft-fixed to ~1
  sill   <- scale + nugget

  distance_m <- seq(0., 256., length.out = 257)
  distance_m <- distance_m[distance_m > 0]
  mean_abs_difference_m <- matern_function(distance_m, matern_parameters)
  difference_nuggetless_m <- mean_abs_difference_m - nugget
  difference_percentage <- 100 * difference_nuggetless_m / scale

  data.frame(
    distance_m,
    mean_abs_difference_m,
    difference_nuggetless_m,
    difference_percentage
  ) %>%
  magrittr::set_colnames(c(
    glue::glue("distance (m)"),
    glue::glue("mean abs difference ({units})"),
    glue::glue("mean abs difference *ex nugget* ({units})"),
    glue::glue("difference percentage")
  )) %>%
  return()
}
