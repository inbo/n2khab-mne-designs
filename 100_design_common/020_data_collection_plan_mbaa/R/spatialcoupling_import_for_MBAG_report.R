void <- suppressPackageStartupMessages
library("dplyr")     |> void() # our favorite data wrangling toolbox
library("ggplot2")   |> void() # a visualization toolbox
library("here")      |> void() # relative paths


#### load data
all_regdata <- read.csv("data/spatialcoupling/regdata.csv")
bootstrap_quantiles <- read.csv("data/spatialcoupling/quantiles.csv")


#### text report
# required output:
# - {r} bootstrap_results("peat", "sigma")
# - {r} bootstrap_results("heavy", "sigma")
# - {r} threshold("heavy", "threshold")
# - {r} bootstrap_results("light", "sigma")
# - {r} threshold("heavy", "threshold")

print_bootstrap_result <- function(sc, param) {
  val_min <- bootstrap_quantiles %>%
    filter(soilclass == sc) %>%
    pull(sprintf("%s_q02", param))
  val_mid <- bootstrap_quantiles %>%
    filter(soilclass == sc) %>%
    pull(sprintf("%s_median", param))
  val_max <- bootstrap_quantiles %>%
    filter(soilclass == sc) %>%
    pull(sprintf("%s_q98", param))
  return(
    sprintf(
      "%.1f [%.1f, %.1f]",
      val_mid, val_min, val_max
    )
  )
}
# print_bootstrap_result("heavy", "threshold")



#### Regression Machinery

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




# fit with Matérn function
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



#### plot
soilclasses <- c("heavy", "light", "peat")
soilclass_colors <- c(
  "heavy" = "sienna",
  "light" = "burlywood",
  "peat" = "darkseagreen",
  "unknown" = "slategray"
)



add_regression_to_plot <- function(
    h, optimizer_results, color = "black", maxx = Inf
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


  # plotting
  plotx <- seq(0, maxx, length.out = maxx + 1)
  plotx <- plotx[plotx > 0]
  plotx <- plotx[plotx < maxx]

  h <- h +
    geom_vline(xintercept = threshold, color = color, alpha = 1.0) +
    geom_vline(xintercept = range, color = color, alpha = 1.0) +
    geom_hline(yintercept = nugget, color = color, alpha = 1.0) +
    geom_hline(yintercept = sill, color = color, alpha = 1.0) +
    geom_point(aes(x = regx[regx <= maxx], y = regy[regx <= maxx]),
      size = 2.5, color = color, alpha = 0.6) +
    geom_line(aes(x = plotx, y = predict(plotx)),
              color = color, linewidth = 1.2)

  return(h)

}


plot_regressions <- function() {
  g <- ggplot(NULL)
  for (sc1 in soilclasses) {
    regression_data <- all_regdata %>%
      filter(sc == sc1) %>% select(x, y)
    regression_result <- fit_matern(
      x = regression_data$x,
      y = regression_data$y,
      distweighted = TRUE,
      par = c(1., 10, 0., 1.),
      lower = c(0.0, 0.0, 0., 0.99),
      upper = c(4., 480, 0.48, 1.01),
      method = "L-BFGS-B"
    )
    suppressWarnings(
      g <- add_regression_to_plot(
        g,
        regression_result,
        color = soilclass_colors[sc1],
        maxx = 256
      )
    )
  }
  g + xlab("distance (m)") + ylab("mean absolute difference (mMaaiveld)") +
    ylim(0, 0.6) +
    theme_bw()

}

if (FALSE) {
  plot_regressions()
}
