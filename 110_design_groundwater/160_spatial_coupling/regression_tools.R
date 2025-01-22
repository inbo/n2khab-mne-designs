



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
  differences <- 1000*differences /sqrt(x)
  return(sqrt( mean(differences^2) ))
}

# this can turn regression output into a usable function.
create_prediction_function <- function(regressor, results) {
  fcn <- function (x) {
    regressor(x, results$par)
  }

  return(fcn)
}

calculate_limit <- function(orsl, threshold = 0.01) {
  par <- orsl$par
  par[3] <- 0 # no nugget
  # par[4] <- 1. # regular shape
  test_x <- seq(0., par[2], length.out = 1000)
  test_y <- matern_function(test_x, par)
  limit <- test_x[min(which(test_y > threshold))]
  return(limit)
}

# a uniform way to print results
print_regression_results <- function(orsl, label = "") {
  par <- paste(round(orsl$par, 4), collapse = ", ")
  conv <- orsl$convergence
  eps <- orsl$value

  threshold <- calculate_limit(orsl)

  print(
    sprintf("%s: conv %i at (%s), mse %.1f", label, conv, par, eps)
  )
  print(
    sprintf("==> Threshold of dw > 1cm reached at %.3f m distance.", threshold)

  )
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
