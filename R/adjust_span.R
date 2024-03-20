#' Recursively calculates the span for a LOESS curve.
#'
#' In general, the distribution and quantity of data influences the value of the LOESS's span.
#' To avoid manipulation of the span by the user, its value is computed recursively from an initial value of 0.2.
#'
#' @name adjust_span
#' @param x numeric data from a dataframe for which we want to produce a LOESS curve
#' @param y numeric data from a dataframe for which we want to produce a LOESS curve
#' @param span a double representing the desired span to compute the LOESS curves
#' @export adjust_span

adjust_span <- function(x, y, span) {
  # Check for NaN or Inf values in x and y
  if (any(is.nan(x)) || any(is.infinite(x)) || any(is.nan(y)) || any(is.infinite(y))) {
    stop("NaN or Inf values found in x or y (adjust_span()).")
  }

  fit <- suppressWarnings(loess(log10(y+0.0000001) ~ x, span = span))

  # Check for NA values in specific components of the loess object
  components <- c("enp", "s", "one.delta", "two.delta", "trace.hat", "divisor", "robust", "pars", "kd")
  if (any(is.na(fit[components])) || inherits(fit, "warning")) {
    # If any component contains NA values, try again with a larger span
    if (span < 1) {
      adjust_span(x, y, span + 0.1)
    } else {
      stop("Failed to adjust span. Consider changing the initial span value.")
    }
  } else {
    # If there are no errors or warnings, return the fitted object
    return(fit)
  }
}
