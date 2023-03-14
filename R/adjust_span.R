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
  # x = variable
  # y = value
  # span = 0.2
  fit <- suppressWarnings(loess(log10(y+0.0000001) ~ x, span = span))
  if (inherits(fit, "try-error")) {
    # If loess() returns an error, try again with a smaller span
    adjust_span(x, y, span/2)
  } else if (inherits(fit, "warning")) {
    # If loess() issues a warning, try again with a larger span
    adjust_span(x, y, span + 0.1)
  } else {
    # If there are no errors or warnings, return the fitted object
    return(fit)
  }
}
