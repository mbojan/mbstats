#' Rescale vector to given mean and SD
#'
#' @param x numeric; values to be rescaled
#' @param mu,sigma numeric; target mean and standard deviation
#' @param na.rm logical; passed to [mean()] and [sd()]
#'
#' @return Numeric vector of the same length as `x` of the rescaled values.
#'
#' @seealso [scale()] and [scales::rescale()] for rescaling to new
#'   minimum and maximum.
#'
#' @export
#'
#' @examples
#' x <- rescale_to(rnorm(20), 100, 50)
#' mean(x); sd(x)

rescale_to <- function(x, mu, s, na.rm=FALSE) {
  stopifnot(is.numeric(x))
  s / sd(x, na.rm=na.rm) * x + (mu - s/sd(x, na.rm=na.rm) * mean(x, na.rm=na.rm))
}
