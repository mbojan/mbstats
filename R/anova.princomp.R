#' Component variation in PCA analysis
#'
#' Calculate percent of explained variance for Principal Component Analysis
#' objects.
#'
#' @param object object of class `princomp`
#' @param ... other arguments currently not supported
#'
#' @details The function calculates percentages of variance explained for each
#'   component resulting from Principal Component Analysis. The variance is
#'   equal to: \deqn{100\frac{\sigma_i^2}{\sum_i \sigma_i^2}}{sigma^2 / sum(
#'   sigma^2 ) * 100} where \eqn{\sigma}{sigma} is equal to component's standard
#'   deviation.
#'
#' @return A vector of length equal to a number of components existing in
#'   `object`. Each element contains the percent of variance explained by
#'   the corresponding component.
#'
#' @seealso [stats::princomp()], [stats::anova()]
#'
#' @export
#' @method anova princomp
#'
#' @examples
#' pcmodel <- princomp(USArrests, cor=TRUE)
#' pcmodel
#' anova(pcmodel) # variance prc.

anova.princomp <- function(object, ...) {
	object$sdev^2 / sum(object$sdev^2) * 100
}
