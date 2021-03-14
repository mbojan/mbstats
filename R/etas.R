#' Eta coefficients
#'
#' Calculate Eta coefficients, known as "Correlation ratios". Squared value of
#' Eta has an interpretation in terms of the proportion of explained variance.
#' The interpretation follows from a problem of predicting values of \eqn{y}.
#' The value of the \eqn{\eta^2}{Eta^2} is the proportion by which the error of
#' predicting values of \eqn{y} is reduced by using the information provided by
#' \eqn{x}.
#'
#' @return Values of eta and partial eta coefficients.
#'
#' @param object the R object
#' @param ... arguments passed to other methods
#'
#' @export

etas <- function( object, ... ) UseMethod("etas")



#' @describeIn etas The default method expect vectors. The function requires
#'   additional argument `fac` -- a vector of the same length as `object`. The
#'   result is a value of the \eqn{\eta^2}{Eta^2} assuming that we want to
#'   predict the values of `object` with the values of `fac` using the so called
#'   "Type I regression of means". For two variables \eqn{y} and \eqn{x} the
#'   \eqn{\eta}{Eta} is given by the formula:
#'
#'   \deqn{\eta^2 = ( D^2(y) - E[D^2(y|x)] ) / D^2(y)}{Eta^2 = ( D^2(y) - E[D^2(y|x)] ) / D^2(y)}
#'
#' @param fac vector for conditioning variable
#'
#' @export
#'
#' @examples
#' ### Generate some data
#' x1 <- rnorm(50)
#' x2 <- rnorm(50)
#' y <- 5 + 2*x1 + rnorm(50,0,2) + 3*x2 + rnorm(50,0,.5)
#'
#' ### Method for vectors
#' etas( y, rep(1:2, each=25) )
etas.default <- function( object, fac, ... ) {
	n <- length(object)
	if( length(fac) != n )
		stop("arguments must be of the same length")
	( stats::var(object) - mean(tapply(object, fac, FUN=stats::var)) ) / stats::var(object)
}



#' @describeIn etas For objects of class `anova` the function calculates the
#'   Eta's and Partial Eta Squares for all effects in the given model. In this
#'   setting the eta squares for the given effect are equal to:
#'
#'   \deqn{\frac{SS_{effect}}{SS_{total}}}{SSeffect / SStotal}
#'
#'   where \eqn{SS} are apropriate Sums of Squares.  The ``Partial Eta Squares''
#'   for the given effect are equal to:
#'
#'   \deqn{\frac{SS_{effect}}{SS_{effect}+SS_{resid}}}{SSeffect / (SSeffect+SSresid)}
#'
#' @export
etas.anova <- function(object, ...) {
	nam <- row.names(object["Sum Sq"])
	ss <- object[["Sum Sq"]]
	sstotal <- sum(ss)
	ssresid <- ss[nam=="Residuals"]
	ss <- ss[-length(ss)]
	rval <- data.frame( ss/sstotal, ss/(ss+ssresid) )
	structure( rval,	names=c("Eta", "Partial Eta"),
				row.names=nam[-length(nam)] )
}




#' @describeIn etas For objects of class `lm` the function is applied on the
#' result of calling [anova()].
#'
#' @export
#'
#' @examples
#'
#' ### Method for 'lm' which calls 'anova'
#' m <- lm( y ~ x1 + x2 )
#' etas(m)

etas.lm <- function(object, ...) etas.anova(stats::anova(object))
