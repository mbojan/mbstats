#' Hellwig's method for choosing an optimal subset of predictors
#'
#' Hellwig's method selects a subset of predictors in a linear model such that
#' they are correlated with the response but relatively uncorrelated among each
#' other.
#'
#' @param y numeric, response variable
#' @param x numeric matrix of predictors
#' @param method character, type of correlation measures used, passed to [cor()]
#'
#' @details Given \eqn{m} predictors Hellwig's method consists of evaluating all
#'   \eqn{2^m - 1} combinations using the following steps:
#'
#'   1. Individual capacity of a predictor variable in a subset is given by:
#'   \deqn{h_{kj} = r_{0j}^2 / \sum_{i \in I} r_{ij}}{h_kj = r_0j^2 / sum_{i \in I} r_ij}
#'   where \eqn{r_{0j}}{r_0j} is correlation of j-th predictor with the
#'   response variable, \eqn{r_{ij}}{r_ij} is a correlation i-th and j-th
#'   predictors, and I is the set of predictors under consideration.
#'
#'   2. Integral capacity of information for every combination \eqn{k} is equal
#'   to: \deqn{H_k = \sum_j h_{kj}}{H_k = sum_j h_kj}
#'
#'   The subset with the highest value of \eqn{H_k} should be selected.
#'
#'
#' @return Data frame with two columns:
#'
#'   - `k` -- combination of predictor variables in the form of x-y-z where x,
#'   y, z... are the indices of columns in \code{x}, and
#'   - `h` -- the capacity of the subset \eqn{H_k}.
#'
#' @export
#'
#' @examples
#' set.seed(1234)
#' x <- matrix(rnorm(1000), 250, 4)
#' y <- rnorm(250)
#' hellwig(y, x)

hellwig <- function( y, x, method="pearson") {
  requireNamespace("utils")
  x <- as.data.frame(x)
  cm <- stats::cor(x, method=method) # correlation matrix among indeps
  cd <- stats::cor(x, y, method=method) # correlations with dependent
  # list of combination vectors
  k <- sapply( seq(2, length(x)), function(i)
              utils::combn(length(x), i, simplify=FALSE) )
  k <- do.call("c", k)
  # function calculating individual capacities
  hfun <- function(v)
  {
    sapply(v, function(i) cd[i]^2 / sum(abs(cm[v,i])) )
  }
  h <- sapply(k, hfun)
  data.frame( k = sapply( k, paste, collapse="-"),
             h = sapply(h, sum),
             stringsAsFactors=FALSE)
}
