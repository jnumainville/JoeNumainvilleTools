#' weightedSumm returns the weighted mean, variance, and standard deviation of a numerical matrix in a list
#'
#' @param x the matrix
#' @return the weighted mean, variance, and standard deviation of x in a list
#' @examples \dontrun{
#'  weightedSumm(x)
#'}
#' @export
weightedSumm <- function(x, p) {

  stopifnot(length(x) > 0)
  stopifnot(length(p) > 0)
  stopifnot(is.numeric(x))
  stopifnot(is.numeric(p))
  stopifnot(length(x) == length(p))
  # These to ensure that every element is finite
  stopifnot(all(is.finite(x)))
  stopifnot(all(is.finite(p)))
  # Checks if the sum of p is very close to 1
  stopifnot(all.equal(sum(p),1))

  mean = sum(p * x)
  var = sum(((x - mean) ^ 2) * p)
  sd = sqrt(var)
  list(mean, var, sd)
}
