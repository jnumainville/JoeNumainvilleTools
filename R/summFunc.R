#' summFunc returns the mean, variance, and standard deviation of a numerical matrix in a list
#'
#' @param x the matrix
#' @return the mean, variance, and standard deviation of x in a list
#' @examples \dontrun{
#'  summFunc(x)
#'}
#' @export
summFunc <- function(x) {
  stopifnot(is.numeric(x))
  stopifnot(all(is.finite(x)))
  stopifnot(length(x) > 0)
  mean = sum(x) / length(x)
  var = sum((x - mean)^2)/length(x)
  sd = sqrt(var)
  ls = list(mean,var,sd)
  return(ls)
}
