#' mle calculates the maximum likelihood estimate for the gamma distribution
#'
#' @param x the numerical vector to optimize over
#' @return the value of the distribution that gives the highest value
#' @examples \dontrun{
#'  mle(x)
#'}
#' @export
mle <- function(x) {
  oout = optimize(function(alpha) f = sum(dgamma(x, shape = alpha, log = TRUE)), maximum = TRUE, pmax(mean(x) / 1e3, mean(x) + c(-1, 1) * 3 * sd(x)))
  oout$maximum
}
