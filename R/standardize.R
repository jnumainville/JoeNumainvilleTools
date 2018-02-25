#' standardize calculates (x - mean(x))/sd(x) for each column in a given matrix
#'
#' @param a the matrix
#' @return the standardized matrix
#' @examples \dontrun{
#'  standardize(x)
#'}
#' @export
standardize <- function(a) {
  stopifnot(nrow(a) > 1)
  stopifnot(is.numeric(a))
  stopifnot(all(is.finite(a)))
  f <- function(x) {
    x = (x - mean(x))/sd(x)
  }
  apply(a, 2, f)
}
