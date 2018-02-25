#' This function is used to estimate where the maximum is given a function
#'
#' @param x the entries
#' @param f the function to optimize
#' @param interval the interval to optimize over
#' @return filtered data
#' @examples \dontrun{
#'  opt(x, f, c(-1,1))
#'}
#' @export
opt <- function(x, f, interval) {
  oout <- optimize(f, maximum = TRUE, interval, x = x)
  oout$maximum
}
