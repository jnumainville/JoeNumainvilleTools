#' ggplot_wrapper graphs a function using ggplot2
#'
#' @param dat the data
#' @param x the x value for ggplot2
#' @param y the y value for ggplot2
#' @param color optional color value for ggplot2
#' @examples \dontrun{
#'  ggplot_wrapper(data, data$x, data$y, "red")
#'}
#' @export
ggplot_wrapper <- function(dat, x, y, color) {
  if (missing(color)) {
      ggplot(data = dat) + geom_point(mapping = aes(x = x, y = y))
  }
  else {
      ggplot(data = dat) + geom_point(mapping = aes(x = x, y = y, color = color))
  }
}
