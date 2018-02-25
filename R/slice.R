#' slice filters a specific column so all entries match a certain value
#'
#' @param dat the data
#' @param col the column to filter
#' @param filtWith what the value should equal
#' @return filtered data
#' @examples \dontrun{
#'  slice(data, data$height, 6)
#'}
#' @export
slice <- function(dat, col, filtWith) {
  dplyr::filter(dat, col == filtWith)
}
