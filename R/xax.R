#' xax calculates the transverse of matrix x multiplied by the inverse of A multiplied
#'
#' @param x numerical vector
#' @param a numerical vector
#' @return 1 x 1 matrix containing the operation result
#' @examples \dontrun{
#'  xax(a,x)
#'}
#' @export
xax <- function(a, x) {

  stopifnot(is.numeric(x));
  stopifnot(is.numeric(a));
  stopifnot(all(is.finite(x)))
  stopifnot(all(is.finite(a)))
  stopifnot(dim(a)[1] == dim(a)[2])
  #since matrix(x) is n x 1, a needs to be n x n
  stopifnot(nrow(matrix(x)) == nrow(a))
  stopifnot(nrow(matrix(x)) == ncol(a))



  t(matrix(x)) %*% solve(a) %*% matrix(x)
}
