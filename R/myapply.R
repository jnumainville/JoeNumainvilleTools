#' myapply is a modified version of the apply function, but it only works on margin = 1 and 2
#'
#' @param X the array to apply the function over
#' @param MARGIN 1 for rows and 2 for columns
#' @param FUN the function to apply over
#' @return the modified array
#' @examples \dontrun{
#'  myapply(x, 1, mean)
#'}
#' @export
myapply <- function(X, MARGIN, FUN,...) {
  FUN <- match.fun(FUN)
  stopifnot(MARGIN %in% c(1,2));
  stopifnot(length(dim(X)) == 2);

  nRow = dim(X)[1]
  nCol = dim(X)[2]

  result = list()

  if(MARGIN == 1) {
    for (i in 1:nRow) {
      result[[i]] = FUN(X[i,],...)
    }
  }
  else if (MARGIN == 2) {
    for (j in 1:nCol) {
      result[[j]] = FUN(X[,j],...)
    }
  }
  simplify2array(result)
}
