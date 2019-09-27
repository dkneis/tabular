#' Check not-null constraints
#'
#' Check for violation of a not-null constraint
#'
#' @param x Data frame representing a table of a data base.
#' @param cols Names of the colum(s) of \code{x} which are to be scanned for
#'   missing values (vector of type character).
#' @param silent Logical. If \code{FALSE}, details on constraint
#'  violations are shown using \code{\link[base]{print}}.
#' 
#' @return \code{TRUE} if the check was passed successfully and
#'   \code{FALSE} otherwise.
#'
#' @note The function tests for the occurrence of \code{NA} in any of the
#'   columns of \code{x} specified in \code{cols}. Hence, it is assumed that
#'   missing values are marked as \code{NA} (instead of \code{NULL} like in
#'   a true SQL data base).
#'
#' @seealso There are more functions to check constraints, namely
#'   \code{\link{check.unique}}, \code{\link{check.key}},
#'   and \code{\link{check.link}}.
#'   See also the example for \code{\link{db.read}}.
#'   
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' data(temperature)
#' print(temperature)
#' 
#' # Should succeed
#' check.notnull(temperature, c("day", "city"))
#' 
#' # Example of NA
#' temperature[2, "day"] <- NA
#' print(temperature)
#' check.notnull(temperature, c("day", "city"))

check.notnull <- function(x, cols, silent=FALSE) {
  if (!is.data.frame(x))
    stop("'x' must be a data frame")
  if (!is.character(cols) || (length(cols) < 1))
    stop("'cols' must be a vector of column names (character)")
  bad <- cols[!cols %in% colnames(x)]
  if (length(bad) > 0)
    stop(paste0("column(s) specified as 'cols' not found in 'x': '",
        paste(bad, collapse=", "),"'"))

  i <- which(sapply(x[,cols,drop=FALSE], function(z) {any(is.na(z))}))
  if (length(i) > 0) {
    if (!silent)
      print(paste0("data in 'x' violate not-null constraint with regard ",
        "to the column(s) '",paste(colnames(x)[i], collapse="', '"),"'"))
    return(FALSE)
  }
  return(TRUE)
}
