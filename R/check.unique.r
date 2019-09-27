#' Check unique constraints
#'
#' Check for violation of a unique constraint
#'
#' @param x Data frame representing a table of a data base.
#' @param cols Names of the colum(s) of \code{x} to which the constraint
#'   applies (vector of type character). See notes.
#' @param silent Logical. If \code{FALSE}, details on constraint
#'  violations are shown using \code{\link[base]{print}}.
#' 
#' @return \code{TRUE} if the check was passed successfully and
#'   \code{FALSE} otherwise.
#'
#' @note If \code{cols} has length > 1, uniqueness is checked for the
#'   combination of columns rather than for each column individually. See
#'   the examples.
#'
#' @seealso There are more functions to check constraints, namely
#'   \code{\link{check.notnull}}, \code{\link{check.key}},
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
#' check.unique(temperature, c("day", "city"))
#' 
#' # Example of duplicates
#' temperature[2, "day"] <- 1
#' print(temperature)
#' check.unique(temperature, c("day", "city"))

check.unique <- function(x, cols, silent=FALSE) {
  if (!is.data.frame(x))
    stop("'x' must be a data frame")
  if (!is.character(cols) || (length(cols) < 1))
    stop("'cols' must be a vector of column names (character)")
  bad <- cols[!cols %in% colnames(x)]
  if (length(bad) > 0)
    stop(paste0("column(s) specified as 'cols' not found in 'x': '",
        paste(bad, collapse=", "),"'"))

  dup <- anyDuplicated(x[,cols,drop=FALSE])
  if (dup > 0) {
    if (!silent)
      print(paste0("data in 'x' violate unique constraint with regard ",
        "to the column(s) '",paste(cols, collapse="', '"),"'; first bad",
        " record occurs at row ",dup," (",paste(cols, unlist(x[dup,cols]),
          sep="=", collapse=", "),")"))
    return(FALSE)
  }
  return(TRUE)
}

