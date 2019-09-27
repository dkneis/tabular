#' Check foreign key constraints
#'
#' Check for violation of a foreign key constraint
#'
#' @param child.tbl Data frame representing the table on the 'many' side of an
#'   'one-to-many' relation. See notes.
#' @param child.col Name of a column from \code{child.tbl} providing the link
#'   to the other table (character string).
#' @param parent.tbl Data frame representing the table on the 'one' side of an
#'   'one-to-many' relation. See notes.
#' @param parent.col Name of a column from \code{parent.tbl} providing the link
#'   to the other table (character string).
#' @param silent Logical. If \code{FALSE}, details on constraint
#'  violations are shown using \code{\link[base]{print}}.
#' 
#' @return \code{TRUE} if the check was passed successfully and
#'   \code{FALSE} otherwise.
#'
#' @note The function is made to verify that two tables having a one-to-many
#'   relation (\url{https://en.wikipedia.org/wiki/One-to-many_(data_model)})
#'   can successfully be joined on the specified columns.
#'
#' @seealso There are more functions to check constraints, namely
#'   \code{\link{check.notnull}}, \code{\link{check.unique}},
#'   and \code{\link{check.key}}.
#'   See also the example for \code{\link{db.read}}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' 
#' data(people, countries)
#' print(people)
#' print(countries)
#'
#' # Should succeed
#' check.link(people, "id_country", countries, "id")
#' 
#' # Example of an orphaned child record
#' check.link(people, "id_country", countries[1:2,], "id")
#' 
#' # Example of ambiguity
#' countries2 <- rbind(countries, data.frame(id=3, country="India"))
#' check.link(people, "id_country", countries2, "id")

check.link <- function(child.tbl, child.col, parent.tbl, parent.col, silent=FALSE) {

  # check child table  
  if (!is.data.frame(child.tbl))
    stop("'child.tbl' must be a data frame")
  if (!is.character(child.col) || (length(child.col) != 1))
    stop("'child.col' must be a character string")
  if (!child.col %in% colnames(child.tbl))
    stop("column specified as 'child.col' not present in 'child.tbl'")
  # check parent table
  if (!is.data.frame(parent.tbl))
    stop("'parent.tbl' must be a data frame")
  if (!is.character(parent.col) || (length(parent.col) != 1))
    stop("'parent.col' must be a character string")
  if (!parent.col %in% colnames(parent.tbl))
    stop("column specified as 'parent.col' not present in 'parent.tbl'")

  # get field values
  childValues <- child.tbl[,child.col]
  parentValues <- parent.tbl[,parent.col]

  # check identity of types
  if (!identical(typeof(childValues), typeof(parentValues))) {
    if (!silent)
      print(paste0("invalid relation between 'child.tbl' and 'parent.tbl'",
        " since 'child.col' has type ",typeof(childValues),
        " whereas 'parent.col' has type ",typeof(parentValues)))
    return(FALSE)
  }

  # check for duplicates in the parent field
  if (anyDuplicated(parentValues)) {
    if (!silent)
      print(paste0("invalid relation between 'child.tbl' and 'parent.tbl'",
        " since entries in 'parent.col' are not unique"))
    return(FALSE)
  }
    
  # check for orphaned records in child table
  bad <- childValues[!childValues %in% parentValues]
  if (length(bad) > 0) {
    if (!silent)
      print(paste0("invalid relation between 'child.tbl' and 'parent.tbl'",
        " since 'child.tbl' contains ",length(bad),
        " orphaned record(s); the first orphaned record has value '",
        bad[1],"' in 'child.col'"))
    return(FALSE)
  }

  return(TRUE)
}

