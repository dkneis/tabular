#' Read a set of text files forming a data base
#'
#' Reads a set of delimited text files supposed to form a relational data base
#'
#' @param dir Directory containing the text files (character string). Will be
#'   passed to argument \code{path} of \code{\link[base]{list.files}}.
#' @param ext A character string representing a file extension used to
#'   identify the files to be read. This is typically a string of three
#'   characters like, e.g., 'csv' or 'tsv'. A preceding dot is implicitly
#'   assumed to be present and must be omitted.
#' @param sep The field delimiter (character) for use with 
#'   \code{\link[utils]{read.table}}.
#' @param ... Further optional arguments passed to \code{\link[utils]{read.table}}.
#' 
#' @return A list, each element of which is a data frame. Element names are
#'   constructed from the source file names by stripping the specified extension
#'   and the preceeding dot.
#'
#' @note The text files are read with \code{\link[utils]{read.table}} using the
#'   fixed arguments \code{header=TRUE} and \code{stringsAsFactors=FALSE}. Thus,
#'   one should not try to overwrite these settings using the \code{...}
#'   argument. It is possible to set other optional arguments of \code{read.table} 
#'   like \code{skip} or \code{encoding}.
#'
#' @seealso After reading the set of files, one typically wants to check the
#'   data base for integrity using the functions \code{\link{check.notnull}},
#'   \code{\link{check.unique}}, \code{\link{check.key}}, and
#'   \code{\link{check.link}}. It is probably good style to wrap all necessary
#'   checks into a single dedicated function that can be called repeatedly
#'   (e.g. after manipulation of data) or which can be re-used for other
#'   data bases of the same layout. See example below. 
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' 
#' # Read example DB shipped with the package
#' db <- db.read(dir=system.file("examples", package="tabular"), ext="tsv")
#' print(names(db))
#' 
#' # Integrity checks wrapped into a dedicated function
#' validate <- function(db) {
#'   with(db, {
#'     stopifnot(check.key(samples, c("date","id_location")))
#'     stopifnot(check.link(samples, "id_location", locations, "id"))
#'     # further checks would go here ...
#'  })
#' }
#' 
#' validate(db)

db.read <- function(dir=".", ext="tsv", sep="\t", ...) {
  if (!dir.exists(dir))
    stop(paste0("directory '",dir,"' is inaccessible"))
  tables <- list.files(path=dir, pattern=paste0("[.]",ext,"$"), full.names=TRUE)
  if (length(tables) == 0)
    stop(paste0("no files with specified extention in directory '",dir,"'"))
  db <- list()
  for (tbl in tables) {
    tblName <- gsub(basename(tbl), pattern=paste0("(.+)[.]",ext,"$"),
      replacement="\\1")
    db[[tblName]] <- utils::read.table(file=tbl, header=TRUE, sep=sep,
      stringsAsFactors=FALSE, ...)
  }
  db
}
