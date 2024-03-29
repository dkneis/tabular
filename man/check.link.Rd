% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.link.r
\name{check.link}
\alias{check.link}
\title{Check foreign key constraints}
\usage{
check.link(
  child.tbl,
  child.col,
  parent.tbl,
  parent.col,
  na.ok = FALSE,
  silent = FALSE
)
}
\arguments{
\item{child.tbl}{Data frame representing the table on the 'many' side of an
'one-to-many' relation. See notes.}

\item{child.col}{Name of a column from \code{child.tbl} providing the link
to the other table (character string).}

\item{parent.tbl}{Data frame representing the table on the 'one' side of an
'one-to-many' relation. See notes.}

\item{parent.col}{Name of a column from \code{parent.tbl} providing the link
to the other table (character string).}

\item{na.ok}{Logical. Allow \code{NA} values in the child column without
the respective complement in the parent column?}

\item{silent}{Logical. If \code{FALSE}, details on constraint
violations are shown using \code{\link[base]{print}}.}
}
\value{
\code{TRUE} if the check was passed successfully and
  \code{FALSE} otherwise.
}
\description{
Check for violation of a foreign key constraint
}
\note{
The function is made to verify that two tables having a one-to-many
  relation (\url{https://en.wikipedia.org/wiki/One-to-many_(data_model)})
  can successfully be joined on the specified columns.
}
\examples{

data(people, countries)
print(people)
print(countries)

# Should succeed
check.link(people, "id_country", countries, "id")

# Example of an orphaned child record
check.link(people, "id_country", countries[1:2,], "id")

# Example of ambiguity
countries2 <- rbind(countries, data.frame(id=3, country="India"))
check.link(people, "id_country", countries2, "id")
}
\seealso{
There are more functions to check constraints, namely
  \code{\link{check.notnull}}, \code{\link{check.unique}},
  and \code{\link{check.key}}.
  See also the example for \code{\link{db.read}}.
}
\author{
David Kneis \email{david.kneis@tu-dresden.de}
}
