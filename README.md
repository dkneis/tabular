``tabular`` - SQL-like constraints for data frames
==================================================

Purpose
---------------------------------------------

This package is to add safety to poor man's data bases consisting of a collection of R data frames.

In a true, SQL-based relational data base, we can design the individual tables such that constrains apply to certain columns (or combinations of columns). The widely used constraints are

- 'unique': The respective column will not accept duplicate entries
- 'not null': The respective column does not allow for missing values
- 'primary key': Combines the 'unique' and 'not null' constraints
- 'foreign key': Forces the existence of corresponding values in a child and parent table; this is a necessary condition for joining the two tables

Such constraints are of vital importance for data integrity. Unfortunately, R has no built-in mechanism to impose those constrains on data frames and collections thereof. This package is to provides a pragmatic solution to this issue.

Example
---------------------------------------------

In this example, we create a poor man's data base consisting of two join-able data frames. Well' then write a function to check the integrity of this data base. Wrapping all checks into a dedicated function is recommended since this function can be called repeatedly, e.g. after insert or delete operations.

```
library("tabular")

# test data
countries <- read.table(header=TRUE, text='
id name
1  USA
2  UK
3  Russia
')

bosses <- read.table(header=TRUE, text='
id name    id_country
1  Trump   1
2  Johnson 2
3  Blair   2
4  Putin   3
')

# validation function taking a list of data frames as argument
validate <- function(database) {
  with(database, {
    # primary key and unique constraints
    stopifnot(check.key(countries, "id"))
    stopifnot(check.unique(countries, "name"))
    stopifnot(check.notnull(countries, "name"))
    stopifnot(check.key(bosses, "id"))
    stopifnot(check.unique(bosses, "name"))
    stopifnot(check.notnull(bosses, "name"))
    # foreign key constraint
    stopifnot(check.link(bosses, "id_country", countries, "id"))
  })
  invisible(NULL)
}

# application of the validation function
db <- list(countries=countries, bosses=bosses)
validate(db)

```

Requirements and installation
---------------------------------------------

The development version of the package can be installed from github as shown below. A CRAN version may become available later once the package has matured.

```
# Install and load development version
library("devtools")
install_github("dkneis/tabular")
library("tabular")
```
