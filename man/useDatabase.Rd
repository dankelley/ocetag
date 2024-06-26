% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useDatabase.R
\name{useDatabase}
\alias{useDatabase}
\title{Create or update a database of tags}
\usage{
useDatabase(dbname = getDatabaseName(), mapping, tags, debug = 0)
}
\arguments{
\item{dbname}{character value naming the database.}

\item{mapping}{a named vector (or a list) that specifies a
mapping from textual descriptions to numerical codes; see
\sQuote{Examples}.}

\item{tags}{an optional named vector (or a list) that specifies
additional column names and types for the tag table.  The default
set is
\code{list(file = "TEXT", index = "INT", tag = "INT", analyst = "TEXT", analysisTime = "TIMESTAMP")}
which is likely to be suitable for CTD data, with tags referenced
to the depth at the stated index (starting with index 1, at the
surface). See \sQuote{Example} for how pressure may be added.}

\item{debug}{integer value indicating a debugging level.  Use 0 for silent
operation, or any integer exceeding 1 to show some output during the action
of this function.}
}
\description{
If the named file does not exist, it is created as an SQLite file,
with tables that match the needs of other functions in the package.
If it does exist, it is passed to \code{\link[=updateDatabase]{updateDatabase()}} for possible
updating to the present-day database schema (see the
\sQuote{History of Changes} section of the documentation for
[updateDatabase() for a listing of the changes).
}
\section{History of Changes}{

\itemize{
\item \strong{2024-04-20:} two changes were made to the database setup.
First, the \code{version} element of the \code{version} table was changed
from 1 to 2.  Second, the \code{level} element of the \code{tags} table was
renamed as \code{index}.  Both changes are automatically handled by
\code{useDatabase()}, which alters old files to the new format. All
other functions of the package use the new format, so the system is
backwards-compatible.
}
}

\examples{
# This example uses a temporary file and unlinks it later;
# obviously, this is not what would be done in practice, but
# it is necessary for the example to pass CRAN checks.
library(ocetag)
# Use a simple mapping
mapping <- c("good data" = 1, "bad data" = 4)
# Add pressure to the 'tags' database
tags <- c("pressure" = "FLOAT")
dbname <- tempfile() # do not do this in practice
# Finally, create the database.
useDatabase(dbname, mapping = mapping, tags = tags)
unlink(dbname) # do not do this in practice

}
\author{
Dan Kelley
}
