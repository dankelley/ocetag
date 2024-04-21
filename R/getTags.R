#' Get Tag Mapping from the Database
#'
#' @param file character value naming the CTD file.  Only items relating
#' to this file are returned.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @return [getTags()] returns a data frame holding the tags that
#' have been saved for a particular data file.
#'
#' @examples
#' # This example uses a temporary file and unlinks it later;
#' # obviously, this is not what would be done in practice, but
#' # it is necessary for the example to pass CRAN checks.
#' dbname <- tempfile() # do not do this in practice
#' createDatabase(dbname)
#' saveTag("ctd.cnv", index = 1, tag = 2, analyst = "Dan Kelley", dbname = dbname)
#' tags <- getTags("ctd.cnv", dbname = dbname)
#' print(tags)
#' unlink(dbname) # do not do this in practice
#'
#' @author Dan Kelley
#'
#' @export
getTags <- function(file = NULL, dbname = getDatabaseName(), debug = 0) {
    dmsg(debug, "getTags(file=\"", file, "\", dbname=\"", dbname, "\"\n")
    updateDatabase(dbname, debug = debug)
    tags <- NULL
    if (file.exists(dbname)) {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        if (RSQLite::dbExistsTable(con, "tags")) {
            tags <- RSQLite::dbReadTable(con, "tags")
            RSQLite::dbDisconnect(con)
            if (!is.null(file)) {
                tags <- tags[tags$file == file, ]
            }
        }
    }
    tags
}
