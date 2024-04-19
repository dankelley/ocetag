#' Get Tag Mapping from the Database
#'
#' @param file character value naming the CTD file.  Only items relating
#' to this file are returned.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @return [getTagMapping()] returns a data frame holding the `code` and
#' `meaning` of each possible tag.
#'
#' @author Dan Kelley
#'
#' @export
getTags <- function(file = NULL, dbname = getDatabaseName(), debug = 0) {
    dmsg(debug, "getTags(file=\"", file, "\", dbname=\"", dbname, "\"\n")
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
