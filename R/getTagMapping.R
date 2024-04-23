#' Get Mapping of Tag Value to Tag Meaning
#'
#' Get textual descriptions of the meanings of the numerical tags.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @return [getTagMapping] returns a data frame of tag value:meaning pairs.
#'
#' @author Dan Kelley
#'
#' @export
getTagMapping <- function(dbname = getDatabaseName(), debug = 0) {
    dmsg(debug, "getTagMapping(dbname=\"", dbname, "\"\n")
    updateDatabase(dbname, debug = debug)
    rval <- NULL
    if (file.exists(dbname)) {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        if (RSQLite::dbExistsTable(con, "tagMapping")) {
            rval <- RSQLite::dbReadTable(con, "tagMapping")
        }
        RSQLite::dbDisconnect(con)
    }
    rval
} # getTagMapping()
