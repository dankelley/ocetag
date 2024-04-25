#' Update a database of tags
#'
#' Examine the named database, and update it if required (see
#' \sQuote{Details}). Report an error if the file does not
#' exist, or if the file does exist, but it is not an SQLite file
#' containing tables named `version`, `tagMapping` and `tags`.
#'
#' The following is a history of changes to the database schema.
#'
#' * **2024-04-20:** two changes were made to the database setup.
#' First, the `version` element of the `version` table was changed
#' from 1 to 2.  Second, the `level` element of the `tags` table was
#' renamed as `index`.  Both changes are automatically handled by
#' `useDatabase()`, which alters old files to the new format. All
#' other functions of the package use the new format, so the system is
#' backwards-compatible.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @importFrom RSQLite dbConnect dbCreateTable dbDisconnect dbReadTable dbWriteTable SQLite
#'
#' @author Dan Kelley
#'
#' @export
updateDatabase <- function(dbname = getDatabaseName(), debug = 0) {
    version <- 3L
    if (!(is.character(dbname) && nchar(dbname) > 0L)) {
        stop("dbname must be a non-empty character value")
    }
    if (!file.exists(dbname)) {
        stop("No such database exists; use useDatabase() to make one")
    }
    debug <- if (debug[1] > 0) 1L else 0L
    # update the db if required to match the present format
    dmsg(debug, "Examining database file \"", dbname, "\"\n")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
    oldversion <- RSQLite::dbReadTable(con, "version")
    dmsg(debug, "Checking version number\n")
    if (oldversion < version) {
        dmsg(debug, "    ... updating to version ", version, "\n")
        RSQLite::dbWriteTable(con, "version", data.frame(version = version), overwrite = TRUE)
    } else {
        dmsg(debug, "   ... up-to-date\n")
    }
    tableNames <- RSQLite::dbListTables(con)
    if (!"tags" %in% tableNames) {
        stop("database lacks a 'tags' table")
    }
    if (!"notes" %in% tableNames) {
        dmsg(debug, "Creating notes table\n")
        RSQLite::dbCreateTable(con, "notes", c("file" = "TEXT", "index" = "INTEGER", "note" = "TEXT"))
    }
    dmsg(debug, "Checking for tables named `version`, `mapping` and `tags`\n")
    tableNames <- RSQLite::dbListTables(con)
    dmsg(debug, "Checking names in 'tags' table\n")
    # rename tags$level as tags$index (if needed)
    tags <- RSQLite::dbReadTable(con, "tags")
    tagsNames <- names(tags)
    if ("level" %in% tagsNames) {
        dmsg(debug, "    ... renaming 'level' to 'index'\n")
        names(tags) <- gsub("level", "index", tagsNames)
        RSQLite::dbWriteTable(con, "tags", tags, overwrite = TRUE)
    } else {
        dmsg(debug, "    ... all are correct\n")
    }
    dmsg(debug, "Finished updating ", dbname, "\n")
    RSQLite::dbDisconnect(con)
}
