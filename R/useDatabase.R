#' Create or update a database of tags
#'
#' If the named file does not exist, it is created as an SQLite file,
#' with tables that match the needs of other functions in the package.
#' If it does exist, it is passed to [updateDatabase()] for possible
#' updating to the present-day database schema (see the
#' \sQuote{History of Changes} section of the documentation for
#' [updateDatabase() for a listing of the changes).
#'
#' @template dbnameTemplate
#'
#' @param mapping a named vector (or a list) that specifies a
#' mapping from textual descriptions to numerical codes; see
#' \sQuote{Examples}.
#'
#' @param tags an optional named vector (or a list) that specifies
#' additional column names and types for the tag table.  The default
#' set is
#' `list(file = "TEXT", index = "INT", tag = "INT", analyst = "TEXT", analysisTime = "TIMESTAMP")`
#' which is likely to be suitable for CTD data, with tags referenced
#' to the depth at the stated index (starting with index 1, at the
#' surface). See \sQuote{Example} for how pressure may be added.
#'
#' @template debugTemplate
#'
#' @examples
#' # This example uses a temporary file and unlinks it later;
#' # obviously, this is not what would be done in practice, but
#' # it is necessary for the example to pass CRAN checks.
#' library(ocetag)
#' # Use a simple mapping
#' mapping <- c("good data" = 1, "bad data" = 4)
#' # Add pressure to the 'tags' database
#' tags <- c("pressure" = "FLOAT")
#' dbname <- tempfile() # do not do this in practice
#' # Finally, create the database.
#' useDatabase(dbname, mapping = mapping, tags = tags)
#' unlink(dbname) # do not do this in practice
#'
#' @section History of Changes:
#'
#' * **2024-04-20:** two changes were made to the database setup.
#' First, the `version` element of the `version` table was changed
#' from 1 to 2.  Second, the `level` element of the `tags` table was
#' renamed as `index`.  Both changes are automatically handled by
#' `useDatabase()`, which alters old files to the new format. All
#' other functions of the package use the new format, so the system is
#' backwards-compatible.
#'
#' @importFrom RSQLite dbConnect dbCreateTable dbDisconnect dbReadTable dbWriteTable SQLite
#'
#' @author Dan Kelley
#'
#' @export
useDatabase <- function(dbname = getDatabaseName(), mapping, tags, debug = 0) {
    version <- 3L # stored in the db (and checked in old dbs)
    if (!(is.character(dbname) && nchar(dbname) > 0L)) {
        stop("dbname must be a non-empty character value")
    }
    debug <- if (debug[1] > 0) 1L else 0L
    if (file.exists(dbname)) {
        dmsg(debug, "Updating existing database\n")
        updateDatabase(dbname, debug = debug)
    } else {
        dmsg(debug, "Creating database file \"", dbname, "\"\n")
        if (missing(mapping)) {
            dmsg("using default mapping, common with CTD data\n")
            mapping <- c(
                "no QC performed" = 0,
                "good data" = 1,
                "probably good" = 2,
                "probably bad" = 3,
                "bad data" = 4,
                "value changed" = 5,
                "estimated value" = 8,
                "missing value" = 9
            )
        }
        # tagsAll starts with a default
        tagsDefaultStart <- c(
            "file" = "TEXT",
            "index" = "INTEGER"
        )
        tagsDefaultEnd <- c(
            "tag" = "INTEGER",
            "analyst" = "TEXT",
            "analysisTime" = "TIMESTAMP"
        )
        if (missing(tags)) {
            tags <- c(tagsDefaultStart, tagsDefaultEnd)
        } else {
            tags <- c(tagsDefaultStart, tags, tagsDefaultEnd)
        }
        if (debug > 0) {
            cat("tags (including user-specified and default):\n")
            print(tags)
        }
        dmsg(debug, "about to make SQL connection\n")
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        # Version table
        if (debug) cat("about to create and write 'version' table\n")
        RSQLite::dbCreateTable(con, "version", c("version" = "INTEGER"))
        # NB version 1 used 'level' for what we now call 'index'
        RSQLite::dbWriteTable(con, "version", data.frame(version = version), overwrite = TRUE)
        # Tag mapping
        if (debug) cat("about to create and write 'mapping' table\n")
        RSQLite::dbCreateTable(con, "mapping", c(value = "INTEGER", meaning = "TEXT"))
        # Revise mapping as a data frame, for inclusion in db
        mapping <- data.frame(value = as.integer(mapping), name = names(mapping))
        RSQLite::dbWriteTable(con, "mapping", mapping, overwrite = TRUE)
        # Update 'tags', reform into a named vector, and then store in db
        if (debug) cat("about to create and write 'tags' table\n")
        RSQLite::dbCreateTable(con, "tags", tags)
        # Notes
        dmsg(debug, "about to create 'notes' table\n")
        RSQLite::dbCreateTable(con, "notes", c("file" = "TEXT", "index" = "INTEGER", "note" = "TEXT"))
        dmsg(debug, "about to disconnect db\n")
        RSQLite::dbDisconnect(con)
    }
    invisible(NULL)
}
