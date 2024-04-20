#' Create a Database for Tagging
#'
# This is a utility function, used by [ctdTag()] to set up a database
# to hold tags established by clicking on CTD plots. If a database
# of the provided name already exists, it is not altered.
#'
#' @template dbnameTemplate
#'
#' @param mapping a named vector (or a list) that specifies a
#' mapping from textual descriptions to numerical codes; see
#' \sQuote{Examples}.
#'
#' @param tags an optional named vector (or a list) that specifies additional column names and
#' types for the tag table.  The default set is
#' `list(file = "TEXT", index = "INT", tag = "INT", analyst = "TEXT", analysisTime = "TIMESTAMP")`
#' which is likely to be suitable for CTD data, with tags referenced to the depth
#' at the stated index (starting with index 1, at the surface).
#' See \sQuote{Example} for how pressure may be added.
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
#' createDatabase(dbname, mapping = mapping, tags = tags)
#' unlink(dbname) # do not do this in practice
#'
#' @importFrom RSQLite dbConnect dbCreateTable dbDisconnect dbReadTable dbWriteTable SQLite
#'
#' @author Dan Kelley
#'
#' @export
createDatabase <- function(dbname = getDatabaseName(), mapping, tags, debug = 0) {
    if (!(is.character(dbname) && nchar(dbname) > 0L)) {
        stop("dbname must be a non-empty character value")
    }
    debug <- if (debug[1] > 0) 1L else 0L
    if (!file.exists(dbname)) {
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
            "index" = "INT")
        tagsDefaultEnd <- c(
            "tag" = "INT",
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
        RSQLite::dbWriteTable(con, "version", data.frame(version = 1L), overwrite = TRUE)
        # Tag mapping
        if (debug) cat("about to create and write 'mapping' table\n")
        RSQLite::dbCreateTable(con, "mapping", c(value = "INT", meaning = "TEXT"))
        # Revise mapping as a data frame, for inclusion in db
        mapping <- data.frame(value = as.integer(mapping), name = names(mapping))
        if (debug) {
            cat("next is mapping used for tags\n")
            print(mapping)
        }
        RSQLite::dbWriteTable(con, "mapping", mapping, overwrite = TRUE)
        # Update 'tags', reform into a named vector, and then store in db
        if (debug) {
            cat("about to create and write 'tags' table\n")
            cat("next is tags\n")
            print(tags)
        }
        RSQLite::dbCreateTable(con, "tags", tags)
        dmsg(debug, "about to disconnect db\n")
        RSQLite::dbDisconnect(con)
    } else {
        dmsg(debug, "Using existing database file \"", dbname, "\"\n")
    }
    invisible(NULL)
}
