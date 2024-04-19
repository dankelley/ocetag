#' Create a Database for Tagging
#'
# This is a utility function, used by [ctdTag()] to set up a database
# to hold tags established by clicking on CTD plots. If a database
# of the provided name already exists, it is not altered.
#'
#' @template dbnameTemplate
#'
#' @param mapping an optional list value that specifies a mapping from numerical tag
#' codes to textual descriptions.  If not provided, a scheme common with CTD
#' data is used.  (See \sQuote{Examples}.)
#'
#' @param tags an optional list that specifies the column names and types for
#' the tag table.  If not provided, a scheme suitable for CTD data is used.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(ocetag)
#' mapping <- list("good data" = 1, "bad data" = 4)
#' tags <- list(
#'     "file" = "TEXT",
#'     "level" = "INT",
#'     "tag" = "INT",
#'     "analyst" = "TEXT",
#'     "analysisTime" = "TIMESTAMP"
#' )
#' unlink("test.db")
#' db <- createDatabase("test.db", mapping = mapping, tags = tags, debug = 2)
#' system("echo .dump|sqlite3 test.db")
#'}
#'
#' @importFrom RSQLite dbConnect dbCreateTable dbDisconnect dbReadTable dbWriteTable SQLite
#' @export
createDatabase <- function(dbname = getDatabaseName(), mapping, tags, debug = 0) {
    debug <- if (debug[1] > 0) 1L else 0L
    dmsg("dbname=\"", dbname, "\", debug=", debug, "\n")
    if (!file.exists(dbname)) {
        if (missing(mapping)) {
            dmsg("using default mapping, common with CTD data\n")
            mapping <- list(
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
        if (missing(tags)) {
            tags <- list(
                "file" = "TEXT",
                "level" = "INT",
                "scan" = "INT",
                "analyst" = "TEXT",
                "analysisTime" = "TIMESTAMP"
            )
        }
        dmsg(debug, "Creating database file \"", dbname, "\"\n")
        if (!(is.character(dbname) && nchar(dbname) > 0L)) {
            stop("dbname must be a non-empty character value")
        }
        if (!is.list(mapping)) {
            stop("'mapping' must be a list")
        }
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        # Version table
        RSQLite::dbCreateTable(con, "version", c("version" = "INTEGER"))
        RSQLite::dbWriteTable(con, "version", data.frame(version = 1L), overwrite = TRUE)
        # Mapping table (start with default)
        # tagMapping <- data.frame(value = 0:9, meaning = as.character(0:9))
        tagMapping <- data.frame(
            value = as.integer(mapping),
            meaning = names(mapping)
        )
        # cat("initial tagMapping...\n");print(tagMapping)
        tagValue <- as.integer(mapping)
        tagMeaning <- names(mapping)
        if (debug > 0) {
            cat("tagValue: ", paste(tagValue, collapse = ";"), "\n")
            cat("tagMeaning: ", paste(tagMeaning, collapse = ";"), "\n")
        }
        # cat("final tagMapping...\n");print(tagMapping)
        RSQLite::dbCreateTable(con, "tagMapping", c(value = "INT", meaning = "TEXT"))
        RSQLite::dbWriteTable(con, "tagMapping",
            data.frame(value = as.integer(mapping), name = names(mapping)),
            overwrite = TRUE
        )
        # Tag table
        tagTableSpecification <- as.character(tags)
        names(tagTableSpecification) <- names(tags)
        RSQLite::dbCreateTable(con, "tags", tagTableSpecification)
        RSQLite::dbDisconnect(con)
    } else {
        dmsg(debug, "Using existing database file \"", dbname, "\"\n")
    }
    invisible(NULL)
}
