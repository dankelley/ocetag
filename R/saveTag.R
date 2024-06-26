#' Save a tag
#'
#' @param file character value naming the CTD file.
#'
#' @param index integer specifying the sequence value of the tagged
#' data point.
#'
#' @param \dots optional extra columns to be stored in the database.
#' These must have been specified in the [useDatabase()] call
#' that created the database named in `dbname`.
#'
#' @template tagTemplate
#'
#' @template analystTemplate
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @examples
#' # This example uses a temporary file and unlinks it later;
#' # obviously, this is not what would be done in practice, but
#' # it is necessary for the example to pass CRAN checks.
#' dbname <- tempfile() # do not do this in practice
#' useDatabase(dbname)
#' saveTag("ctd.cnv", index = 1, tag = 2, analyst = "Dan Kelley", dbname = dbname)
#' tags <- getTags("ctd.cnv", dbname = dbname)
#' print(tags)
#' unlink(dbname) # do not do this in practice
#'
#' @author Dan Kelley
#'
#' @export
saveTag <- function(file, index, ..., tag,
                    analyst = "anonymous", dbname = getDatabaseName(), debug = 0) {
    if (missing(file)) stop("must supply 'file'")
    if (missing(index)) stop("must supply 'index'")
    dots <- list(...)
    if (!length(dots) && missing(index)) stop("must supply 'index' and/or '...'")
    if (missing(tag)) stop("must supply 'tag'")
    if (missing(dbname)) stop("must supply 'dbname'")
    debug <- if (debug[1] > 0) 1L else 0L
    dmsg(
        debug,
        sprintf(
            "saveTag(file=\"%s\",index=%d,...,tag=%d,analyst=\"%s\", dbname=\"%s\") {\n",
            file, index, tag, analyst, dbname
        )
    )
    df <- data.frame(
        file = file, index = index, tag = tag,
        analyst = analyst, analysisTime = round(Sys.time())
    )
    if (debug > 0) {
        cat("before adding ..., df is as follows\n")
        print(df)
    }
    dotsNames <- names(dots)
    for (i in seq_along(dots)) {
        dmsg(debug, "  adding ", dotsNames[i], "\n")
        df[[dotsNames[i]]] <- dots[[i]]
    }
    if (debug > 0) {
        cat("after possibly adding ..., df is as follows\n")
        print(df)
    }
    con <- dbConnect(RSQLite::SQLite(), dbname)
    if (RSQLite::dbExistsTable(con, "tags")) {
        existingTags <- RSQLite::dbReadTable(con, "tags")
    }
    # Ensure that all named fields exist in the db
    newnames <- names(df)
    oldnames <- names(existingTags)
    if (!identical(sort(newnames), sort(oldnames))) {
        stop(
            "Column name mismatch\n",
            "  saveTag() has        c(\"", paste(newnames, collapse = "\",\""), "\")\n",
            "  useDatabase() had c(\"", paste(oldnames, collapse = "\",\""), "\")"
        )
    }
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}
