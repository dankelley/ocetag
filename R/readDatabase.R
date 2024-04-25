#' Read a tag database into an R object
#'
#' Examine the named database, and return a list containing all
#' of its data.  This may be helpful to users who are interested
#' in the internal structure, but who are not cognizant of the R
#' interface to SQL.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @importFrom RSQLite dbConnect dbDisconnect dbListTables dbReadTable dbWriteTable SQLite
#'
#' @author Dan Kelley
#'
#' @export
readDatabase <- function(dbname = getDatabaseName(), debug = 0) {
    message("readDatabase has debug=", debug)
    debug <- if (debug[1] > 0) 1L else 0L
    dmsg(debug, "Opening database file \"", dbname, "\"\n")
    con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
    tables <- RSQLite::dbListTables(con)
    dmsg(
        debug, "Found tables named \"", paste(tables, collapse = "\" \""),
        "\"\n"
    )
    rval <- list()
    for (table in tables) {
        dmsg(debug, "   reading \"", table, "\"\n")
        rval[[table]] <- RSQLite::dbReadTable(con, table)
    }
    RSQLite::dbDisconnect(con)
    dmsg(debug, "Done\n")
    rval
}
