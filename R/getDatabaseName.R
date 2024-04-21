#' Get name of database
#'
#' @param dbprefix character value used as the start of the name.  The default
#' value places the database in the user's top level.
#'
#' @return [getDatabaseName] returns a character string holding the full
#' pathname of the database.
#'
#' @author Dan Kelley
#'
#' @export
getDatabaseName <- function(dbprefix = "~/ocetag") {
    path.expand(paste0(dbprefix, "_", getUserName(), ".db"))
}
