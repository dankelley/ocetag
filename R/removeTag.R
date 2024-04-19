#' Remove a tag
#'
#' @param file character value naming the CTD file.  Only items relating
#' to this file are returned.
#'
#' @param level integer specifying the sequence value of the data point that was tagged.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
removeTag <- function(file = NULL, level = NULL, dbname = NULL, debug = 0) {
    dmsg(debug, "removeTag(file=", file, ", level=", level, ", dbname=", dbname, "\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        dmsg(debug, "will remove ", paste(remove, collapse = " "), "-th tag\n")
        dmsg(debug, " BEFORE levels are: ", paste(tags$level, collapse = " "), "\n")
        tags <- tags[-remove, ]
        dmsg(debug, " AFTER  levels are: ", paste(tags$level, collapse = " "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite = TRUE)
    }
    RSQLite::dbDisconnect(con)
}
