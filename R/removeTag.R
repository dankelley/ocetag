#' Remove a tag
#'
#' @param file character value naming the CTD file.  Only items relating
#' to this file are returned.
#'
#' @param index integer specifying the sequence value of the data point that was tagged.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
removeTag <- function(file = NULL, index = NULL, dbname = NULL, debug = 0) {
    dmsg(debug, "removeTag(file=", file, ", index=", index, ", dbname=", dbname, "\n")
    updateDatabase(dbname, debug = debug)
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$index == index)
    if (length(remove)) {
        dmsg(debug, "will remove ", paste(remove, collapse = " "), "-th tag\n")
        dmsg(debug, " BEFORE indexs are: ", paste(tags$index, collapse = " "), "\n")
        tags <- tags[-remove, ]
        dmsg(debug, " AFTER  indexs are: ", paste(tags$index, collapse = " "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite = TRUE)
    }
    RSQLite::dbDisconnect(con)
}
