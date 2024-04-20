#' Get Mapping of Tag Value to Tag Meaning
#'
#' A utility function, used by [ctdTag()] to get textual descriptions
#' of the meanings of the numerical tags.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @return [getTagMapping()] returns a data frame of tag value:meaning pairss.
#'
#' @author Dan Kelley
#'
#' @export
getTagMapping <- function(dbname = getDatabaseName(), debug = 0) {
    dmsg(debug, "getTagMapping(file=\"", file, "\", dbname=\"", dbname, "\"\n")
    rval <- NULL
    if (file.exists(dbname)) {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        if (RSQLite::dbExistsTable(con, "tagMapping")) {
            rval <- RSQLite::dbReadTable(con, "tagMapping")
        }
        RSQLite::dbDisconnect(con)
    }
    #>msg <- "Codes and their meanings: "
    #>for (i in seq_len(nrow(rval))) {
    #>    msg <- c(msg, rval[i, "value"], "=\"", rval[i, "meaning"], "\"; ", sep="")
    #> }
    #>message(msg)
    rval
} # getTagMapping()
