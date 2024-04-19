#' Save a tag
#'
#' @param file character value naming the CTD file.
#'
#' @param level integer specifying the sequence value of the tagged data point.
#'
#' @param scan integer specifying the scan number of the tagged data point.
#'
#' @template tagTemplate
#'
#' @template analystTemplate
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @export
saveTag <- function(file = NULL, level = NULL, scan = NULL, tag = NULL, analyst = NULL, dbname = NULL, debug = 0) {
    # no checking on NULL; add that if we want to generalize
    dmsg(debug, "saveTag(..., level=", level, ", scan=", scan, ", tag=", tag, ", ...)\n")
    df <- data.frame(file = file, level = level, scan = scan, tag = tag, analyst = analyst, analysisTime = round(Sys.time()))
    if (debug > 0) {
        print(df, file = stderr())
    }
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}
