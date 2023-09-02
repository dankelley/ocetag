library(DBI)
library(RSQLite)

getUserName <- function()
{
    # FIXME: maybe use Sys.info()[["user"]] ???
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res)) "unknown" else res
}

getDatabaseName <- function(prefix="~/ctd_tag")
{
    normalizePath(paste0(prefix, "_", getUserName(), ".db"))
}

createDatabase <- function(dbname=getDatabaseName(), debug=0)
{
    if (!file.exists(dbname)) {
        if (debug > 0)
            msg("creating '", dbname, "'\n")
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        RSQLite::dbCreateTable(con, "version",
            c("version"="INTEGER"))
        RSQLite::dbWriteTable(con, "version", data.frame(version=1L), overwrite=TRUE)
        RSQLite::dbCreateTable(con, "tags",
            c("file"="TEXT", level="INT", tag="INT", analyst="TEXT", analysisTime="TIMESTAMP"))
        RSQLite::dbDisconnect(con)
    }
}

getTags <- function(file=NULL, dbname=getDatabaseName(), debug=0)
{
    tags <- NULL
    if (file.exists(dbname)) {
        con <- dbConnect(RSQLite::SQLite(), dbname)
        if (RSQLite::dbExistsTable(con, "tags")) {
            tags <- RSQLite::dbReadTable(con, "tags")
            RSQLite::dbDisconnect(con)
            if (!is.null(file)) {
                tags <- tags[tags$file == file, ]
            }
        }
    }
    tags
}

removeTag <- function(file=NULL, level=NULL, dbname=NULL, debug=0)
{
    if (debug > 0)
        msg("removeTag(file=", file, ", level=", level, ", dbname=", dbname, "\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        if (debug > 0) {
            msg("will remove ", paste(remove, collapse=" "), "-th tag\n")
            msg(" BEFORE levels are: ", paste(tags$level, collapse=" "), "\n")
        }
        tags <- tags[-remove, ]
        if (debug > 0)
            msg(" AFTER  levels are: ", paste(tags$level, collapse=" "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    }
    RSQLite::dbDisconnect(con)
}

saveTag <- function(file=NULL, level=NULL, tag=NULL, analyst=NULL, dbname=NULL, debug=0)
{
    # no checking on NULL; add that if we want to generalize
    df <- data.frame(file=file, level=level, tag=tag, analyst=analyst, analysisTime=Sys.time())
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

