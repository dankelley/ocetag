library(DBI)
library(RSQLite)

getUserName <- function()
{
    # FIXME: maybe use Sys.info()[["user"]] ???
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res)) {
        res <- "unknown"
    }
    res
}

getDatabaseName <- function(prefix="~/ctd_tag")
{
    normalizePath(paste0(prefix, "_", getUserName(), ".db"))
}

createDatabase <- function(dbname=getDatabaseName())
{
    if (!file.exists(dbname)) {
        dmsg("creating '", dbname, "'\n")
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        RSQLite::dbCreateTable(con, "version",
            c("version"="INTEGER"))
        RSQLite::dbWriteTable(con, "version", data.frame(version=1L), overwrite=TRUE)
        RSQLite::dbCreateTable(con, "tags",
            c("file"="TEXT", level="INT", tag="INT", analyst="TEXT", analysisTime="TIMESTAMP"))
        RSQLite::dbDisconnect(con)
    }
}

getTags <- function(file=NULL, dbname=getDatabaseName())
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

removeTag <- function(file=NULL, level=NULL, dbname=NULL)
{
    dmsg("removeTag(file=", file, ", level=", level, ", dbname=", dbname, "\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        dmsg("will remove ", paste(remove, collapse=" "), "-th tag\n")
        dmsg(" BEFORE levels are: ", paste(tags$level, collapse=" "), "\n")
        tags <- tags[-remove, ]
        dmsg(" AFTER  levels are: ", paste(tags$level, collapse=" "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    }
    RSQLite::dbDisconnect(con)
}

saveTag <- function(file=NULL, level=NULL, tag=NULL, analyst=NULL, dbname=NULL)
{
    # no checking on NULL; add that if we want to generalize
    df <- data.frame(file=file, level=level, tag=tag, analyst=analyst, analysisTime=Sys.time())
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

