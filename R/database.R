library(RSQLite)

#' Get user name
#'
#' @return [getUserName()] returns a character value naming the user, i.e.
#' holding the user's login name.
#'
#' @author Dan Kelley
#'
#' @export
getUserName <- function()
{
    # FIXME: maybe use Sys.info()[["user"]] ???
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res)) "unknown" else res
}

#' Get name of database
#'
#' @param dbprefix character value used as the start of the name.  The default
#' value places the database in the user's top level.
#'
#' @return [getDatabaseName()] returns a character string holding the full
#' pathname of the database.
#'
#' @author Dan Kelley
#'
#' @export
getDatabaseName <- function(dbprefix="~/ocetag")
{
    path.expand(paste0(dbprefix, "_", getUserName(), ".db"))
}

#' Create a Database for Tagging
#'
# This is a utility function, used by [ctdTag()] to set up a database
# to hold tags established by clicking on CTD plots. If a database
# of the provided name already exists, it is not altered, and [createDatabase()]
# returns without altering it.
#
#' @template dbnameTemplate
#'
#' @param mapping an optional list value that specifies a mapping from numerical tag
#' codes to textual descriptions.  For example, this might be something
#' like `list("MLD"=1)` in an analysis focussed on finding the mixed-layer
#' depth in a series of CTD profiles. The mapping is stored in the database
#' as a table named `tagMapping`, and this information is displayed by
#' [ctdtag()], below the plot.
#'
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @importFrom RSQLite dbConnect dbCreateTable dbDisconnect dbReadTable dbWriteTable SQLite 
#' @export
createDatabase <- function(dbname=getDatabaseName(), mapping=list(), debug=0)
{
    if (!file.exists(dbname)) {
        dmsg(debug, "creating '", dbname, "'\n")
        if (!(is.character(dbname) && nchar(dbname) > 0L))
            stop("dbname must be a non-empty character value")
        if (!is.list(mapping))
            stop("'mapping' must be a list")
        con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname)
        # Version table
        RSQLite::dbCreateTable(con, "version", c("version"="INTEGER"))
        RSQLite::dbWriteTable(con, "version", data.frame(version=1L), overwrite=TRUE)
        # Mapping table (start with default)
        tagMapping <- data.frame(value=0:9, meaning=as.character(0:9))
        #cat("initial tagMapping...\n");print(tagMapping)
        tagValue <- as.integer(mapping)
        tagMeaning <- names(mapping)
        #cat("tagValue: ", paste(tagValue, collapse=";"), "\n")
        #cat("tagMeaning: ", paste(tagMeaning, collapse=";"), "\n")
        for (i in seq_along(tagValue)) {
            value <- tagValue[i]
            meaning <- tagMeaning[i]
            #message("i=", i)
            if (tagValue < 0L || tagValue > 9L)
                stop("tag mapping code must be in range 1 to 9, but value ", i, " is ", tagValue)
            tagMapping[tagMapping$value==tagValue[i], "meaning"] <- tagMeaning
            #message(" ... ok")
        }
        #cat("final tagMapping...\n");print(tagMapping)
        RSQLite::dbCreateTable(con, "tagMapping", c(value="INT", meaning="TEXT"))
        RSQLite::dbWriteTable(con, "tagMapping", tagMapping, overwrite=TRUE)
        # Tag table
        RSQLite::dbCreateTable(con, "tags",
            c("file"="TEXT", level="INT", scan="INT", tag="INT", analyst="TEXT", analysisTime="TIMESTAMP"))
        RSQLite::dbDisconnect(con)
    }
}

#' Get Mapping of Tag Value to Tag Meaning
#'
#' A utility function, used by [ctdtag()] to get textual descriptions
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
getTagMapping <- function(dbname=getDatabaseName(), debug=0)
{
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

#' Get Tag Mapping from the Database
#'
#' @param file character value naming the CTD file.  Only items relating
#' to this file are returned.
#'
#' @template dbnameTemplate
#'
#' @template debugTemplate
#'
#' @return [getTagMapping()] returns a data frame holding the `code` and
#' `meaning` of each possible tag.
#'
#' @author Dan Kelley
#'
#' @export
getTags <- function(file=NULL, dbname=getDatabaseName(), debug=0)
{
    dmsg(debug, "getTags(file=\"", file, "\", dbname=\"", dbname, "\"\n")
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
removeTag <- function(file=NULL, level=NULL, dbname=NULL, debug=0)
{
    dmsg(debug, "removeTag(file=", file, ", level=", level, ", dbname=", dbname, "\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        dmsg(debug, "will remove ", paste(remove, collapse=" "), "-th tag\n")
        dmsg(debug, " BEFORE levels are: ", paste(tags$level, collapse=" "), "\n")
        tags <- tags[-remove, ]
        dmsg(debug, " AFTER  levels are: ", paste(tags$level, collapse=" "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    }
    RSQLite::dbDisconnect(con)
}

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
saveTag <- function(file=NULL, level=NULL, scan=NULL, tag=NULL, analyst=NULL, dbname=NULL, debug=0)
{
    # no checking on NULL; add that if we want to generalize
    dmsg(debug, "saveTag(..., level=", level, ", scan=", scan, ", tag=", tag, ", ...)\n")
    df <- data.frame(file=file, level=level, scan=scan, tag=tag, analyst=analyst, analysisTime=round(Sys.time()))
    if (debug > 0)
        print(df, file=stderr())
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

