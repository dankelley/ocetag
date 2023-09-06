#' Optionally print a debugging message
#'
#' @param debug integer value indicating whether to print the message. If this
#' is 0 or negative, then nothing is printed. If it exceeds 0, the contents of
#' `...` are printed.
#'
#' @param \ldots material to be printed.
#'
#' @author Dan Kelley
#'
#' @export
dmsg <- function(debug, ...)
    if (debug > 0) cat(file=stderr(), ..., sep="")

#' Get user name
#'
#' @return [getUserName()] returns a character value that is the user's
#' login name on this computer.
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

#' Pluralize an item
#'
#' This is useful for saying e.g. "0 apples", "1 apple", or "10 apples".
#'
#' @param n integer giving the number of instances of the item.
#'
#' @param singular character value giving the name of the item.
#'
#' @param plural optional character value giving the plural name.
#' If this is not supplied, it is constructed by appending the letter
#' "s" to `singular`.  This lets the output be e.g. "3 geese"
#' instead of "3 gooses".
#'
#' @author Dan Kelley
#'
#' @export
pluralize <- function(n=1, singular="item", plural=NULL)
{
    singular <- paste(n, singular)
    if (is.null(plural))
        plural <- paste0(singular, "s")
    if (n == 1L) singular else plural
}

