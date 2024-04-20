## ----echo = FALSE-------------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## -----------------------------------------------------------------------------
library(ocetag)
library(oce)
name <- "ctd.cnv"
f <- system.file("extdata", name, package = "ocetag")
d <- oce::read.oce(f)
plotProfile(d, xtype = "sigma0", type = "p", cex = 0.3)
# Change next to TRUE for interactive work
if (FALSE) {
    xy <- locator(1)
    usr <- par("usr")
    dx <- (xy$x - d[["sigma0"]]) / (usr[2] - usr[1])
    dy <- (xy$y - d[["pressure"]]) / (usr[4] - usr[3])
    index <- which.min(dx^2 + dy^2)
} else {
    index <- 12
}
points(d[["sigma0"]][index], d[["pressure"]][index], pch = 3, col = 2, cex = 2)
mtext(sprintf("Red cross at index %d", index), side = 1, col = 2, line = -1)

## -----------------------------------------------------------------------------
# Define the list of tags, here just one, for mixed-layer depth.
mapping <- list("MLD" = 1)

## -----------------------------------------------------------------------------
# Add 'pressure' to the columns in the default 'tags' table.
tags <- list("pressure" = "FLOAT")

## ----echo=FALSE---------------------------------------------------------------
dbname <- tempfile() # Do not use a tempfile() in actual work!!

## ----eval=FALSE---------------------------------------------------------------
#  dbname <- "tags.db"

## -----------------------------------------------------------------------------
createDatabase(dbname, mapping = mapping, tags = tags)

## -----------------------------------------------------------------------------
saveTag(name,
    index = index, pressure = d[["pressure"]][index],
    tag = 1, analyst = "Dan Kelley", dbname = dbname, debug=3
)

## -----------------------------------------------------------------------------
print(getTags(name, dbname = dbname))

## ----echo=FALSE---------------------------------------------------------------
unlink(dbname) # Do not erase your efforts in actual work!!

