# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(ocetag)

test_that("getDatabaseName()", {
    dbname <- getDatabaseName("test")
    expect_equal(dbname, paste0("test_", getUserName(), ".db"))
})

test_that("pluralize()", {
    expect_equal("1 dog", pluralize(1, "dog"))
    expect_equal("2 dogs", pluralize(2, "dog"))
    expect_equal("1 goose", pluralize(1, "goose"))
    expect_equal("2 geese", pluralize(2, "goose", "geese"))
})

# Mark the first point of a CTD dataset as bad, by tagging it as 2,
# which means "bad data" in the default tagging scheme.
test_that("saveTag() and getTags()", {
    dbname <- tempfile()
    createDatabase(dbname)
    saveTag("ctd.cnv", index = 1, tag = 2, analyst = "Dan Kelley", dbname = dbname)
    # sql <- system(paste("echo .dump|sqlite3 ", dbname), intern = TRUE)
    # cat(paste(sql, collapse = "\n"))
    tags <- getTags("ctd.cnv", dbname = dbname)
    expect_equal(tags$file, "ctd.cnv")
    expect_equal(tags$index, 1)
    expect_equal(tags$tag, 2)
    expect_equal(tags$analyst, "Dan Kelley")
    unlink(dbname)
})
