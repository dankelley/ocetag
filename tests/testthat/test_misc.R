# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(ocetag)

test_that("database", {
    dbname <- getDatabaseName("test")
    expect_equal(dbname, paste0("test_", getUserName(), ".db"))
})

test_that("pluralize", {
    expect_equal("1 dog", pluralize(1, "dog"))
    expect_equal("2 dogs", pluralize(2, "dog"))
    expect_equal("1 goose", pluralize(1, "goose"))
    expect_equal("2 geese", pluralize(2, "goose", "geese"))
})

