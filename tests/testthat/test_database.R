# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(ocetag)

test_that("database", {
    dbname <- getDatabaseName("test")
    expect_equal(dbname, paste0("test_", getUserName(), ".db"))
})

