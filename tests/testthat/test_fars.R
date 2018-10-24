# test_dir() or test_file() to combine several tests

# only one test_that made for this package, because data is not included in inst directory

testthat::test_that("filename generated", {
  testthat:expect_that(make_filename(as.integer(2010)), testthat::is_a(character))
})
