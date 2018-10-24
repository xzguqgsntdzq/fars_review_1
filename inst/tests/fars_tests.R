# test_dir() or test_file() to combine several tests

# only one test_that made for this package, because data is not included in inst directory

test_that("filename generated", {
  expect_that(make_filename(as.integer(2010)), is.a(character))
})
