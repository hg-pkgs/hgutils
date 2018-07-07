context("test-extension-utils.R")

test_that(".package_duplicated", {
  dups = hgutils:::.pkg_duplicated(c("rms","dplyr","rms"))
  expect_true("rms" %in% names(dups))
  expect_equal(dups$rms, 2)
})

test_that("generic_implementations", {

  impls = generic_implementations('plot')
  expect_true("ecdf" %in% impls)
  expect_false("default" %in% impls)

  impls2 = generic_implementations('plot',remove_default = FALSE)
  expect_false("ecdf2" %in% impls2)
  expect_true("default" %in% impls2)
})

test_that("update_settings", {
  foo = function(...) {
    default = list(a=1)
    settings = update_settings(default, ...)

    settings$a
  }

  expect_equal(foo(), 1)
  expect_equal(foo(a=2), 2)
  expect_warning(foo(a=2,b=1))
})
