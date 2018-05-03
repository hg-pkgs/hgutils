context("Quick tests")
library(HGUtils)

test_that("Breaks", {
  expect_error(get_breaks(),regexp = "^.*argument \"limits\" is missing.*$")
  expect_lte( length(get_breaks(10)), 10+1)
  expect_equal(get_breaks(10),0:10)
  expect_lte( length(get_breaks(c(41,54546),max_breaks = 16)), 16+1)
  expect_equal(all(get_breaks(24, strict = T)%%10 == 0), T)
})
