library(hgutils)
library(stringr)
library(crayon)

context("test-misc-utils.R")

test_that(".get_title_bar: Length equals 80 and regex", {
  left = "Test case"
  bar = hgutils:::.get_title_bar(left)
  expect_equal(col_nchar(bar),80)
  expect_true(str_detect(bar,"^== .*? =+ .*? ==$"))

  bar = hgutils:::.get_title_bar()
  expect_equal(col_nchar(bar),80)
  expect_true(str_detect(bar,"^=+ .*? ==$"))
})

test_that("rm_na: All NA is removed", {
  a=c(1,2,3,NA,4,5,6,NA,7,8,NA,9,10)
  expect_equal(length(rm_na(a)), 10)
  expect_equal(1:10, rm_na(a))
  expect_equal(sort(rm_na(a)), rm_na(a))
})

test_that("rnd_dbl: Round double",{
  expect_equal(rnd_dbl(1.26564,digits = 2),"1.27")
  expect_equal(rnd_dbl(1.2,digits = 2),"1.20")
})
test_that("format_duration: correct formatting", {
  s=Sys.time()
  expect_equal(format_duration(s,s+.999),"[999 ms.]")
  expect_equal(format_duration(s,s+59),"[59.00 sec.]")
  expect_equal(format_duration(s,s+59.9),"[59.90 sec.]")
  expect_equal(format_duration(s,s+59.001),"[59.00 sec.]")
  expect_equal(format_duration(s,s+86399),"[~1440.0 min.]")
  expect_equal(format_duration(s,s+86400),"[~1.0 days]")
})
test_that("frmt: correct formatting", {
  expect_equal(frmt(c(1,2,3)),"['1','2','3']")
  expect_equal(frmt(1),"'1'")
  expect_equal(frmt(1, show_class = TRUE),"'1' (class: numeric)")
})

test_that("rm_empty_rows: empty rows are gone", {
  data = rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(NA, NA, NA), c(4, 8, NA))
  data = rm_empty_rows(data)
  expect_equal(data, rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(4, 8, NA)))
})
