library(hgutils)
library(magrittr)
library(stringr)
library(crayon)

context("separate_values")
test_that("Values are nicely seperated and in range", {
    for (i in 1:50) {
        space = runif(1, 0, 0.49)
        max_n = floor(1/space)
        y0 = runif(2 + round(runif(1) * (max_n - 2)))

        res = separate_values(y0, space)
        expect_equal(res >= 0 && res <= 1, TRUE, info = res)

        expect_equal(all(sapply(1:length(res), function(x) abs(res[x] - res[-x])) >= space - 1e-04), TRUE)

        expect_error(separate_values(runif(max_n + 3), space))
    }
})

context("ggplot bounds")
test_that("get_bounds includes limits", {
  for (i in 1:250){
    lower = round(runif(1,1,20))
    upper = round(runif(1,0,20))+lower

    breaks = ggplot_breaks(include_bounds=TRUE)(c(lower,upper))
    breaks2 = ggplot_breaks(include_bounds=FALSE)(c(lower,upper))
    expect_true(lower >= min(breaks) && upper <= max(breaks))
    expect_true(length(breaks) <= 12 && length(breaks) >= 0)
    expect_true(length(breaks2) <= 11 && length(breaks2) >= 0)
  }
})

context("Title bar")
test_that("Length equals 80 and regex", {
  left = "Test case"
  bar = hgutils:::.get_title_bar(left)
  expect_equal(col_nchar(bar),80)
  expect_true(str_detect(bar,"^== .*? =+ .*? ==$"))

  bar = hgutils:::.get_title_bar()
  expect_equal(col_nchar(bar),80)
  expect_true(str_detect(bar,"^=+ .*? ==$"))
})

context("Misc utils")
test_that("All NA is removed", {
  a=c(1,2,3,NA,4,5,6,NA,7,8,NA,9,10)
  expect_equal(length(rm_na(a)), 10)
  expect_equal(1:10, rm_na(a))
  expect_equal(sort(rm_na(a)), rm_na(a))
})

test_that("valid package names", {expect_true(all(valid_pkgname(rm_na(str_match(search(),"package\\:(.*)$")[,2]))))})
test_that("Empty workspace", {expect_true({startup(verbose = FALSE); length(ls())==0})})
test_that("Square grid", {
  for(i in 1:100){
    g = get_square_grid(i)
    expect_true(g$rows >= g$columns)
    expect_true(g$rows*g$columns >= i)
  }
})
test_that("Round double",{expect_equal(rnd_dbl(1.26564,digits = 2),"1.27")})
test_that("Round double",{expect_equal(rnd_dbl(1.2,digits = 2),"1.20")})
test_that("Format duration", {s=Sys.time(); expect_equal(format_duration(s,s+90),"[~1.5 min.]")})
test_that("Format duration", {s=Sys.time(); expect_equal(format_duration(s,s+.90),"[900 ms.]")})
test_that("Frmt", {expect_equal(frmt(c(1,2,3)),"['1','2','3']")})
test_that("Frmt", {expect_equal(frmt(1),"'1'")})
test_that("Frmt", {expect_equal(frmt(1, show_class = TRUE),"'1' (class: numeric)")})
test_that("Remove empty rows", {
  data = rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(NA, NA, NA), c(4, 8, NA))
  data = rm_empty_rows(data)
  expect_equal(data, rbind(c(1,2,3), c(1, NA, 4), c(4,6,7), c(4, 8, NA)))
})
