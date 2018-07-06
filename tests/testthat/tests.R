library(hgutils)
library(magrittr)
library(rms)
library(stringr)
library(crayon)

context("time_estimate - NO NEWDATA")
test_that("Same estimates in time_estimate for survest and survfit with no newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)

    for (i in 1:25) {
        surv = runif(2)
        expect_identical(time_estimate(fit1, surv)$time, time_estimate(fit2, surv)$time)
    }
})

context("time_estimate - NEWDATA")
test_that("Same estimates in time_estimate for survest and survfit with newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)

    for (i in 1:25) {
        surv = runif(2)
        newdata = lung[sample(1:nrow(lung), 10 + round(runif(1,max=0.2) * (nrow(lung) - 10))), ]
        expect_identical(time_estimate(fit1, surv, newdata)$time, time_estimate(fit2, surv, newdata)$time)
    }
})

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

context("rm_na")
test_that("All NA is removed", {
  a=c(1,2,3,NA,4,5,6,NA,7,8,NA,9,10)
  expect_equal(length(rm_na(a)), 10)
  expect_equal(1:10, rm_na(a))
  expect_equal(sort(rm_na(a)), rm_na(a))
})

context("valid names")
test_that("valid package names", {expect_true(all(valid_pkgname(rm_na(str_match(search(),"package\\:(.*)$")[,2]))))})
# test_that("valid function names", {
#   pkgs=rm_na(str_match(search(),"package\\:(.*)$")[,1])
#   functions = sapply(pkgs,lsf.str) %>% unlist %>% unique
#   expect_true(all(valid_funcname(functions)))
# })
