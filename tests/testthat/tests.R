context("time_estimate - NO NEWDATA")
suppressWarnings(library(rms))
library(hgutils)

test_that("Same estimates in time_estimate for survest and survfit with no newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)

    for (i in 1:100) {
        surv = runif(2)
        expect_identical(time_estimate(fit1, surv)$time, time_estimate(fit2, surv)$time)
    }
})

context("time_estimate - NEWDATA")
test_that("Same estimates in time_estimate for survest and survfit with newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)

    for (i in 1:100) {
        surv = runif(2)
        newdata = lung[sample(1:nrow(lung), 10 + round(runif(1,max=0.2) * (nrow(lung) - 10))), ]
        expect_identical(time_estimate(fit1, surv, newdata)$time, time_estimate(fit2, surv, newdata)$time)
    }
})

context("separate_values")
test_that("Values are nicely seperated and in range", {
    for (i in 1:100) {
        space = runif(1, 0, 0.49)
        max_n = floor(1/space)
        y0 = runif(2 + round(runif(1) * (max_n - 2)))

        res = separate_values(y0, space)
        expect_equal(res >= 0 && res <= 1, TRUE, info = res)

        expect_equal(all(sapply(1:length(res), function(x) abs(res[x] - res[-x])) >= space - 1e-04), TRUE)

        expect_error(separate_values(runif(max_n + 3), space))
    }
})

context("pkg_genre")
test_that("All packages in pkg_genre exist", {
  pg = hgutils::pkg_genre
  for(p in unlist(pg$packages))
  {
    expect_true(suppressWarnings(require(p, character.only = TRUE, quietly = TRUE)), info = p)
  }
})
