context("survTimeEstimate - NO NEWDATA")
suppressWarnings(library(rms))
library(HGUtils)

test_that("Same estimates in survTimeEstimate for survest and survfit with no newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)
    
    for (i in 1:100) {
        surv = runif(1)
        expect_identical(survTimeEstimate(fit1, surv)$time, survTimeEstimate(fit2, surv)$time)
    }
})

context("survTimeEstimate - NEWDATA")
test_that("Same estimates in survTimeEstimate for survest and survfit with newdata", {
    fit1 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = TRUE, y = TRUE, surv = TRUE)
    fit2 = cph(Surv(time = time, event = status == 2) ~ age + sex, data = lung, x = FALSE, y = FALSE, surv = TRUE)
    
    for (i in 1:100) {
        surv = runif(1)
        newdata = lung[sample(1:nrow(lung), 10 + round(runif(1) * (nrow(lung) - 10))), ]
        expect_identical(survTimeEstimate(fit1, surv, newdata)$time, survTimeEstimate(fit2, surv, newdata)$time)
    }
})

context("seperate_values")
test_that("Values are nicely seperated and in range", {
    for (i in 1:100) {
        space = runif(1, 0, 0.49)
        max_n = floor(1/space)
        y0 = runif(2 + round(runif(1) * (max_n - 2)))
        
        if (is.unsorted(y0)) 
            expect_error(seperate_values(y0, distance = space))
        
        y0 = sort(y0)
        res = seperate_values(y0, space)
        expect_equal(res >= 0 && res <= 1, TRUE)
        
        expect_equal(all(sapply(1:length(res), function(x) abs(res[x] - res[-x])) >= space - 1e-04), TRUE)
        
        expect_error(seperate_values(runif(max_n + 1), space))
    }
})
