library(hgutils)

context("test-plot-utils.R")

test_that("seperate_values: values are nicely seperated and in range", {
  for (i in 1:50) {
    space = runif(1, 0.01, 0.49)
    max_n = round(1/space)
    y0 = runif(n=sample(c(2,2:max_n),1))
    res = separate_values(y0, space)

    expect_equal(all(res >= 0 & res <= 1), TRUE, info = res)
    expect_equal(all(sapply(1:length(res), function(x) abs(res[x] - res[-x])) >= space - 1e-04), TRUE)
    expect_error(separate_values(runif(max_n + 3), space))
  }
})

test_that("ggplot_breaks: get_bounds includes limits + size is ok", {
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

test_that("get_square_grid: grid is square and has enough rows and columns", {
  for(i in 1:100){
    g = get_square_grid(i)
    expect_true(g$rows >= g$columns)
    expect_true(g$rows*g$columns >= i)

    g2 = get_square_grid(i, moreRows = FALSE)
    expect_true(g2$rows <= g2$columns)
    expect_true(g2$rows*g2$columns >= i)
  }
})
