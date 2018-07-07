library(hgutils)

context("test-loading-animations.R")

test_that("progressbar: correct visualization", {
  bar = progressbar(n_iterations = 10,format = "[[*][][ ]]")
  bar2 = progressbar(format = "[[*][][ ]]")
  expect_equal(render(bar,progress=0,show_progress="iteration"),"[                    ] [0/10]")
  expect_equal(render(bar,progress=5,show_progress="iteration"),"[**********          ] [5/10]")
  expect_equal(render(bar,progress=5,show_progress="percentage"),"[**********          ] 50%")
  expect_error(render(bar2,progress=0,show_progress="iteration"))
  expect_equal(render(bar2,progress=0,show_progress="percentage"),"[                    ] 0%")
  expect_equal(render(bar2,progress=0.5),"[**********          ]")
})

test_that("spinner: correct characters", {
  sp = spinner("12",refresh = 100)
  a = render(sp)
  Sys.sleep(0.1)
  b = render(sp)
  expect_true(a!=b)
  expect_equal(paste0(sort(c(a,b)),collapse = ""),"12")
})
