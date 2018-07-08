context("test-description-utils.R")

test_that("read.description/write.description/update_description", {
  desc = read.description()
  expect_true("Title" %in% names(desc))
  expect_equal(desc$Title, "TEST Description")

  write.description(desc)
  desc2 = read.description()

  expect_equal(names(desc), names(desc2))
  specials=c('Depends', 'Imports', 'Suggests')
  for(n in setdiff(names(desc), specials)) {
    expect_equal(desc[n], desc2[n])
  }
  for(n in specials) {
    if(n %in% names(desc)) {
      a = str_replace_all(sort(desc[[n]]),",","")
      b = str_replace_all(sort(desc2[[n]]),",","")
      c = str_replace_all(desc2[[n]],",","")
      expect_equal(b, c)
      expect_equal(a, c)
    }
  }

  N = as.character(round(runif(1,0,1000)))
  update_description("Random",N) #update field
  update_description(N, "Hello World!") #new field
  new_desc = read.description()
  expect_equal(new_desc[["Random"]], N)
  expect_equal(new_desc[[N]], "Hello World!")

  new_desc = new_desc[!grepl("^\\d+$", names(new_desc))]
  class(new_desc) = "description"
  write.description(new_desc)
})
