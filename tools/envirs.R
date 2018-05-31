library(ggplot2)
search()
library(hgutils)

# to_run <- eval(substitute(function() devtools::run_examples(pkg = path,
#                                                             start = start, test = test, run = run, fresh = FALSE),
#                           list(path = pkg$path, start = start, test = test,
#                                run = run)))

e = parent.env(environment())
library(acepack)

where("ace")

e = new.env(parent=environmentName())
vec=c(1,2,3,NA)
to_run <- eval(substitute(function() {remove.packages("writexl"); hgutils::use_common_packages()},NULL))
callr::r(to_run, show = TRUE, spinner=FALSE)
