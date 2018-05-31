# Build ---------------------------

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64", sep=.Platform$path.sep))

.rs.restartR()
hgutils::startup("hgpackages/hgutils")
hgutils::set_package_imports(skip_prompt = TRUE, use_version_numbers=FALSE, rversion="3.4.1")
roxygen2::roxygenise()
devtools::document()
devtools::run_examples(fresh=TRUE, run = FALSE)
devtools::check(check_version=FALSE)

installr::updateR(TRUE)
devtools::spell_check()
devtools::check_rhub()
devtools::release()
#devtools::build_manual()

remove.packages(c("readxl","writexl"))
.rs.restartR()
hgutils::use_common_packages()
