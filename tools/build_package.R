# Build ---------------------------

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:\\Program Files\\MiKTeX 2.9\\miktex\\bin\\x64", sep=.Platform$path.sep))

.rs.restartR()
hgutils::startup("hgpackages/hgutils")
roxygen2::roxygenise()
hgutils::set_DESCRIPTION_imports(skip_prompt = TRUE)
devtools::document()
devtools::run_examples(fresh=TRUE, run = FALSE)
devtools::check(check_version=FALSE) #again!

installr::updateR(TRUE)
devtools::spell_check()
devtools::check_rhub()
devtools::release()
#devtools::build_manual()
#cli?
