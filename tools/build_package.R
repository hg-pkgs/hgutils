# Build ---------------------------

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Program Files/MiKTeX 2.9/miktex/bin/x64",
                        sep=.Platform$path.sep))

#load_packages("spelling")
.rs.restartR()
library(hgutils)
hgutils::startup()
hgutils::crossref_description(skip_prompt=TRUE, use_version_numbers=FALSE,
                              rversion="DEPENDENCIES_VERSION", update=TRUE)
devtools::document()
devtools::spell_check()
devtools::run_examples(fresh=TRUE, run = FALSE)
devtools::test()
devtools::check()

update_description("Date", format(Sys.Date(), "%Y%-%m-%d"))
add_badges("hvdboorn/hgutils")


#installr::updateR(TRUE)
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_rhub()
devtools::release()
