# Build ---------------------------

Sys.setenv(PATH = paste(Sys.getenv("PATH"), "C:/Program Files/MiKTeX 2.9/miktex/bin/x64",
                        sep=.Platform$path.sep))
#install.packages("badgecreatr")
.rs.restartR()
hgutils::startup()
source("tools/shields.R")
hgutils::crossref_description(skip_prompt=TRUE, use_version_numbers=FALSE,
                              rversion="DEPENDENCIES_VERSION", update=TRUE)
devtools::document()
devtools::spell_check(dict="en_US")
devtools::run_examples(fresh=TRUE, run = FALSE)
devtools::test()
devtools::check()
hgutils::update_description("Date",format(Sys.Date(),"%Y%-%m-%d"))
add_shields()
#installr::updateR(TRUE)
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_rhub()
devtools::release()
#devtools::build_manual()
