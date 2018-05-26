# Build ---------------------------
.rs.restartR()
hgutils::startup("hgpackages/hgutils")
roxygen2::roxygenise()
hgutils::set_DESCRIPTION_imports(skip_prompt = TRUE)
devtools::document()
devtools::install()
devtools::check()
