library(hgutils)
startup()
load_packages("stringr", "magrittr", "dplyr")

files = list.files("../../SOURCE/source_webinterface/source/", pattern="\\.[rR]$", full.names = TRUE, recursive = TRUE)
#files = list.files("../../SOURCE/survival-SOURCE/survival-SOURCE/util/", pattern="rename\\.[rR]$", full.names = TRUE)
#files = list.files("./tools/", pattern="loading\\.[rR]$", full.names = TRUE)
#files = list.files("../../SOURCE/survival-SOURCE/survival-SOURCE/util/", pattern=".[rR]$", full.names = TRUE)


a = analyze_package_imports(files)
