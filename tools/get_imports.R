library(hgutils)
load_package_collection("processing")
files = list.files("R","\\.[Rr]$", recursive = TRUE, full.names = TRUE)
content = lapply(files, function(x) paste0(readLines(x), collapse = "\n")) %>% set_names(str_match(files, "/(.*?\\.R)$")[,-1])

processed = lapply(content, function(c) {
  str_match_all(c,"((?:#'.*?\n)+)[\n]*((?:(?!#').*\n)+)")[[1]][,-1] %>%
  {cbind(str_match(.[,2], "\\b(.*?)[ ]*=")[,-1], .)} %>%
  as.data.frame(stringsAsFactors=FALSE) %>% set_names(c("func_name","doc","function"))
})
