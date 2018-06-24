library(hgutils)
startup("hgpackages/hgutils/")
use_common_packages()

fnames = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE)
files = fnames %>% sapply(. %>% readLines %>% paste0(collapse = "\n"))

rfiles = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE)[4]
ln = paste0(readLines(rfiles),collapse = "\n")
load_package_collection("processing")
str_match_all(ln,"\\\\code\\{\\\\link\\[(.*?)\\]\\{(.*?)\\}\\}")[[1]][,-1] %>% {apply(.,1,function(x) paste0(x,collapse = "::"))}
str_match_all(ln,"\\\\code\\{\\\\link\\{(.*?)\\}\\}")[[1]][,-1]
str_match_all(ln,"\\\\code\\{.*?\\}")
str_match_all(ln,"\\\\emph\\{.*?\\}")
str_match_all(ln,"\\\\strong\\{.*?\\}")
str_replace_all(ln,"\\\\code\\{(.*?)\\}","`\\1`")
str_replace_all(ln,"\\\\emph\\{(.*?)\\}","*\\1*")
str_replace_all(ln,"\\\\string\\{(.*?)\\}","**\\1**")
