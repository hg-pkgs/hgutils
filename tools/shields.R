# Start-up ----------------------------------------------------------------
library(hgutils)
startup()
load_package_collection()

add_shields = function() {
  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n") %>% str_split("\n(?=.*?:)") %>% .[[1]] %>%
         lapply(. %>% str_split(":")) %>%
         {sapply(., function(x) list(str_replace_all(x[[1]][2],"^ +","")) %>% set_names(x[[1]][1]))}
  github = "https://github.com/hvdboorn/hgutils/"


  # Processing -----------------------------------------------------------------

  states = c("abandoned","active","concept","inactive","moved","suspended","unsupported","wip")
  github_pkg = str_match(github,"github\\.com\\/([^\\/]*\\/[^\\/]*)")[-1]
  is_dev = str_extract(desc$Version,"(?<=[\\.-])\\d+$") %>% as.numeric %>% {. >= 9000}
  rvers  = str_match(desc$Depends,"R[ ]+\\(>=[ ]+(.*)\\)")[-1]
  status="wip"
  dformat = "%Y-%m-%d"
  #dformat = "%B %m %Y"

  version = paste0("[![Package version](https://img.shields.io/badge/version-v",desc$Version,"-",ifelse(is_dev,"lightgrey","blue"),".svg)](/blob/master/README.Rmd)")
  travis = paste0("[![Travis](https://travis-ci.org/",github_pkg,".svg)](https://travis-ci.org/",github_pkg,")")
  codecov = paste0("[![Codecov](https://img.shields.io/codecov/c/github/",github_pkg,".svg)](https://codecov.io/gh/",github_pkg,")")
  min_r = paste0("[![minimal R version](https://img.shields.io/badge/R-",rvers,"+-blue.svg)](https://cran.r-project.org/)")
  repo_status = paste0("[![Project Status](http://www.repostatus.org/badges/latest/",status,".svg)](http://www.repostatus.org/#",status,")")
  last_update = paste0("[![last_update](https://img.shields.io/badge/last%20update-",
                       format(Sys.Date(), dformat) %>% str_replace_all("-","--"),"-orange.svg)](commits/master)")

  badges = paste0(paste0(c(version, last_update, min_r),collapse="\n"),"  \n",
                  paste0(c(repo_status,travis, codecov),"\n",collapse = ""),"---")

  readme = paste0(readLines("README.Rmd"),collapse = "\n")
  if(!(str_detect(readme,"<!-- START_HGUTILS -->") && str_detect(readme,"<!-- END_HGUTILS -->"))) {
    readme = paste0("<!-- START_HGUTILS --><!-- END_HGUTILS -->\n",readme)
  }
  pieces=str_split(readme,"(?s)(?<=<!-- START_HGUTILS -->).*(?=<!-- END_HGUTILS -->)")[[1]]
  new_readme = paste0(pieces[1],"\n",badges,"\n",pieces[2]) %>% str_split("\n") %>% .[[1]]
  writeLines(new_readme, "README.Rmd")
}
