# Start-up ----------------------------------------------------------------
library(hgutils)
startup()
load_package_collection()

#cat(bgBlack(white(" build ")), bgGreen(white(" passing ")),sep = "")

add_shields = function() {
  github = "https://github.com/hvdboorn/hgutils/"
  desc = read.description()
  states = c("abandoned","active","concept","inactive","moved","suspended","unsupported","wip")
  github_pkg = str_match(github,"github\\.com\\/([^\\/]*\\/[^\\/]*)")[-1]
  is_dev = str_extract(desc$Version,"(?<=[\\.-])\\d+$") %>% as.numeric %>% {. >= 9000}
  rvers  = str_match(desc$Depends,"R[ ]+\\(>=[ ]+(.*)\\)")[-1]
  status="active"
  dformat = "%Y%-%m-%d"
  #dformat = "%B %m %Y"

  version = paste0("[![Package version](https://img.shields.io/badge/GitHub-",desc$Version,"-orange.svg)]()")
  min_r = paste0("[![minimal R version](https://img.shields.io/badge/R-v",rvers,"+-blue.svg)](https://cran.r-project.org/)")
  last_update = paste0("[![last_update](https://img.shields.io/badge/last%20update-",
                       format(Sys.Date(), dformat) %>% str_replace_all("-","--"),"-blue.svg)]()")

  travis = paste0("[![Travis](https://travis-ci.org/",github_pkg,".svg)](https://travis-ci.org/",github_pkg,")")
  repo_status = paste0("[![Project Status](http://www.repostatus.org/badges/latest/",status,".svg)](http://www.repostatus.org/#",status,")")
  codecov = paste0("[![Codecov](https://img.shields.io/codecov/c/github/",github_pkg,".svg)](https://codecov.io/gh/",github_pkg,")")
  cran = paste0("[![CRAN](http://www.r-pkg.org/badges/version/",desc$Package,")](https://cran.r-project.org/package=",desc$Package,")")

  badges = paste0(paste0(c(repo_status, cran, version, min_r, last_update),collapse="\n"),"  \n",
                  paste0(c(travis, codecov),collapse="\n"),"\n---")

  readme = paste0(readLines("README.Rmd"),collapse = "\n")
  if(!(str_detect(readme,"<!-- START_HGUTILS -->") && str_detect(readme,"<!-- END_HGUTILS -->"))) {
    readme = paste0("<!-- START_HGUTILS --><!-- END_HGUTILS -->\n",readme)
  }
  pieces=str_split(readme,"(?s)(?<=<!-- START_HGUTILS -->).*(?=<!-- END_HGUTILS -->)")[[1]]
  new_readme = paste0(pieces[1],"\n",badges,"\n",pieces[2]) %>% str_split("\n") %>% .[[1]]
  writeLines(new_readme, "README.Rmd")
}

# library(XML)
# library(magrittr)
# library(stringr)
# library(crayon)
# library(hgutils)
# url=c("https://api.travis-ci.org/hvdboorn/hgutils.svg",
# "https://img.shields.io/badge/version-v0.0.0.9007-blue.svg",
# "https://img.shields.io/badge/last%20update-07--07--2018-red.svg",
# "https://img.shields.io/cran/v/hgutils.svg",
# "https://img.shields.io/codecov/c/github/hvdboorn/hgutils.svg",
# "http://www.repostatus.org/badges/latest/wip.svg",
# "https://img.shields.io/badge/R-3.1+-blue.svg")
#
# badges = url %>% lapply(. %>% badge2text)
# do.call(cat, c(badges, sep="\n"))
