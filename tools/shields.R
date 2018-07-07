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
  status="wip"
  dformat = "%d-%m-%Y"
  #dformat = "%B %m %Y"

  version = paste0("[![Package version](https://img.shields.io/badge/version-v",desc$Version,"-901913.svg)]()")
  min_r = paste0("[![minimal R version](https://img.shields.io/badge/R-",rvers,"+-blue.svg)](https://cran.r-project.org/)")
  last_update = paste0("[![last_update](https://img.shields.io/badge/last%20update-",
                       format(Sys.Date(), dformat) %>% str_replace_all("-","--"),"-8323a0.svg)]()")

  travis = paste0("[![Travis](https://travis-ci.org/",github_pkg,".svg)](https://travis-ci.org/",github_pkg,")")
  repo_status = paste0("[![Project Status](http://www.repostatus.org/badges/latest/",status,".svg)](http://www.repostatus.org/#",status,")")
  codecov = paste0("[![Codecov](https://img.shields.io/codecov/c/github/",github_pkg,".svg)](https://codecov.io/gh/",github_pkg,")")
  cran = paste0("[![CRAN](https://img.shields.io/cran/v/",desc$Package,".svg)](https://cran.r-project.org/package=",desc$Package,")")

  badges = paste0(paste0(c(version, min_r, last_update),collapse="\n"),"  \n",
                  paste0(c(travis, codecov),collapse="\n"),"  \n",
                  paste0(c(repo_status, cran),"\n",collapse = ""),"---")

  readme = paste0(readLines("README.Rmd"),collapse = "\n")
  if(!(str_detect(readme,"<!-- START_HGUTILS -->") && str_detect(readme,"<!-- END_HGUTILS -->"))) {
    readme = paste0("<!-- START_HGUTILS --><!-- END_HGUTILS -->\n",readme)
  }
  pieces=str_split(readme,"(?s)(?<=<!-- START_HGUTILS -->).*(?=<!-- END_HGUTILS -->)")[[1]]
  new_readme = paste0(pieces[1],"\n",badges,"\n",pieces[2]) %>% str_split("\n") %>% .[[1]]
  writeLines(new_readme, "README.Rmd")
}

library(XML)
library(magrittr)
library(stringr)
library(crayon)
url=c("https://api.travis-ci.org/hvdboorn/hgutils.svg",
"https://img.shields.io/badge/version-v0.0.0.9007-901913.svg",
"https://img.shields.io/badge/last%20update-06--07--2018-8323a0.svg",
"https://img.shields.io/cran/v/hgutils.svg",
"https://img.shields.io/codecov/c/github/hvdboorn/hgutils.svg",
"http://www.repostatus.org/badges/latest/wip.svg")

print_badge = function(url) {
  svg = suppressWarnings(readLines(url))
  data = xmlParse(svg)
  dl = xmlToList(data)

  fix_col = function(x) ifelse (str_detect(x,"^#[[:xdigit:]]{3}$"), str_replace_all(x,"([[:xdigit:]])","\\1\\1"), x)
  col_left = "#555" %>% fix_col %>% col2rgb
  col_right = str_match_all(svg,"<(?=path)[^<]*?fill=\"(#.*?)\"")[[1]][,2] %>% unique %>% tail(1) %>% fix_col %>% col2rgb
  txt_col = dl$g$.attrs['fill'] %>% fix_col %>% col2rgb
  gradient = dl$linearGradient$stop['stop-color'] %>% fix_col %>% col2rgb
  opacity = dl$linearGradient$stop['stop-opacity'] %>% as.numeric
  texts = str_match_all(svg,"<text.*?>(.*?)<\\/text>")[[1]][,2] %>% unique %>% paste0(" ", ., " ")

  to_linear = function(X) {
    sapply(X, function(x) {
      ifelse(x<=0.04045, x/12.92, ((x+0.055)/(1+0.055))^2.4)
    })
  }
  Y = t(to_linear(col_right/255)) %*% c(0.2126,0.7152,0.0722)
  s = rgb2hsv(col_right)['s',]

  cr = if(s>= 0.5 && Y >= 0.3) col2rgb("#000000") else col2rgb("#ffffff")#(mean(col_right)+128)%%256 %>% {col2rgb(rgb(.,.,.,maxColorValue = 255))}
  lft = function(x) make_style(col_left*0.9,bg=TRUE)(make_style(txt_col*0.97)(x))
  rgt = function(x) make_style(col_right,bg=TRUE)(make_style(cr*0.97)(x))
  cat(lft(texts[1]),rgt(texts[2]),sep = "")
}

for(u in url) {print_badge(u); cat("\n")}
