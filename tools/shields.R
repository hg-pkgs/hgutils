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

  version = paste0("[![Package version](https://img.shields.io/badge/version-v",desc$Version,"-blue.svg)]()")
  min_r = paste0("[![minimal R version](https://img.shields.io/badge/R-",rvers,"+-blue.svg)](https://cran.r-project.org/)")
  last_update = paste0("[![last_update](https://img.shields.io/badge/last%20update-",
                       format(Sys.Date(), dformat) %>% str_replace_all("-","--"),"-red.svg)]()")

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
"https://img.shields.io/badge/version-v0.0.0.9007-blue.svg",
"https://img.shields.io/badge/last%20update-07--07--2018-red.svg",
"https://img.shields.io/cran/v/hgutils.svg",
"https://img.shields.io/codecov/c/github/hvdboorn/hgutils.svg",
"http://www.repostatus.org/badges/latest/wip.svg",
"https://img.shields.io/badge/R-3.1+-blue.svg")

badge2text = function(svg_badge_url, full_color=TRUE) {
  if(!str_detect(svg_badge_url,"https?://.*\\.svg$"))
    stop("Argument 'svg_badge_url' must be a valid url to an .svg file")
  svg = suppressWarnings(readLines(svg_badge_url))
  dl = svg %>% xmlParse %>% xmlToList

  texts = str_match_all(svg,"<text.*?>(.*?)<\\/text>")[[1]][,2] %>% unique %>% paste0(" ", ., " ")
  badge = if(!full_color) {
    paste0(texts, collapse = "|")
  } else {
    repair_hex = function(x) ifelse (str_detect(x,"^#[[:xdigit:]]{3}$"), str_replace_all(x,"([[:xdigit:]])","\\1\\1"), x)
    to_linear = function(X) sapply(X, function(x) ifelse(x<=0.04045, x/12.92, ((x+0.055)/(1+0.055))^2.4))
    left_box = function(x) make_style(col_left*0.9,bg=TRUE)(make_style(txt_col*0.97)(x))
    right_box = function(x, text_color) make_style(col_right,bg=TRUE)(make_style(text_color*0.97)(x))

    col_left = col2rgb("#555555")
    col_right = str_match_all(svg,"<(?=path)[^<]*?fill=\"(#.*?)\"")[[1]][,2] %>% unique %>%
                tail(1) %>% repair_hex %>% col2rgb %>% multiply_by(0.9)
    txt_col = dl$g$.attrs['fill'] %>% repair_hex %>% col2rgb

    Y = t(to_linear(col_right/255)) %*% c(0.2126,0.7152,0.0722)
    s = rgb2hsv(col_right)['s',]

    tcolor = if(s >= 0.5 && Y >= 0.3) col2rgb("#000000") else col2rgb("#ffffff")
    paste0(left_box(texts[1]), right_box(texts[2],tcolor), sep = "")
  }

  class(badge) = "badge"
  badge
}

print.badge = function(x, ...) {
  cat(x)
  invisible(x)
}

for(u in url) cat(badge2text(u,full_color = TRUE))
