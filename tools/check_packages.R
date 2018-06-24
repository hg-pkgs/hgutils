#setwd("C:/Users/hgvandenboorn/Dropbox")
library(hgutils)
startup()
load_packages("magrittr","stringr")
pkgs = c('grid','gridExtra','htmltools','plotly','rms','shinyBS','shinydashboard','shinyjs','shinyWidgets','survival', 'ggplot2', 'scales')
pkgs = c('grid','gridExtra','htmltools')

print("Checking duplicate packages...")
dups = unique(pkgs[duplicated(pkgs)])
for (x in dups) print(sprintf("Package '%s' occurs %d times.", x, sum(pkgs==x)))
if (length(dups) == 0) print("No duplicates found")

print("Checking duplicate package attachments...")
redundant = lapply(pkgs, function(pkg) {
  desc = packageDescription(pkg)
  if ("Depends" %in% names(desc)) {sapply(pkgs, function(x) str_detect(desc$Depends, x)) %>% {names(.)[.]}} else NULL
}) %>% unlist %>% unique
print(ifelse(length(redundant) == 0, "No duplicate package attachments found", sprintf("Redundant packages: %s", frmt(redundant))))

suppressWarnings(invisible(sapply(pkgs, function(x) library(x, character.only = TRUE))))

intToUtf8(8942:8945) #three_dots https://mcdlr.com/utf-8/#9457
intToUtf8(9776:9783) #three_hor_lines
intToUtf8(9588:9599) #single lines, halve
intToUtf8(c(9609,9600,9604)) # blocks
as.hexmode(c(9776,9781,9783))
{
  files = list.files("../../source_webinterface/dev/",pattern = ".*\\.[Rr]$", recursive = TRUE, full.names = TRUE)
  content = paste0(sapply(files, function(x) paste0(readLines(x), collapse = "\n")), collapse = "\n")
  used_pkgs = c()
  n_functions = sum(sapply(pkgs, function(pkg) length(ls(sprintf("package:%s",pkg)))))
  function_index = 1
  loading_bar = create_progressbar(format="\u25ba[\u2589][\u2580\u2584][\u3000]\u25c4",update_speed = 150)
  function_usage = c()
  blue = make_style("dodgerblue4")
  for (pkg in pkgs)
  {
    functions = ls(sprintf("package:%s",pkg))
    res = c()
    for(x in functions) {
      loading_bar = update(loading_bar, function_index/n_functions)
      cat(blue(sprintf("\r%s Processing: %s%s", render(loading_bar), paste0(pkg), paste0(rep(" ",20), collapse = ""))))
      if(str_detect(content,paste0("\n[^#]*?\\b",pkg,"::[:]?(\\Q",x,"\\E)|\n[^#]*?\\b(\\Q",x,"\\E)\\("))) res=c(res,x)
      function_index = function_index+1
    }
    res %<>% rm_na %>% unlist %>% sort %>% list
    names(res) = pkg
    function_usage = c(function_usage, res)
    if(length(unlist(res)) > 0) used_pkgs = c(used_pkgs, pkg)
  }
  #cat("\rDone.",rep(" ",80))
  unused_packages = setdiff(pkgs, used_pkgs)
}
to_remove = unique(c(dups, redundant, unused_packages))

#implementation for load bar, with update.loadbar to get the current value


loading_bar = create_progressbar(format = "[*][][ ]")
loading_bar = update(loading_bar, 0.4)
print(loading_bar)

load_packages("stringr")
bar = "|[*][]|"
regex = "^(.*?)\\[(.)\\](?:\\[(.*?)\\])?\\[(.?)\\](.*)$"
regex2 = "^(.+)$"
str_detect(bar, regex)
str_detect("* ",regex2)
str_match(bar, regex)[-1]
