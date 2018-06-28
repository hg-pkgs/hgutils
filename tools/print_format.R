library(hgutils)
load_common_packages()
load_packages("cli","crayon")

{
  spaces = paste0(rep(" ",100),collapse = "")
  packages = list_common_packages()
  SUCCESS=      "Loaded succesfully: "
  FAILED =      "Loading failed:     "
  REDUNDANT =   "Redundant packages: "
  DUPLICATED =  "Duplicate packages: "
  exdent = nchar(SUCCESS) + 2
  pb = progressbar(format="\u258f[\u2589][][\u2581]\u2595",refresh = 0.3, width = 20, n_iterations = length(packages))
  for (p in 1:length(packages)){
    pkg = packages[p]

    pb = update(pb, progress_iter = p)
    cat("\r",render(pb, show_iteration = TRUE),"loading",pkg,spaces)
    Sys.sleep(0.1)
  }
  cat(green("\r",spaces,"\r"))
  redundant = redundant_packages(c(packages,"survival","rms"))
  duplicates = hgutils:::.pkg_duplicated(c(packages,"rms","rms"))

  pkg_success = str_wrap(paste(packages,collapse = ", "),width=80,exdent=exdent)
  pkg_failed = str_wrap(paste(red(underline(packages)),collapse = ", "),width=200,exdent=exdent)
  cat_bullet(green(SUCCESS),pkg_success,bullet = "tick", bullet_col = "green")
  cat_bullet(red(FAILED),pkg_failed,bullet = "cross", bullet_col = "red")

  if(length(duplicates) > 0) {
    pkg_dupl = str_wrap(paste(underline(names(duplicates)),collapse = ", "),width=120,exdent=exdent)
    cat_bullet(yellow(DUPLICATED),pkg_dupl,bullet = "warning", bullet_col = "yellow")
  }
  if(length(redundant) > 0) {
    txt = sapply(names(redundant), function(x) paste0(underline(x), " (loaded by ", frmt(redundant[[x]]), ")"))
    spaces = paste0(rep(" ",nchar(REDUNDANT)+2),collapse = "")
    cat_bullet(yellow(REDUNDANT), paste0(txt,collapse = paste0("\n",spaces)),bullet_col = "yellow", bullet = "warning")
  }
}
