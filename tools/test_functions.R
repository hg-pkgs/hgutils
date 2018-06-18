library(hgutils)
startup("hgpackages/hgutils/")
use_common_packages()

fnames = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE)
files = fnames %>% sapply(. %>% readLines %>% paste0(collapse = "\n"))

#rox = str_match_all(f,"[^#].*?\n((?:#\\'.*?\n)+)[^#].*?")[[1]] #extract roxygens

#support for generics
for (i in 1:length(files))
{
  file = files[i]
  fname = fnames[i]
  cat_rule(left=sprintf("Checking redundancies..."), right=fname, col="white", background_col="dodgerblue4", line="bar4")

  funs = str_match_all(file,"[^#].*?\n((?:#\\'.*?\n)+(?:[^#].*?\n)+\\})")[[1]][,-1] #extract function+roxygen
  info = list()
  for (f in funs)
  {
    splt = str_match_all(f,"((?:#.*?\n)+)((?:[^#].*?\n)+\\})")[[1]][,-1]
    ref = str_match_all(f,"(?:@describeIn|@rdname) (.*?)[ \n]")[[1]][-1] %>% {ifelse(length(.)==0,NA,.)}
    name = str_match_all(f,"@name (.*?)[ \n]")[[1]][-1] %>% {ifelse(length(.)==0,NA,.)}

    f_name = str_match(splt[2], "^(.*?)[ ]*=[ ]*function")[-1]
    M = str_match_all(splt[1], "@importFrom (.*?) (.*)\n")[[1]][,-1]
    if (!is.matrix(M)) M=as.matrix(t(M))
    M = apply(M, 1, function(x) c(pkg=x[1], methods=list(str_split(x[2]," ")[[1]])))

    pkg_fun = c()
    fun = c()
    for (p in M)
    {
      miss_fun = p$methods[!str_detect(splt[2], fixed(p$methods))]
      if (length(miss_fun) > 0){
        fun = c(fun, p$methods)
        pkg_fun = c(pkg_fun, paste0(p$pkg,"::",p$methods))
      }
    }
    new_info = list(fname=f_name, roxygen=splt[1], code=splt[2], imports=M, pkg_fun=pkg_fun, fun=fun, ref=ref, name=name)
    info = if (length(info)==0) list(new_info) else append(info,list(new_info))
  }

  n_found = 0
  #second pass
  for (res in info)
  {
    pkg_fun = res$pkg_fun
    fun = res$fun
    if (!is.null(pkg_fun) && length(pkg_fun) > 0)
    {
      comb = sapply(info, function(x) x$code)[sapply(info, function(x) x$ref==res$fname||x$ref==res$fname||x$ref==res$name||x$fname==res$fname)]
      comb = paste0(comb,collapse = "\n")
      found = str_detect(comb, fixed(fun))
      res$pkg_fun = res$pkg_fun[!found]
      if (length(res$pkg_fun) == 0) {
        cat_bullet(res$fname,"() ", crayon::green(symbol$tick), col="dodgerblue4", bullet_col = "dodgerblue4")
      } else {
        cat_bullet(res$fname,"() ", background_col = "red", col="white", bullet_col = "white")
        cat_bullet("  ", res$pkg_fun, col="red", bullet = "continue", bullet_col = "red")
        n_found = n_found+length(res$pkg_fun)
      }
    } else {
      cat_bullet(res$fname,"() ", crayon::green(symbol$tick), col="dodgerblue4", bullet_col = "dodgerblue4")
    }
  }

  if (n_found==0) {
    cat_bullet("  No redundancies found. ", background_col = "green", col="white", bullet_col = "white", bullet="tick")
  } else {
    cat_bullet(sprintf("  %s redundanc%s found. ",n_found,ifelse(n_found==1,"y","ies")),
               background_col = "yellow", col="black", bullet_col = "black", bullet="arrow_right")
  }

  #replace as follows
  # A=str_replace(file,"((?:.*\n)+.*\n)(.*@importFrom dplyr .*)((?:.*?\n)+time_estimate[ ]*?=[ ]*?function\\((?:.*\n)+)","\\1____\\3")
  # writeLines(A,"output.txt")

  if (i < length(files))
    cat("\n")
}

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
