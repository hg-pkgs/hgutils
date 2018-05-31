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

limits=c(0,1001)
N = 10; max_breaks = 10; int_only = TRUE; strict = FALSE; use_outer=TRUE
#limits=c(0,100)

#sq = seq(0, ifelse(include_upper,ceiling(xmax/selected),floor(xmax/selected)) * selected, selected) + ceiling(xmin/selected) * selected

A=data.frame()
for(i in 1:1000)
{
  min=runif(1,0,100); max=runif(1,min,1000); N=round(runif(1,1,50)); max_breaks=round(runif(1,10,20))
  A = rbind(A,list(l=length(get_breaks(c(min,max), N, max_breaks = max_breaks,include_bounds = FALSE)),
                   max_breaks=max_breaks, min=min, max=max, N=N))
}
