#' Load and install packages
#'
#' @description Utility function to load and optionally install packages if they are missing. When the function terminates,
#' packages are installed (if necessary) and loaded. Upgradeable packages are shown.
#'
#' @param ... list of package names.
#' @param install_packages whether to install the selected packages.
#' @param force_install whether to install packages even if they are installed already.
#' @param show_outdated_packages whether to show a list of packages which are outdated.
#' @param default_loading_method load according to the default R method using only \code{library()}
#' @param return_library_statements makes this function only return a string containing \code{library()} statements which can be paste into an R script.
#'
#' @details
#' \code{load_packages} optionally installs, upgrades and attaches packages to the work space for a list of specified packages.
#'
#'
#' @return Returns invisibly a list with additional package information and results of installing/upgrading and loading.
#' @seealso
#' \code{\link[utils]{install.packages}} for installation of new packages,
#' \code{\link[utils]{update.packages}} for updating outdated packages,
#' \code{\link[base]{library}} for load and attaching packages.
#'
#' @examples \dontrun{
#' # Package names given one-by-one or in a vector
#' load_packages(c('magrittr', 'dplyr'))
#' load_packages('magrittr', 'dplyr')
#'
#' # Package names may be unquoted
#' load_packages(magrittr, dplyr)
#' load_packages('magrittr','dplyr', install_packages=FALSE)
#' }
#'
#' @export
#' @family developer functions
#'
#' @importFrom utils install.packages old.packages update.packages compareVersion installed.packages
#' @importFrom crayon underline
#' @importFrom magrittr set_rownames
load_packages = function(..., install_packages = TRUE, force_install = FALSE, show_outdated_packages=TRUE,
                         default_loading_method=FALSE, return_library_statements=FALSE) {
  oldw <- getOption("warn")
  options(warn = -1)
  start = Sys.time()
  #-- Check for extra arguments in '...' -------
  packages = list(...) %>% unlist
  duplicates = .pkg_duplicated(packages)

  #-- Check for invalid package names in '...' -------
  packages = packages %>% unique %>% sort
  invalid_names = packages[!valid_pkgname(packages)]
  if(length(invalid_names) > 0)
    stop(sprintf("The argument '...' contains the following invalid package names: %s.", paste0(invalid_names,collapse = ", ")))

  found = sapply(packages, function(x) length(find.package(x, quiet = TRUE)) > 0)
  installed = names(found)[found]
  not_installed = names(found)[!found]

  #-- Define constants -------
  bull = .bullets()
  show_progress = "iteration"
  spaces = paste0(rep(" ",80),collapse = "")
  SUCCESS=         "Loaded succesfully:   "
  UPGRADED=        "Upgraded succesfully: "
  UPGRADE_FAIL=    "Upgrading failed:     "
  FAILED =         "Loading failed:       "
  REDUNDANT =      "Redundant packages:   "
  DUPLICATED =     "Duplicate packages:   "
  CONSIDER_UPGR =  "Consider upgrading:   "
  exdent = nchar(SUCCESS) + 3
  redundant = redundant_packages(packages)
  success = c(); fail=c(); upgraded=c(); upgrade_fail=c()
  n_packages = length(packages)
  prog = if(length(setdiff(packages,installed.packages()[,"Package"])) > 0){
    progressbar(format=">[*][][ ]<",refresh = 1/24, width = min(max(10,n_packages),20), n_iterations = n_packages)
  } else {
    spinner(refresh = 1/24)
  }

  if(return_library_statements || default_loading_method) {
    text = paste0("library(", installed,")", collapse = "; ")
    ni = wrap_text_table(not_installed, exdent) %>% str_replace_all("(\\w+)",.cwarn(underline("\\1")))

    if(length(not_installed) > 0)
      cat(bull$warn, "Skipped packages:     ", ni, "\n",sep="")
    options(warn = oldw)
    if (return_library_statements) {
      return(text)
    } else {
      eval(parse(text = text))
      return(invisible())
    }
  }

  #-- show title -------
  left = sprintf("Loading packages (total: %s package%s)",length(packages), ifelse(length(packages)>1,"s",""))
  cat(.get_title_bar(left),"\n")

  cat("\r",render(prog, progress=0)," Retrieving package info...",spaces, sep = "")

  consider_upgrade = c()
  if(show_outdated_packages) {
    inst = installed.packages() %>% set_rownames(NULL)
    inst = inst[order(package_version(inst[,"Version"]),decreasing = TRUE),]
    current_versions = lapply(installed, function(x) {vers = inst[inst[,"Package"]==x,];
    if(is.null(nrow(vers))) {
      vers
    } else if (nrow(vers)==0){
      NULL
    } else {
      vers[1,]
    }}) %>% do.call(rbind,.)

    if(!is.null(current_versions) && nrow(current_versions) > 0) {
      outdated_pkgs = old.packages(instPkgs = current_versions) %>% data.frame(stringsAsFactors=FALSE)
      outdated_pkgs$Installed = sapply(outdated_pkgs$Package, function(x) format(packageVersion(x))) #other installed is old
      outdated_pkgs %<>% {.[package_version(.$Installed) < package_version(.$ReposVer),]}
      consider_upgrade = outdated_pkgs$Package
    }
  }

  for (p in 1:length(packages)) {
    package = packages[p]

    cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")

    will_install = !(package %in% installed) && install_packages || force_install
    #will_upgrade = upgrade && package %in% consider_upgrade

    can_load = TRUE
    if(!(package %in% installed) && !install_packages)
      fail = c(fail, package)

    if (will_install) {
      cat("\r",render(prog, p, show_progress)," installing ",package,"...",spaces, sep = "")
      stfu({install.packages(package, verbose = FALSE, quiet = TRUE)})

      stfu({can_load = require(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      if (!can_load) fail = c(fail, package)
    }

    # if (will_upgrade) #upgrade package
    # {
    #   sel = outdated_pkgs[outdated_pkgs$Package==package,]
    #   cat("\r",render(prog, p, show_progress)," upgrading ",package,"...",spaces, sep = "")
    #   stfu({detach(paste0("package:",package),unload = TRUE);
    #     update.packages(oldPkgs=package, verbose = FALSE, quiet = TRUE,ask = FALSE)})
    #
    #   current_ver = format(packageVersion(package))
    #   if (compareVersion(current_ver, sel$Installed) <= 0) {upgrade_fail=c(upgrade_fail, package)} else {upgraded=c(upgraded, package)}
    #
    #   data_acc = rbind(data_acc, data.frame(package=package, action="UPGRADE",
    #                                         result=compareVersion(current_ver, sel$ReposVer) < 0, stringsAsFactors = FALSE))
    #   added_res = TRUE
    # }

    if (can_load) {
      cat("\r",render(prog, p, show_progress)," loading ",package,"...",spaces, sep = "")
      stfu({library(package, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)})
      success = c(success,package)
    }
  }
  cat("\r",spaces,"\r")

  ## Output status ####################
  pkg_success = wrap_text_table(success, exdent)
  #pkg_upgrade = wrap_text_table(upgraded, exdent)
  #pkg_upgrade_failed = wrap_text_table(upgrade_fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_failed = wrap_text_table(fail, exdent) %>% str_replace_all("(\\w+)",.cfail(underline("\\1")))
  pkg_dupl = wrap_text_table(names(duplicates), exdent) %>% str_replace_all("(\\w+)",.cwarn(underline("\\1")))
  pkg_cons = wrap_text_table(consider_upgrade, exdent) %>% str_replace_all("(\\w+)", .chint(underline("\\1")))

  if(length(success) > 0) cat(bull$succ,SUCCESS,pkg_success,"\n",sep = "")
  #if(length(upgraded) > 0) cat(bull$succ,UPGRADED,pkg_upgrade,"\n",sep="")
  #if(length(upgrade_fail) > 0) cat(bull$fail, .cfail(UPGRADE_FAIL),pkg_upgrade_failed,"\n",sep="")
  if(length(fail) > 0) cat(bull$fail, .cfail(FAILED),pkg_failed,"\n",sep="")
  if(length(duplicates) > 0) cat(bull$warn, DUPLICATED,pkg_dupl,"\n",sep="")

  if(length(redundant) > 0) {
    txt = sapply(names(redundant), function(x) paste0(.cwarn(underline(x)), " (loaded by ", frmt(redundant[[x]]), ")"))
    spaces = paste0(rep(" ",nchar(REDUNDANT)+3),collapse = "")
    cat(bull$warn, REDUNDANT, paste0(txt,collapse = paste0("\n",spaces)),"\n", sep="")
  }
  if(length(consider_upgrade) > 0) cat(bull$hint, CONSIDER_UPGR, pkg_cons,"\n",sep="")

  end = Sys.time()
  cat(sprintf("\n%sDone. %s\n", bull$info, .cnumb(format_duration(start, end))))
  options(warn=oldw)
}

#' @param ... list of package names.
#' @param collection_name One or multiple collection names. Must be in \code{"data_import","image_import","ggplot",
#' "grid","survival","processing","shiny","development"}.
#' @export
load_package_collection = function(collection_name = names(list_package_collections()), ...) {
  .Deprecated("load_packages")
  col_names = unique(match.arg(collection_name, several.ok = TRUE))
  pkg_cols = list_package_collections()
  pkgs = sapply(col_names,function(x) pkg_cols[x]) %>% unlist %>% unique %>% sort

  load_packages(pkgs, ...)
}

#' List package collections
#' @export
#' @rdname load_package_collection
list_package_collections = function() {
  .Deprecated("load_packages")
  list(
    "data_import" = c("readxl","writexl","foreign","utils","haven","sas7bdat","Hmisc"),
    "image_import" = c("png","bmp","rtiff","rgdal"),
    "ggplot" = c("ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo","grDevices"),
    "grid" = c("gridExtra","gridGraphics","grDevices"),
    "survival" = c("rms","mice"),
    "processing" = c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice", "Hmisc","tidyr"),
    "shiny" = c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"),
    "development" = c("devtools","roxygen2","testthat","utils","rhub","cli","crayon")
  )
}

#' @export
#' @rdname load_package_collection
list_common_packages = function() {
  .Deprecated("load_packages")
  c("devtools", "utils", "readxl", "writexl", "gridExtra", "gridGraphics",
    "reshape2", "scales", "ggplot2", "stringr", "formatR", "tibble", "magrittr","dplyr","roxygen2")
}

#' @export
#' @rdname load_package_collection
load_common_packages = function(...) {
  .Deprecated("load_packages")
  load_packages(list_common_packages(), ...)
}

#' Analyze package imports
#'
#' @description Analyzes the package imports via \code{library()} and \code{load_packages()} in a list of filenames.
#' @param files A vector of filenames of R source files. Typically this is created by \code{list.files(folder, pattern="\\\.[rR]$")}
#'
#' @return a named list of results (invisibly). This list contains all import statments, a list of duplicated imports,
#' a list of redundant imports, all function calls in the files with the corresponding imports and a list of packages with the number of function calls.
#' @export
#'
#' @examples
#' \dontrun{
#' analyze_package_imports(list.files(pattern="\\.[rR]$", recursive=TRUE))
#' }
#' @importFrom utils getParseData lsf.str
#' @importFrom dplyr filter group_by summarise n arrange desc select
#' @importFrom magrittr set_colnames
#' @importFrom crayon red
analyze_package_imports = function(files) {
  if(length(files) == 0 || !(str_detect(files, "\\.[rR]$") %>% all)) {
    stop("Argument files must contain filenames of R files ending in .r or .R")
  }

  cat("Analyzing imports...                 \r")
  imports = lapply(files, function(f) {
    short_fname = str_match(f,"^.*/([^/]+)$")[-1]
    parsed = getParseData(parse(f, keep.source = TRUE), includeText = TRUE)
    inds_start = which(parsed$token=="SYMBOL_FUNCTION_CALL" & parsed$text %in% c("load_packages", "library"))

    lapply(inds_start, function(ind) {
      #start id of entire expression
      is_load_packages = parsed$text[parsed$id == parsed$parent[ind]] == "load_packages"
      expr_id = parsed$parent[parsed$id == parsed$parent[ind]]
      start_row = which(parsed$id == expr_id)
      end_row = which(parsed$parent == expr_id & parsed$text==")") #closing bracket of load_packages
      sub = parsed[start_row:end_row,]

      first_named_argument = which(sub$token=="EQ_SUB") %>% (function(x) {if(length(x)==0) nrow(sub) else x}) %>% min
      sub = sub[1:first_named_argument,]

      tokens = if(is_load_packages) {c("STR_CONST")} else c("STR_CONST","SYMBOL")

      imps = sub$text[sub$token %in% tokens] %>% str_match("^.*?([[:alpha:]][[:alnum:]\\.]*[[:alnum:]]).*?$") %>% {.[,2]} %>% as.data.frame %>% set_names("package")

      if(nrow(imps)==0) {
        return(NULL)
      }

      imps$line = paste0(short_fname,":",sub$line1[sub$token %in% tokens])
      imps$full_location = paste0(f,":",sub$line1[sub$token %in% tokens])
      imps
    }) %>% do.call(rbind.data.frame, .)
  }) %>% do.call(rbind.data.frame, .)
  imports$package %<>% as.character

  ## Duplicates
  cat("Analyzing duplicates...              \r")
  dup_names = imports$package[duplicated(imports$package)] %>% unique
  duplicates = lapply(dup_names, function(d) {
    imports[imports$package == d,]
  }) %>% set_names(dup_names)

  packages = unique(imports$package)
  ## Redundant packages
  cat("Analyzing redundant packages...      \r")
  redundant = redundant_packages(packages) #name is already loaded by value

  #analyse function names
  cat("Loading packages...                  \r")
  if(length(packages)>0) {
    for(i in 1:length(packages)) {
      pct = round((i/length(packages))*100, 0) %>% paste0(., "%")
      cat("Loading packages...  ", pct, "                \r")
      stfu(library(packages[i], character.only = TRUE))
    }
  }
  exports = lapply(packages, function(p) lsf.str(paste0("package:",p))) %>% set_names(packages)

  cat("Extracting function calls...         \r")
  function_calls = lapply(files, function(f) {
    short_fname = str_match(f,"^.*/([^/]+)$")[-1]
    parsed = getParseData(parse(f, keep.source = TRUE), includeText = TRUE) %>% filter(token %in% c("SYMBOL_FUNCTION_CALL","SPECIAL"))
    if(nrow(parsed)==0) {
      return(NULL)
    }

    if(length(packages) > 0) {
      parsed$package = sapply(parsed$text, function(fun) {sapply(packages, function(p) fun %in% exports[[p]]) %>% {packages[.]} %>% paste0(collapse = ", ")})
    } else {
      parsed$package = ""
    }
    parsed$line = paste0(short_fname, ":",parsed$line1)
    parsed$full_location = paste0(f,":",parsed$line1)
    parsed %<>% filter(package!="") %>% select(line,full_location,text,package)
  }) %>% do.call(rbind.data.frame, .)

  cat("                                     \n")
  counts = lapply(packages, function(p) sum(str_detect(function_calls$package, paste0("\\b",p,"\\b")))) %>% set_names(packages)

  functions_per_package = lapply(packages, function(p) {
    function_calls[str_detect(function_calls$package,paste0("\\b",p,"\\b")),]
  }) %>% set_names(packages)

  summary_per_package = lapply(packages, function(p) {
    group_by(functions_per_package[[p]], text) %>% summarise(n=n()) %>% set_colnames(c("function", "n")) %>% arrange(desc(n))
  }) %>% set_names(packages)

  ## Show output
  txt_import =     "Imports:    "
  txt_duplicated = "Duplicated: "
  txt_redundant =  "Redundant:  "
  txt_usage =      "Usage:      "
  exdent = nchar(txt_import) + 5
  bull = .bullets()

  if(length(packages) > 0) {
    cat(bull$succ, txt_import, wrap_text_table(paste0(packages, collapse = ", "), exdent), "\n")
  } else {
    cat(bull$fail, "No imports were found.\n")
  }
  if (length(duplicates)>0) {
    cat(bull$warn, txt_duplicated, "\n")
    for(p in names(duplicates)) {
      cat("     ",p,":\n")
      cat("       ",paste0(duplicates[[p]]$line, collapse = "\n        "), "\n")
    }
  }
  if (length(redundant)>0) {
    cat(bull$warn, txt_redundant, "\n")
    for(p in names(redundant)) {
      cat("     ",p,"is imported by:", paste0(redundant[[p]], collapse = ", "), "\n")
    }
  }

  if(length(packages)>0) {
    cat(bull$succ, txt_usage,"\n")
    for(p in names(counts)) {
      txt=paste0("     ",p,": ",counts[[p]],"\n")
      if(counts[[p]] == 0)
        txt = red(txt)
      cat(txt,sep="")
    }
  }

  output = list(imports = imports, duplicates=duplicates, redundant=redundant, function_calls=functions_per_package, function_usage=summary_per_package)
  invisible(output)
}
