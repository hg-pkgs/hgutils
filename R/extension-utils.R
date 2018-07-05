#' Validate package and function names
#' @description Naming rule obtained from \emph{'Writing R Extensions'} manual.
#' The corresponding regular expression used for verifying the package name is \code{"[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"}.
#' For function names this is \code{"((?:[[:alpha:]]|\\.(?![0-9]))[[:alnum:]_\\.]*)"}
#' @param pkg string vector containing package names. Can be a vector of strings with size of at least 1.
#' @param func string vector containing function names. Can be a vector of strings with size of at least 1.
#'
#' @return A named logical indicating whether the package name is valid.
#' @export
#' @references \code{\link[base]{make.names}}, \href{https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#The-DESCRIPTION-file}{'Writing R Extensions'} manual.
#' @examples
#' valid_pkgname("hgutils") # valid
#' valid_pkgname("ggplot2") # valid
#' valid_pkgname("pkg2.-1") # invalid
#'
#' valid_funcname(".hgutils") # valid
#' valid_funcname("ggplot2") # valid
#' valid_funcname(".2pkg") # invalid
#' @importFrom stringr str_detect
#' @importFrom magrittr %>% set_names
#' @family developer functions
valid_pkgname = function(pkg) {
  regex = "^[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]$"

  if (!is.character(pkg))
    stop(sprintf("Argument 'pkg' must be of class 'character', but is %s.", frmt(pkg, TRUE)))

  str_detect(pkg,regex) %>% set_names(pkg)
}

#' @export
#' @rdname valid_pkgname
valid_funcname = function(func) {
  regex = "^((?:[[:alpha:]]|\\.(?![0-9]))[[:alnum:]_\\.]*)$"

  if (!is.character(func))
    stop(sprintf("Argument 'func' must be of class 'character', but is %s.", frmt(func, TRUE)))

  str_detect(func, regex) %>% set_names(func)
}

#' Update default function settings
#' @description Uses ellipsis parameter to update a list of default settings.
#'
#' @param default named list of default values for settings.
#' @param ... optional settings values to override the default settings.
#'
#' @return The updated list of settings with updated values.
#' @export
#' @family developer functions
#' @examples
#' foo = function(...) {
#'   default = list(a=1)
#'   settings = update_settings(default, ...)
#' }
#'
#' \dontrun{foo(a=2, b=3)}
update_settings = function(default, ...) {
  supplied = list(...)
  supplied = supplied[names(supplied)!=""]
  match = intersect(names(default), names(supplied))
  additional = setdiff(names(supplied), names(default))
  if (length(additional) > 0)
    warning(sprintf("Ignoring unspecified parameters: %s. Must be one of %s.", frmt(additional), frmt(names(default))))

  default[names(supplied)] = supplied
  default
}

#' Retrieve generic function implementations
#' @description Obtains a list of classes for which the supplied generic function has an implementation.
#'
#' @param generic name of the generic function.
#' @param remove_default whether to keep the default generic implementation in the result.
#'
#' @return A vector with class names for which argument '\code{generic}' has an implementation.
#' @export
#'
#' @examples
#' #get a list of classes which have an implementation for graphics::plot
#' impls = generic_implementations('plot')
#'
#' @note Removes the default generic implementation
#' @importFrom magrittr %>%
#' @importFrom stringr str_match
#' @importFrom utils methods
#' @family developer functions
generic_implementations = function(generic, remove_default = TRUE) {
  stopifnot(length(generic) == 1)
  impls = methods(generic)
  if (!is.character(generic) || length(impls)==0)
    stop(sprintf("Argument 'generic' is not a valid generic function."))

  impls %>% sapply(. %>% {str_match(., "^.*\\.(.*)$")[, 2]}) %>% unname %>% rm_na %>%
    if(remove_default) .[. != "default"] else .
}

#' Set imports for \emph{DESCRIPTION} file
#' @description Update the \emph{DESCRIPTION} file with all imported packages stated in the source code.
#'
#' @param skip_prompt whether to skip the confirmation prompt to change the \emph{DESCRIPTION} file. Defaults to \code{FALSE}.
#' @param update whether the \emph{DESCRIPTION} file should be updated. Defaults to \code{TRUE}.
#' @param use_version_numbers whether package version numbers should be included in the \emph{DESCRIPTION} file. Defaults to \code{TRUE}.
#' @param rversion version of R to be used in the \emph{DESCRIPTION} file.
#' Can be \code{DEPENDENCIES_VERSION} for the latest version in the package dependencies,
#' \code{LATEST_VERSION} for the current R version or any valid version number.
#'
#' @return Invisibly returns a list with the current R version,
#' the R version obtained from dependencies and packages names (including version numbers).
#' @export
#'
#' @examples \dontrun{crossref_description(skip_prompt=TRUE)}
#' @importFrom magrittr %>%
#' @importFrom stringr str_match str_replace str_replace_all str_split
#' @importFrom utils packageVersion packageDescription read.delim
#' @importFrom crayon blue green red underline
#'
#' @family developer functions
#' @seealso \code{\link[base]{numeric_version}}
crossref_description = function(skip_prompt=FALSE, update=TRUE, use_version_numbers=TRUE, rversion = "DEPENDENCIES_VERSION") {
  start = Sys.time()
  bull = .bullets()
  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
    stop("Working directory not set to an R project folder.")

  rversion_const = c("DEPENDENCIES_VERSION","LATEST_VERSION")
  if (!rversion %in% rversion_const && !str_detect(rversion,"[[:digit:]]+([\\.-][[:digit:]]+)+"))
    stop(sprintf("Argument 'rversion' must be either a valid version number or one of %s.",frmt(rversion_const)))

  cat(.get_title_bar("Updating DESCRIPTION"),"\n")

  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n")
  package_name = desc %>% {str_match(., "Package:[ ]*(.*?)\n(?:.*\n)+Version:[ ]*(.*?)\n")[-1]} %>% paste(collapse = " ")
  existing_imports = str_match(desc,"Imports:((?:.*\n)+?).*?:")[,2] %>%
    str_replace_all("[ \n]|(?:\\(.*?\\))","") %>% {strsplit(.,",")[[1]]}

  pkg_name_regex = "[[:alpha:]][[:alnum:]\\.]*[[:alnum:]]"
  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE, quote="") %>% unlist %>%
                 str_match(., paste0("#'[ ]*@import[ ]+([[:alnum:] \\.]*)$|",
                           paste0("#\'[ ]*@importFrom[ ]+(",pkg_name_regex,")|"))) %>%
                 #paste0("^[^#]*?(?:library|require)\\((",pkg_name_regex,")[ ]*[,\\)]|"),
                 #paste0("[^#]*?\\((",pkg_name_regex,")::[:]?[^(:)]+|"),
                 #"^[^#]*?load_packages\\((?:c\\()?([[:alnum:] ,\"\\'\\.]*?)\\).*")
                   .[, -1] %>% rm_na) %>%
          unlist %>% str_split("[ ,]") %>% unlist %>% str_replace_all("[\\'\"]","") %>%
          unique %>% {.[valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) stfu(require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE))) %>% unname]

  pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")
  pkgs = if(use_version_numbers) pack_version else depen

  #-- Determine R versions -------------------------------
  current_r = format(getRversion())
  dep_rvers = sapply(depen, function(x) packageDescription(x)$Depends)
  dependencies_r = do.call(rbind, dep_rvers) %>% unlist %>%
                   unname %>% {str_match(.,"R \\(>= (.*?)\\)")[,-1]} %>% rm_na %>% numeric_version %>% max %>% format
  if(length(dependencies_r)==0) dependencies_r = current_r

  rversion = ifelse(identical(rversion, "LATEST_VERSION"), current_r, dependencies_r)
  RVersion = sprintf("R (>= %s)",rversion)


  IMPORT = "Imported packages: "
  RVER =   "R version:         "
  exdent = nchar(IMPORT)+3

  tab = wrap_text_table(pack_version, exdent=exdent, min_size = 1)
  pkgs_str = tab %>%
             str_replace_all("([[:alpha:]][[:alnum:]\\.]*[[:alnum:]])", underline("\\1")) %>%
             str_replace_all("_"," ") %>% str_replace_all(",","")
  cat(bull$numb,RVER,.cnumb(RVersion),"\n",bull$mess,IMPORT,pkgs_str,"\n",sep = "")

  ## Replace DESCRIPTION file #####################
  desc %>% str_replace("R \\(.*?\\)",RVersion) %>%
           str_replace("(Imports:)(?:(?:.*\n)+?)(.*?:)",sprintf("\\1\n  %s\n\\2",paste0(pkgs,collapse=",\n  "))) %>%
           writeLines("DESCRIPTION")
  cat("\n",bull$succ,"DESCRIPTION successfully updated.\n",sep = "")

  depen = stfu({Filter(function(x) !require(x, character.only = TRUE), depen)}) #check if needs installation
  end = Sys.time()
  if (length(depen) > 0) {cat("\n\n"); load_packages(depen)} else cat(bull$mess,"Done. ",.cnumb(format_duration(start,end)),"\n",sep="")

  invisible(list(current_r_version=current_r, dependencies_r_version=dependencies_r,
            packages=depen, packages_version=pack_version))
}

#' Find duplicated packages names
#'
#' @param pkgs A list of packages names
#'
#' @return A named list of duplicated names and number of occurrences
#' @importFrom magrittr %>% set_names
.pkg_duplicated = function(pkgs)
{
  unique(pkgs[duplicated(pkgs)]) %>% set_names(., .) %>% lapply(. %>% {sum(pkgs==.)})
}

#' Find redundant packages
#'
#' @param packages list of package names.
#'
#' @return A named list of packages names, where each value is a vector of packages already loading the corresponding package.
#' @details Certain packages have a direct dependency on other packages. In that case it is unnecessary to attach the latter packages.
#' This function finds those packages and returns them in a named list. For each named item, the name is imported by the value in the list.
#' @export
#'
#' @examples
#' \dontrun{
#' #grid does not have be loaded since gridGraphics already does so.
#' redundant_packages("gridGraphics","grid")
#' }
#' @importFrom magrittr %>% set_names
#' @importFrom stringr str_detect
redundant_packages = function(packages){
  packages = unique(packages)
  redundant = packages %>% set_names(., .) %>%
              lapply(. %>% {
                sapply(packages, function(other) {
                  desc = packageDescription(other)
                  if("Depends" %in% names(desc) && str_detect(desc$Depends, .)) other else NULL
                  }) %>% unlist %>% unique
              })
  redundant[!sapply(redundant, is.null)]
}
