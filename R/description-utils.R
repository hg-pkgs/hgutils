#' Description functions
#'
#' @description Read, write and update the DESCRIPTION file. \code{read.description} reads the
#' DESCRIPTION file in the current project directory and returns a named list.
#' \code{write.description} writes the named list back to disk, overwriting the
#' current DESCRIPTION file. Finally, \code{update_description} combines both functions
#' by reading the DESCRIPTION file, updating or creating a field and writing the result
#' back to disk.
#'
#' @param description the DESCRIPTION file.
#' @param fieldname the name of the field.
#' @param value the new value.
#' @param after if the field name is new, the name of the field after which the element is placed.
#'
#' @examples \dontrun{
#' description = read.description()
#' write.description(read.description())
#'
#' #update date in description file
#' update_description("Date", format(Sys.Date(), "%Y%-%m-%d"))
#' }
#'
#' @details The 'Depends', 'Imports' and 'Suggests' fields are sorted before writing the DESCRIPTION file.
#' @importFrom crayon bold
#' @importFrom stringr str_split str_replace_all str_trim
#' @importFrom magrittr extract2
#' @name description-functions
NULL

#' @rdname description-functions
#' @export
read.description = function() {
  if(!file.exists("DESCRIPTION")){
    mess_hint(paste0("Try creating a DESCRIPTION file with with ",bold("usethis::use_description()\n")))
    stop("the DESCRIPTION file does not exists.")
  }
  desc = readLines("DESCRIPTION") %>% paste0(collapse = "\n") %>% str_split("\n(?=.*?:)") %>% .[[1]] %>%
    sapply(. %>% str_split(":", 2), USE.NAMES = FALSE) %>% do.call(rbind, .)
  desc = desc[,2] %>% set_names(desc[,1]) %>% as.list
  desc = lapply(desc, function(x) sapply(str_split(str_trim(x), "\n")[[1]], str_trim, USE.NAMES = FALSE))
  class(desc) = "description"
  desc
}

#' @rdname description-functions
#' @export
write.description = function(description) {
  if(!"description" %in% class(description)) {
    mess_hint(paste0("The argument 'description' must be created with ",bold("hgutils::read.description()\n")))
    stop("Argument 'description' must be of class 'description'")
  }

  sort_values = function(desc, name) {
    for (nm in name) {
      if (nm %in% names(desc)) {
        desc[[nm]] %<>% str_replace_all(",","") %>% sort %>%
          paste0(collapse = ",\n") %>% str_split("\n") %>% extract2(1)
      }
    }
    desc
  }
  description %<>% sort_values(c("Depends", "Imports", "Suggests"))

  sapply(names(description), function(x) {
    if(length(description[[x]])==1) {paste0(x,": ",description[[x]])} else {paste0(x,":",paste0("\n    ",description[[x]],collapse = ""))}
  }) %>% writeLines("DESCRIPTION")
}

#' @rdname description-functions
#' @export
update_description = function(fieldname, value, after = NULL) {
  desc=read.description()
  cname = class(desc)
  if (fieldname %in% names(desc)) {
    desc[fieldname] = value
  } else {
    if(!is.null(after) && !after %in% names(desc))
      stop(sprintf("Field '%s' does not exist.",after))
    ind = ifelse(is.null(after), length(desc), which(names(desc) == after))
    desc = c(desc[1:ind],list(value) %>% set_names(fieldname),if(ind<length(desc)){desc[(ind+1):length(desc)]})
    class(desc) = cname
  }
  write.description(desc)
}

#' Add badges to the README file for use on Github
#'
#' @param github_pkg The Github repository
#' @param states Current software cycle state
#' @param readme_file The filename of the readme file
#' @param show_repo_status Whether to show the repository status as a badge
#' @param show_cran_version Whether to show the CRAN version as a badge
#' @param show_package_version Whether to show the package version as a badge
#' @param show_min_r Whether to show the minimal R version as a badge
#' @param show_last_update Whether to show the last update date as a badge
#' @param show_travis Whether to show the Travis test results as a badge (see \url{http://travis-ci.org})
#' @param show_code_coverage Whether to show the code coverage as a badge (see \url{http://codecov.io})
#'
#' @export
#'
#' @importFrom stringr str_detect str_extract str_match
#' @examples \dontrun{
#' add_badges("hvdboorn/hgutils")
#' }
add_badges = function(github_pkg, states=c("active", "abandoned", "concept", "inactive", "moved", "suspended", "unsupported", "wip"),
                      readme_file = "README.md",
                       show_repo_status=TRUE, show_cran_version=TRUE, show_package_version=TRUE,
                       show_min_r=TRUE, show_last_update=TRUE,
                       show_travis=TRUE, show_code_coverage=TRUE) {
  if(!str_detect(github_pkg, "^[^/]+/[^/]+$")) {
    stop("Argument github_pkg must be a string of the following form: 'username/package'")
  }
  desc = read.description()
  states = match.arg(states)

  is_dev = str_extract(desc$Version,"(?<=[\\.-])\\d+$") %>% as.numeric %>% {. >= 9000}
  rvers  = str_match(desc$Depends, "R[ ]+\\(>=[ ]+(.*)\\)")[-1]
  status = states[1]
  dformat = "%Y%-%m-%d"

  version = ifelse(show_package_version, paste0("[![Package version](https://img.shields.io/badge/GitHub-",desc$Version,"-orange.svg)](www.github.com/",github_pkg,")"),"")
  min_r = ifelse(show_min_r, paste0("[![minimal R version](https://img.shields.io/badge/R-v",rvers,"+-blue.svg)](https://cran.r-project.org/)"), "")
  last_update = ifelse(show_last_update, paste0("[![last_update](https://img.shields.io/badge/last%20update-",
                                                desc$Date %>% str_replace_all("-","--"),"-blue.svg)](www.github.com/",github_pkg,")"), "")

  travis = ifelse(show_travis, paste0("[![Travis](https://travis-ci.org/",github_pkg,".svg)](https://travis-ci.org/",github_pkg,")"), "")
  repo_status = ifelse(show_repo_status, paste0("[![Project Status](http://www.repostatus.org/badges/latest/",status,".svg)](http://www.repostatus.org/#",status,")"), "")
  codecov = ifelse(show_code_coverage, paste0("[![Codecov](https://img.shields.io/codecov/c/github/",github_pkg,".svg)](https://codecov.io/gh/",github_pkg,")"), "")
  cran = ifelse(show_cran_version, paste0("[![CRAN](http://www.r-pkg.org/badges/version/",desc$Package,")](https://cran.r-project.org/package=",desc$Package,")"), "")

  badges = paste0(paste0(c(repo_status, cran, version, min_r, last_update),collapse="\n"),"  \n",
                  paste0(c(travis, codecov),collapse="\n"),"\n---")

  readme = paste0(readLines(readme_file),collapse = "\n")
  if(!(str_detect(readme,"<!-- START_BADGES -->") && str_detect(readme,"<!-- END_BADGES -->"))) {
    readme = paste0("<!-- START_BADGES --><!-- END_BADGES -->\n",readme)
  }
  pieces=str_split(readme,"(?s)(?<=<!-- START_BADGES -->).*(?=<!-- END_BADGES -->)")[[1]]
  new_readme = paste0(pieces[1],"\n",badges,"\n",pieces[2]) %>% str_split("\n") %>% .[[1]]
  writeLines(new_readme, readme_file)
}

