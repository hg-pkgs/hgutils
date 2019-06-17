#' Extracts the matches from stringr::str_match[_all]
#'
#' @param result The results from stringr::str_match[_all]
#'
#' @return a list of matches
.regexl = function(result) {
  result %>% sapply(. %>% {
    .[, -1]
  }) %>% unlist %>% unname
}

#' Get the package names from file content
#'
#' @param content The content of an .R file
#'
#' @return a list of package names which were loaded in the content
#' @importFrom stringr str_replace_all str_match_all str_split
.extract_packages = function(content) {
  HGUTILS_LOAD = "load_packages\\((?:c\\()?(.*?)\\){1,2}"  #(?: create group without matching)
  DEFAULT_LOAD = "library\\((.*?)\\)"
  #INST = "install.packages\\((?:c\\()?(.*?)\\){1,2}"
  REDUN = "[\"[:cntrl:][:space:]]*"

  c(HGUTILS_LOAD, DEFAULT_LOAD) %>% sapply(. %>% str_match_all(content, .) %>% .regexl %>% paste(collapse = ",") %>%
                                             str_replace_all(REDUN, "")) %>% paste(collapse = ",") %>% {
    str_split(., ",")[[1]]
  } %>% str_match_all("^[\\'\\\"]?([a-zA-Z][a-zA-Z0-9\\.]*[a-zA-Z0-9])[\\'\\\"]?$") %>% .regexl  #match on proper package names, possibly with apostr.

}

#' Extract package names from R files in folder
#'
#' @param folder The base folder name, containing the .R files
#' @param recursive Should the listing recurse into directories?
#'
#' @return A named list. For each .R file containing package loading, a list of package names
get_packages = function(folder, recursive=TRUE) {
  files = list.files(folder, pattern = "^.*\\.[rR]$", recursive = recursive, full.names = TRUE)
  pkgs = sapply(files, function(f) {
    content = readLines(f, warn = F) %>% paste0(collapse = "")
    .extract_packages(content)
  })
  pkgs[sapply(pkgs, length) != 0]
}

# folder="../../SOURCE/survival-SOURCE/"
# redudant_project_packages = function(folder) {
#   loaded_packages = get_packages(folder)
#
#   for(p in unique(unlist(loaded_packages))) {
#     if
#   }
# }
