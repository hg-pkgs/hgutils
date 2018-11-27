rm(list = ls())
library(dplyr)
library(stringr)
library(tibble)
setwd("C://Users/hgvandenboorn/Dropbox/SOURCE/source_webinterface/source/")

ILP = "install_load_packages\\((?:c\\()?(.*?)\\){1,2}"  #(?: create group without matching)
LIBS = "library\\((.*?)\\)"
INST = "install.packages\\((?:c\\()?(.*?)\\){1,2}"
REDUN = "[\"[:cntrl:][:space:]]*"

# gets a list from the results of the stringr::str_match[_all] function
regexl = function(res) {
    res %>% sapply(. %>% {
        .[, -1]
    }) %>% unlist %>% unname
}

# Get the package names from file content
.extract_packages = function(text) {
    c(ILP, LIBS, INST) %>% sapply(. %>% str_match_all(text, .) %>% regexl %>% paste(collapse = ",") %>% str_replace_all(REDUN, "")) %>% paste(collapse = ",") %>%
        {
            str_split(., ",")[[1]]
        } %>% str_match_all("^[\\'\\\"]?([a-zA-Z][a-zA-Z0-9\\.]*[a-zA-Z0-9])[\\'\\\"]?$") %>% regexl  #match on proper package names, possibly with apostr.

}

# get the package names mentioned in a list of files
get_packages = function(files) {
    all_pkgs = c()
    for (f in files) {
        content = readLines(f, warn = F) %>% paste0(collapse = "")
        all_pkgs = .extract_packages(content) %>% c(all_pkgs, .)
    }
    all_pkgs %>% unique %>% sort
}

files = list.files(".", pattern = "^.*\\.[rR]$", recursive = TRUE, full.names = TRUE)
pkgs = get_packages(files)
# installed = installed.packages()[,1] to_remove = setdiff(installed,pkgs) remove.packages(to_remove)
library(hgutils)
load_packages(pkgs)
