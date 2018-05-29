structure(list("set_package_imports", structure(list(function(skip_prompt = FALSE) {
  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
      stop("Working directory not set to an R project folder.")

  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE) %>% unlist %>%
          str_match(., paste0("^#\'[ ]+@import(?:.*?From)? ([^ ]*).*$|",
                              "^.*(?:library|require)\\((.*?) *[,\\)].*$|",
                              "^.*?[^[:alpha:]](.*?)::[:]?.*$|",
                              "^.*use_packages\\((?:c\\()?(.*?)\\).*$")) %>% .[, -1] %>% rmNA) %>%
          unlist %>% str_split(",") %>% unlist %>% str_replace_all("[\\'[:space:]]","") %>% unique %>%
          {.[is_valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
          require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

  pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")
  RVersion = sprintf("R (>= %s)",getRversion())

  cat_rule(left = "Found packages", line = "bar4", col = "dodgerblue4",line_col = "black")
  cat("\n")
  cat_bullet(RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pack_version, col = "dodgerblue4", bullet_col = "black", bullet = "tick")

  if (skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1) {
    readLines("DESCRIPTION") %>% paste0(collapse = "\n") %>%
      str_replace("(R \\(.*?\\))",RVersion) %>%
      str_replace("(?s)(Imports: )(.*?)(\n[[:alpha:]]+:)",sprintf("\\1\n  %s\\3",paste0(pack_version,collapse=",\n  "))) %>%
      writeLines("DESCRIPTION")

    cat("\n")
    cat_rule()
    cat_bullet("DESCRIPTION successfully updated.", bullet = "tick", bullet_col="dodgerblue4", col="dodgerblue4")
  } else warning("DESCRIPTION was not adjusted.")
}, function(skip_prompt = FALSE) {
  if (!dir.exists("R/") || !file.exists("DESCRIPTION"))
      stop("Working directory not set to an R project folder.")

  depen = list.files("R/", ".*\\.[rR]$", full.names = TRUE, recursive = TRUE) %>%
          sapply(. %>% read.delim(sep = "\n", stringsAsFactors = FALSE) %>% unlist %>%
          str_match(., paste0("^#\'[ ]+@import(?:.*?From)? ([^ ]*).*$|",
                              "^.*(?:library|require)\\((.*?) *[,\\)].*$|",
                              "^.*?[^[:alpha:]](.*?)::[:]?.*$|",
                              "^.*use_packages\\((?:c\\()?(.*?)\\).*$")) %>% .[, -1] %>% rmNA) %>%
          unlist %>% str_split(",") %>% unlist %>% str_replace_all("[\\'[:space:]]","") %>% unique %>%
          {.[is_valid_pkgname(.)]} %>% sort

  depen = depen[sapply(depen, function(x) suppressWarnings(suppressPackageStartupMessages(
          require(x, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)))) %>% unname]

  pack_version = depen %>% sapply(. %>% packageVersion %>% format) %>% paste0(depen," (>= ",.,")")
  RVersion = sprintf("R (>= %s)",getRversion())

  cat_rule(left = "Found packages", line = "bar4", col = "dodgerblue4",line_col = "black")
  cat("\n")
  cat_bullet(RVersion, background_col = "dodgerblue4", bullet_col = "white", col="white", bullet = "continue")
  cat_bullet(pack_version, col = "dodgerblue4", bullet_col = "black", bullet = "tick")

  if (skip_prompt || menu(c("Yes","No"),title="\nReplace DESCRIPTION imports?") == 1) {
    readLines("DESCRIPTION") %>% paste0(collapse = "\n") %>%
      str_replace("(R \\(.*?\\))",RVersion) %>%
      str_replace("(?s)(Imports: )(.*?)(\n[[:alpha:]]+:)",sprintf("\\1\n  %s\\3",paste0(pack_version,collapse=",\n  "))) %>%
      writeLines("DESCRIPTION")

    cat("\n")
    cat_rule()
    cat_bullet("DESCRIPTION successfully updated.", bullet = "tick", bullet_col="dodgerblue4", col="dodgerblue4")
  } else warning("DESCRIPTION was not adjusted.")
}), .Names = c("package:hgutils", "")), c("package:hgutils", 
"namespace:hgutils"), c(TRUE, FALSE), c(FALSE, TRUE)), .Names = c("name", 
"objs", "where", "visible", "dups"), class = "getAnywhere")
