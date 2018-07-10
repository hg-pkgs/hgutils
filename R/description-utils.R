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
