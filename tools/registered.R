#' Internal function to check whether the current user has the working directory registered.
#'
#' @param verbose Whether to print the user name and registered working directory.
#' @param return_data Whether to return a boolean or a \code{\link[tibble]{tibble}} containing the found user.
#'
#' @return Either a boolean (if return_data is FALSE) or a tibble containing fields 'desc' (computer description),
#' 'usr' (computer user name) and 'location' (working directory base).
# .is_registered = function(verbose = FALSE, return_data = TRUE) {
#   current_usr = Sys.info()["user"]
#   results = working_dirs[working_dirs$usr == current_usr, ]
#   registered = nrow(results) == 1
#
#   if (verbose) {
#       if (!registered) print(paste0("User '", current_usr, "' is not registered."), quote = F) else
#           print(paste0("User '", current_usr, "' is registered. Default working directory is '", results$location[1], "'"), quote = F)
#   }
#
#   if (return_data)
#       return(results) else return(registered)
# }

#' Checks whether the current user has the working directory registered.
#'
#' @param verbose Whether to print the user name and registered working directory.
#'
#' @return Boolean indicating whether the user is registered or not.
#'
#' @examples is_registered()
#' @export
#' @family initialization functions
# is_registered = function(verbose = FALSE) {
#   .is_registered(verbose = verbose, return_data = FALSE)
# }
