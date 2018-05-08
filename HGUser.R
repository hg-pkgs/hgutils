# print.HGUser = function (x, ...) print(paste0('Hello ',x$user_name)) A = list(user_name = 'a',wd_locations = list('b')) class(A) =
# 'HGUser'
library(HGUtils)
type_sum.HGUser <- function(x) {
    "HGUser"
}

createUser = function() {
    user = list(user_info = Sys.info(), proj = tibble(project_name = character(0), working_directory = character(0), packages = list()))
    class(user) = c("HGUser", class(user))
    
    invisible(user)
}

identical.HGUser = function(user_a, user_b) identical(user_a$user_info, user_b$user_info)

.getUser = function(db) {
    user = db %>% filter(., .$user %>% sapply(. %>% {
        identical(.$user_info, Sys.info())
    }))
    invisible(user[[1]][[1]])
}

addProject = function(user, project_name, working_directory = NA, packages = list(NULL)) {
    # project_name='p' working_directory=NA packages=list(NULL)
    if (!is.na(working_directory) && !dir.exists(working_directory)) 
        stop(paste0("The project directory '", working_directory, "' does not exist."))
    
    if (is.na(working_directory)) {
        working_directory = getwd()
        warning(paste0("Working directory not specified. Setting it to current working directory: '", working_directory, "'"))
    }
    
    user$proj %<>% add_row(project_name = project_name, working_directory = working_directory, packages = list(packages))
    install_load_packages(packages, load = F)
    if (!is.null(unlist(packages))) 
        print(paste0("Added project '", project_name, "'. All required packages have been installed."), quote = F)
    invisible(user)
}

addPackages = function(user, project, packages) {
    user$proj
    install_load_packages(packages, load = F)
}

user = createUser() %>% addProject(project_name = "plot_calibration", working_directory = "S:/Dropbox/Dropbox/MetaSurv analysis/", packages = c("ggplot2", 
    "reshape2", "dplyr", "ggthemes", "RColorBrewer", "gtable", "grid", "gridExtra", "scales", "magrittr"))
user2 = user
user2$user_info["user"] = "HOI"
db = tibble(user = character(0))
db %<>% add_row(user = list(user))
db %<>% add_row(user = list(user2))

# find user


info = Sys.info()

user
identical(user, userb)
