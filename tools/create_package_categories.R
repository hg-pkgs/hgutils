library(hgutils)
use_common_packages()
pkg_genre = data.frame(name="Data import",shorthand="data_import", packages=I(list(c("readxl","writexl","foreign","utils"))), stringsAsFactors = FALSE) %>%
rbind(list(name="Image import",shorthand="data_import", packages=I(list(c("png","bmp","rtiff","rgdal"))))) %>%
rbind(list("GGplot graphics","ggplot", I(list(c("ggplot2","ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo"))))) %>%
rbind(list("Grid graphics","grid", I(list(c("grid","gridExtra","gridGraphics"))))) %>%
rbind(list("Survival","survival", I(list(c("survival","Hmisc","rms","mice"))))) %>%
rbind(list("Data processing","processing",I(list(c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice"))))) %>%
rbind(list("Shiny","shiny",I(list(c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"))))) %>%
rbind(list("Package development","development",I(list(c("devtools","roxygen2","testthat","utils")))))
as_tibble

devtools::use_data(pkg_genre, overwrite = TRUE)
