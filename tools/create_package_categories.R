library(hgutils)
use_common_packages()
pkg_genre = data.frame(name="data_import", packages=I(list(c("readxl","writexl","foreign","utils"))), stringsAsFactors = FALSE) %>%
rbind(list("image_import", I(list(c("png","bmp","rtiff","rgdal"))))) %>%
rbind(list("ggplot", I(list(c("ggplot2","ggthemes","ggmap","colorspace","reshape2","RColorBrewer","Cairo"))))) %>%
rbind(list("grid", I(list(c("grid","gridExtra","gridGraphics"))))) %>%
rbind(list("survival", I(list(c("survival","Hmisc","rms","mice"))))) %>%
rbind(list("processing",I(list(c("magrittr","dplyr","stringr","lubridate","tibble","utils","mice"))))) %>%
rbind(list("shiny",I(list(c("shiny","shinydashboard","shinyBS","shinyjs","plotly","shinycssloaders","shinyalert","shinythemes"))))) %>%
rbind(list("development",I(list(c("devtools","roxygen2","testthat","utils","rhub"))))) %>%
as_tibble

devtools::use_data(pkg_genre, overwrite = TRUE)
