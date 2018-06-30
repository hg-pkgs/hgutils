#' Standardized message colors
#' @param ... the text to be colored.
#' @return A function to color text
#' @usage NULL
#' @importFrom crayon make_style
#' @name message_colors
NULL

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cmess = function(...) {make_style("dodgerblue4")(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cmess_bg = function(...) {make_style("white")(make_style("dodgerblue4", bg = TRUE)(paste0(list(...),collapse = "")))}

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cwarn = function(...) {make_style("chocolate1")(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cwarn_bg = function(...) {make_style("chocolate1", bg = TRUE)(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For success messages.
.csucc = function(...) {make_style("green")(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For success messages.
.csucc_bg = function(...) {make_style("green", bg = TRUE)(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For fail messages.
.cfail = function(...) {make_style("red")(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For fail messages.
.cfail_bg = function(...) {make_style("white")((make_style("red", bg = TRUE)(paste0(list(...), collapse = ""))))}

#' @usage NULL
#' @describeIn message_colors For fail messages.
.cnumb = function(...) {make_style("cyan")(paste0(list(...),collapse = ""))}

#' @usage NULL
#' @describeIn message_colors For fail messages.
.cnumb_bg = function(...) {make_style("cyan", bg = TRUE)(paste0(list(...),collapse = ""))}

BULLET_LENGTH = 3

#' Bullets
#'
#' @return A list of various coloured bullets
.bullets = function() {
  list(mess=.cmess(" \u25ba "), succ=.csucc(" \u25ba "), fail=.cfail(" \u25ba "),
       warn=.cwarn(" \u25ba "), numb=.cnumb(" \u25ba "))
}
#bull = list(mess=.cmess(" \u25ba "), succ=.csucc(" \u221a "), fail=paste0(" ",.cfail(" \u078 ")," "), warn = paste0(" ",.cwarn_bg("\u203c")," "))
