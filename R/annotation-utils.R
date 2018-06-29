#' Standardized message colors
#'
#' @return A function to color text
#' @usage NULL
#' @importFrom crayon make_style
#' @name message_colors
NULL

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cmess = function() {make_style("dodgerblue4")}

#' @usage NULL
#' @describeIn message_colors For warning messages.
.cwarn = function() {make_style("chocolate1")}

#' @usage NULL
#' @describeIn message_colors For success messages.
.csucc = function() {make_style("green")}

#' @usage NULL
#' @describeIn message_colors For fail messages.
.cfail = function() {make_style("red")}

BULLET_LENGTH = 3
bull = list(mess=" \u25ba ",succ=" \u221a ",fail=" \u078 ",warn=" \u203c ")
