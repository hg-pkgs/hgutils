#' Standardized message colors
#' @param ... the text to be colored.
#' @return A function to color text
#' @usage NULL
#' @importFrom crayon make_style
#' @keywords internal
#' @name message_colors
NULL

#' @rdname message_colors
#' @keywords internal
.cinfo = function(...) {make_style("dodgerblue4")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.cinfo_bg = function(...) {make_style("white")(make_style("dodgerblue4", bg = TRUE)(paste0(list(...),collapse = "")))}
#' @keywords internal
#' @rdname message_colors
.cwarn = function(...) {make_style("chocolate1")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.cwarn_bg = function(...) {make_style("chocolate1", bg = TRUE)(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.chint = function(...) {make_style("deeppink")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.chint_bg = function(...) {make_style("deeppink", bg = TRUE)(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.csucc = function(...) {make_style("green")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.csucc_bg = function(...) {make_style("green", bg = TRUE)(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.cfail = function(...) {make_style("red")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.cfail_bg = function(...) {make_style("white")((make_style("red", bg = TRUE)(paste0(list(...), collapse = ""))))}
#' @keywords internal
#' @rdname message_colors
.cnumb = function(...) {make_style("cyan")(paste0(list(...),collapse = ""))}
#' @keywords internal
#' @rdname message_colors
.cnumb_bg = function(...) {make_style("cyan", bg = TRUE)(paste0(list(...),collapse = ""))}

#' Display colored messages
#'
#' @param header message title.
#' @param message the message.
#' @param pre optional string to be displayed before the message bullet (such as \code{'\n'}).
#' @keywords internal
#' @return A colored message.
#' @name messages
NULL


#' @rdname messages
#' @keywords internal
#' @export
mess_info = function(header, message, pre=""){cat(pre, .cinfo(.bullets()$info),header,message,sep = "")}
#' @keywords internal
#' @rdname messages
#' @export
mess_succ = function(header, message, pre=""){cat(pre, .csucc(.bullets()$succ),header,message,sep = "")}
#' @keywords internal
#' @rdname messages
#' @export
mess_fail = function(header, message, pre=""){cat(pre, .cfail(.bullets()$fail),.cfail(header), .cfail(message),sep = "")}
#' @keywords internal
#' @rdname messages
#' @export
mess_warn = function(header, message, pre=""){cat(pre, .cwarn(.bullets()$warn),.cwarn(header), .cwarn(message),sep = "")}
#' @keywords internal
#' @rdname messages
#' @export
mess_numb = function(header, message, pre=""){cat(pre, .cnumb(.bullets()$numb),header, .cnumb(message),sep = "")}
#' @keywords internal
#' @rdname messages
#' @export
mess_hint = function(message, header="", pre=""){cat(pre, .chint(.bullets()$hint),.chint(header), .chint(message),sep = "")}

#' Bullets
#' @keywords internal
#' @return A list of various colored bullets
.bullets = function() {
  list(info=.cinfo(" \u25ba "), succ=.csucc(" \u25ba "), fail=.cfail(" \u25ba "),
       warn=.cwarn(" \u25ba "), numb=.cnumb(" \u25ba "), hint=.chint(" \u25ba "))
}
