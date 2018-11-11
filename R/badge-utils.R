#' #' Convert a badge SVG to text
#' #'
#' #' @param svg_badge_url URL of a SVG badge.
#' #' @param full_colour whether to process the badge colours.
#' #'
#' #' @return a badge in text form, optionally coloured.
#' #' @export
#' #' @importFrom XML xmlParse xmlToList
#' #' @importFrom stringr str_match_all
#' #' @importFrom utils tail
#' #' @importFrom grDevices col2rgb rgb2hsv
#' badge2text = function(svg_badge_url, full_colour=TRUE) {
#'   if(!str_detect(svg_badge_url,"https?://.*\\.svg$"))
#'     stop("Argument 'svg_badge_url' must be a valid url to an .svg file")
#'   svg = suppressWarnings(readLines(svg_badge_url))
#'   dl = svg %>% xmlParse %>% xmlToList
#'
#'   texts = str_match_all(svg,"<text.*?>(.*?)<\\/text>")[[1]][,2] %>% unique %>% paste0(" ", ., " ")
#'   badge = if(!full_colour) {
#'     paste0(texts, collapse = "|")
#'   } else {
#'     repair_hex = function(x) ifelse (str_detect(x,"^#[[:xdigit:]]{3}$"), str_replace_all(x,"([[:xdigit:]])","\\1\\1"), x)
#'     to_linear = function(X) sapply(X, function(x) ifelse(x<=0.04045, x/12.92, ((x+0.055)/(1+0.055))^2.4))
#'     left_box = function(x) make_style(col_left*0.9,bg=TRUE)(make_style(txt_col*0.97)(x))
#'     right_box = function(x, text_colour) make_style(col_right,bg=TRUE)(make_style(text_colour*0.97)(x))
#'     get_Y = function(c) to_linear(c/255) %*% c(0.2126, 0.7152, 0.0722)
#'
#'     col_left = col2rgb("#555555")
#'     col_right = str_match_all(svg,"<(?=path)[^<]*?fill=\"(#.*?)\"")[[1]][,2] %>% unique %>%
#'       tail(1) %>% repair_hex %>% col2rgb %>% multiply_by(0.9)
#'     txt_col = dl$g$.attrs['fill'] %>% repair_hex %>% col2rgb
#'
#'     Y = get_Y(col_right)
#'     s = rgb2hsv(col_right)['s',]
#'
#'     tcolour = if(s >= 0.5 && Y >= 0.3) col2rgb("#000000") else col2rgb("#ffffff")
#'     paste0(left_box(texts[1]), right_box(texts[2],tcolour), sep = "")
#'   }
#'
#'   class(badge) = "badge"
#'   attr(badge, "subject") = trimws(texts[1])
#'   attr(badge, "status") = trimws(texts[2])
#'   badge
#' }
#'
#' #' Prints coloured badges
#' #'
#' #' @inheritParams base::print
#' #' @export
#' print.badge = function(x, ...) {
#'   cat(x)
#'   invisible(x)
#' }
