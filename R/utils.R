#' @import dplyr
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @export
`%notin%` <- function(x, table) {
  !(match(x, table, nomatch = 0L) > 0L)
}
