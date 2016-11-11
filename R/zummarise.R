#'
#' Summarises and then ungroups df
#'
#' Because I thought this was how it always worked
#'
#' @param df The data frame
#' @return whatever
#' @export zummarise
zummarise <- function(.data,...) {
  dplyr::ungroup(
    dplyr::summarise_(.data, .dots = lazyeval::lazy_dots(...))
  )
}




