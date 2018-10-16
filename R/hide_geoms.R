#' Hide geoms on plot while keeping legends and scaling intact
#'
#' Hides all the geoms on the graph while keeping everything else (e.g.,
#' the scaling, the legends, the titles, etc.) the same. \cr
#' Useful for when, in a talk, you want to explain to the audience what
#' they are about to see, while acquainting them with the legends, etc.
#'
#' Currently, this just goes through each layer of data and sets the
#' \code{size} and \code{alpha} to 0. This hasn't been tested at edge cases.
#'
#' @param gg_obj The \code{ggplot} object whose data you want to hide.
#' @return a \code{\link[gtable]{gtable}} object (which you can plot)
#' @import ggplot2
#' @examples
#'
#' a <- dplyr::tibble(
#'   alp=runif(120,0,3),
#'   bet=alp*2+1,
#'   gam=rbinom(120,1,0.5))
#' b <- dplyr::tibble(
#'   alp=runif(120,3,10),
#'   bet=-alp*2+10,
#'   gam=rbinom(120,1,0.5))
#' g <- a %>%
#'   ggplot2::ggplot(aes(x=alp,y=bet, color=as.factor(gam))) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_vline(xintercept = 5) +
#'   ggplot2::geom_line(data=b)
#' plot(g)
#' plot(hide_geoms(g))
#'
#' @export
hide_geoms <- function(gg_obj) {
  plot_data <- ggplot2::ggplot_build(gg_obj)
  data_list <- plot_data$data %>%
    purrr::map(~dplyr::mutate(., size=0, alpha=0))
  plot_data$data <- data_list
  return(ggplot2::ggplot_gtable(plot_data))
}

#' Get ggplot default colors
#'
#' Taken from John Colby's answer here: https://stackoverflow.com/questions/8197559/emulate-ggplot2-default-color-palette, this function lets you get the default palette from ggplot2.
#' @param n the number of different colors in the palette
#' @export
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
