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
#' @examples
#'
#' a <- tibble(
#'   alp=runif(120,0,3),
#'   bet=alp*2+1,
#'   gam=rbinom(120,1,0.5))
#' b <- tibble(
#'   alp=runif(120,3,10),
#'   bet=-alp*2+10,
#'   gam=rbinom(120,1,0.5))
#' g <- a %>%
#'   ggplot2::ggplot(aes(x=alp,y=bet, color=as.factor(gam))) +
#'   ggplot2::geom_point() +
#'   ggplot2::geom_vline(xintercept = 5) +
#'   ggplot2::geom_line(data=b)
#' plot(g)
#' plot(hide_data(g))
#'
#'
#' @export
hide_geoms <- function(gg_obj) {
  plot_data <- ggplot2::ggplot_build(gg_obj)
  data_list = plot_data$data %>%
    purrr::map(. %>% dplyr::mutate(size=0,alpha=0))
  plot_data$data <- data_list
  return(ggplot2::ggplot_gtable(plot_data))
}
