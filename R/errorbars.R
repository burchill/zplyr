#' Make errorbars with one function
#'
#' Pipe the ggplot object into this. If not immediately after
#' \code{ggplot(\dots)}, use \code{\%+\%} instead of \code{+} to add layers until
#' the ggplot object is piped into the function.
#'
#' @param gg_obj the ggplot object
#' @param \dots values for the stat_summary
#' @examples
#' ggplot(aes(x = x, y = y)) %+%
#'    geom_point() %>%
#'    errorbars() +
#'    xlab("WHAAAT")
#' @export
errorbars <- function(gg_obj, ...) {
  ggplot2::`%+%`(
    ggplot2::`%+%`(
      gg_obj,
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", ...)
    ),
    stat_summary(fun.y=mean,geom = "point", ...)
  )
}


#' Make dotplots with errorbars with one function
#'
#' Pipe the ggplot object into this, if not immediately, uses %+%
#' Right now, only uses default values for errorbars
#'
#' @param the ggplot object
#' @param \dots values for the stat_summary
#' @examples
#' ggplot(aes(x=x,y=y)) %+%
#'    geom_point() %>%
#'    dots_and_bars() +
#'    xlab("WHAAAT")
#' @export
dots_and_bars <-function(gg_obj,
                        binaxis = "y",
                        stackdir = "center",
                        dotsize = 0.5,
                        alpha=0.2,
                        ...) {
  errorbars(
    ggplot2::`%+%`(
      gg_obj,
      geom_dotplot(binaxis = binaxis,
                   stackdir = stackdir,
                   dotsize = dotsize,
                   alpha=alpha,
                   ...)
    )
  )
}
