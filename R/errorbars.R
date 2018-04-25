#' Make ggplot errorbars and mean with a single function
#'
#' Often, automatically bootstrapped errorbars in \code{ggplot2} are accompanied
#' by a dot indicating the mean of the errorbars. Generally, this is done with
#' two calls of \code{\link[ggplot2]{stat_summary}}, one for the bars and one
#' for the dot.  This function is just a shortcut that will add both.
#'
#' @param gg_obj The ggplot object (i.e., what is built up from \code{ggplot(\dots)}). If \code{errorbars()} does not immediately follow \code{ggplot(\dots)}, use \code{\%+\%} instead of \code{+} to add the intervening ggplot layers until the ggplot object is piped into the function.
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @examples
#' ggplot(aes(x = x, y = y)) %+%
#'    geom_point() %>%
#'    errorbars() +
#'    xlab("WHAAAT")
#' @seealso \code{\link{dots_and_bars}}
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


#' Make dotplots with errorbars with a single function
#'
#' Adds a dotplot, 95\% CI errorbars, and a point representing the mean
#' to a ggplot object. Almost identical to \code{\link{errorbars}}, but
#' with the obvious additional \code{\link[ggplot2]{geom_dotplot}}. \cr
#' Right now, only uses default values for errorbars
#'
#' @param gg_obj The ggplot object (i.e., what is built up from \code{ggplot(\dots)}). If \code{errorbars()} does not immediately follow \code{ggplot(\dots)}, use \code{\%+\%} instead of \code{+} to add the intervening ggplot layers until the ggplot object is piped into the function.
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @examples
#' ggplot(aes(x=x,y=y)) %+%
#'    geom_point() %>%
#'    dots_and_bars() +
#'    xlab("WHAAAT")
#' @seealso \code{\link{errorbars}}
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
    ),
    ...)
}
