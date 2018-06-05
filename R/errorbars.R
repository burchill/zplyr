#' (Deprecated) Make ggplot error bars and mean with a single function
#'
#' \strong{Deprecated! Use \code{\link{stat_errorbar}} instead!} \cr
#' Often, automatically bootstrapped error bars in \code{ggplot2} are accompanied
#' by a dot indicating the mean of the errorbars. Generally, this is done with
#' two calls of \code{\link[ggplot2]{stat_summary}}, one for the bars and one
#' for the dot.  This function is just a shortcut that will add both.
#'
#' @param gg_obj The ggplot object (i.e., what is built up from \code{ggplot(\dots)}). If \code{errorbars()} does not immediately follow \code{ggplot(\dots)}, use \code{\%+\%} instead of \code{+} to add the intervening ggplot layers until the ggplot object is piped into the function.
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @examples
#' \dontrun{
#' ggplot(df, aes(x = x, y = y)) %+%
#'    geom_point() %>%
#'    errorbars() +
#'    xlab("WHAAAT")
#' }
#' @export
errorbars <- function(gg_obj, ...) {
  .Deprecated("stat_errorbar")
  ggplot2::`%+%`(
    ggplot2::`%+%`(
      gg_obj,
      stat_summary(fun.data = mean_cl_boot, geom = "errorbar", ...)
    ),
    stat_summary(fun.y=mean,geom = "point", ...)
  )
}


#' (Deprecated) Make dotplots with error bars with a single function
#'
#' \strong{Deprecated! Use \code{\link{stat_dots_and_bar}} instead!} \cr
#' Adds a dotplot, 95\% CI error bars, and a point representing the mean
#' to a ggplot object. Almost identical to \code{\link{errorbars}}, but
#' with the obvious additional \code{\link[ggplot2]{geom_dotplot}}. \cr
#' Right now, only uses default values for error bars
#'
#' @param gg_obj The ggplot object (i.e., what is built up from \code{ggplot(\dots)}). If \code{errorbars()} does not immediately follow \code{ggplot(\dots)}, use \code{\%+\%} instead of \code{+} to add the intervening ggplot layers until the ggplot object is piped into the function.
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @param binaxis Set by default to "y"
#' @param stackdir Set by default to "center"
#' @param dotsize Set by default to 0.5
#' @param alpha Set by default to 0.2
#' @examples
#' \dontrun{ggplot(df, aes(x=x,y=y)) %+%
#'    geom_point() %>%
#'    dots_and_bars() +
#'    xlab("WHAAAT")
#' }
#' @export
dots_and_bars <-function(gg_obj,
                        binaxis = "y",
                        stackdir = "center",
                        dotsize = 0.5,
                        alpha=0.2,
                        ...) {
  .Deprecated("stat_dots_and_bar")
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
