#' Make ggplot errorbars and mean with a single function
#'
#' Often, automatically bootstrapped errorbars in \code{ggplot2} are accompanied
#' by a dot indicating the mean of the errorbars. Generally, this is done with
#' two calls of \code{\link[ggplot2]{stat_summary}}, one for the bars and one
#' for the dot.  This function is just a shortcut that will add both.
#'
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @return A list of the \code{stat_summary} objects
#' @examples
#'df <- data.frame(
#'   y<-rnorm(100),
#'   x<-factor(c(rep("Label1",50), rep("Label2",50)))
#')
#'
#'ggplot(df, aes(x = x, y = y)) +
#'   geom_point(alpha=0.3) +
#'   geom_errorbars(color="red") +
#'   xlab("WHAAAT")
#' @seealso \code{\link{geom_dots_and_bars}}
#' @export
geom_errorbars <- function(...) {
  return(list(ggplot2::stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", ...),
              ggplot2::stat_summary(fun.y=mean, geom = "point", ...)))
}


#' Make dotplots with errorbars with a single function
#'
#' Adds a dotplot, 95\% CI errorbars, and a point representing the mean
#' to a ggplot object. Almost identical to \code{\link{errorbars}}, but
#' with the obvious additional \code{\link[ggplot2]{geom_dotplot}}. \cr
#' Right now, only uses default values for errorbars
#'
#' @param \dots Additional arguments for the \code{\link[ggplot2]{stat_summary}} pair.
#' @return A list of the \code{stat_summary} objects
#' @examples
#' df <- data.frame(
#'   y<-rnorm(100),
#'   x<-factor(c(rep("Label1",50), rep("Label2",50)))
#' )
#'
#' ggplot(df, aes(x = x, y = y)) +
#'   geom_dots_and_bars(color="red",fill="red") +
#'   xlab("WHAAAT")
#' @seealso \code{\link{errorbars}}
#' @export
geom_dots_and_bars <-function(...,
                         binaxis = "y",
                         stackdir = "center",
                         dotsize = 0.5,
                         alpha=0.2) {
  dotplot <- list(ggplot2::geom_dotplot(binaxis = binaxis,
                                        stackdir = stackdir,
                                        dotsize = dotsize,
                                        alpha=alpha,
                                        ...))
  return(append(dotplot, geom_errorbars(...)))
}
