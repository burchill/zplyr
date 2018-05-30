#' Make ggplot error bars and mean with a single function
#'
#' Often, automatically bootstrapped error bars in \code{ggplot2} are accompanied
#' by a dot indicating the mean of the errorbars. Generally, this is done with
#' two calls of \code{\link[ggplot2]{stat_summary}}, one for the bars and one
#' for the dot.  This function is just a shortcut that will add both. 
#' 
#' \code{geom_errorbars} is the deprecated name for \code{stat_errorbar}. I realized that
#' \code{ggplot2} already had a \code{\link[ggplot2]{geom_errorbar}}, which would be very 
#' confusing. Additionally, since \code{geom_errorbars} operated more like a `stat` function 
#' (e.g., `stat_summary`), it made more sense to change its name.
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
#'   stat_errorbar(color="red") +
#'   xlab("WHAAAT")
#' @seealso \code{\link{stat_dots_and_bar}}
#' @export
stat_errorbar <- function(...) {
  return(list(ggplot2::stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", ...),
              ggplot2::stat_summary(fun.y = mean, geom = "point", ...)))
}

#' @rdname stat_errorbar
#' @export
geom_errorbars <- function(...) {
  .Deprecated("stat_errorbar")
  return(list(ggplot2::stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", ...),
              ggplot2::stat_summary(fun.y = mean, geom = "point", ...)))
}





#' Make dotplots with error bars with a single function
#'
#' Adds a dotplot, 95\% CI error bars, and a point representing the mean
#' to a ggplot object. Almost identical to \code{\link{stat_errorbar}}, but
#' with the obvious additional \code{\link[ggplot2]{geom_dotplot}}. \cr
#'  \cr
#' Right now, only uses default values for the error bars.
#' 
#' \code{geom_dots_and_bars} is the deprecated name for \code{stat_dots_and_bar}. I realized that
#' \code{ggplot2} already had a \code{\link[ggplot2]{geom_errorbar}}, which would be very 
#' confusing for \code{geom_dots_and_bars}'s sister function, \code{\link{geom_errorbars}}.
#' To keep the naming similarity, and because \code{geom_dots_and_bars} operated more 
#' like a `stat` function (e.g., `stat_summary`) than a `geom`, it made 
#' more sense to change its name.
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
#'   stat_dots_and_bar(color="red",fill="red") +
#'   xlab("WHAAAT")
#' @seealso \code{\link{stat_errorbar}}
#' @export
stat_dots_and_bar <- function(...,
                               binaxis = "y",
                               stackdir = "center",
                               dotsize = 0.5,
                               alpha=0.2) {
  dotplot <- list(ggplot2::geom_dotplot(binaxis = binaxis,
                                        stackdir = stackdir,
                                        dotsize = dotsize,
                                        alpha=alpha,
                                        ...))
  return(append(dotplot, stat_errorbar(...)))
}

#' @rdname stat_dots_and_bar
#' @export
geom_dots_and_bars <- function(...,
                               binaxis = "y",
                               stackdir = "center",
                               dotsize = 0.5,
                               alpha=0.2) {
  .Deprecated("stat_dots_and_bar")
  dotplot <- list(ggplot2::geom_dotplot(binaxis = binaxis,
                                        stackdir = stackdir,
                                        dotsize = dotsize,
                                        alpha=alpha,
                                        ...))
  return(append(dotplot, stat_errorbar(...)))
}
