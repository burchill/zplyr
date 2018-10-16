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

#' Share arguments amongst multiple ggplot functions
#'
#' Often it is the case that one wishes to pass identical arguments to
#' multiple functions in \code{ggplot}.  For example, if one wants to scale
#' both \code{color} and \code{fill} with the same breaks and legend names/labels. \cr
#' The function \code{share_scales} lets you send identical arguments to a list of \code{ggplot} functions you
#' specify.
#'
#' @param \dots Unnamed arguments should be \code{ggplot2} functions
#' (e.g., \code{scale_color_discrete}, \code{scale_fill_discrete}, etc.) and
#' named arguments should be whatever arguments you want passed to these functions
#' @return A list of the \code{ggproto} objects
#' @examples
#' df <- data.frame(
#'    x<-rnorm(100),
#'    f<-c(rep("Label1",50), rep("Label2",50))
#' )
#'
#' l <- c(1,2,3,4,5,6)
#' df <- data.frame(x = rgamma(100, 2, 2),
#'                  y = factor(sample(l, size=100, replace = TRUE)))
#'
#'   ggplot(df, aes(x=x, y=y, size=y, color=y, fill=y, shape=y)) +
#'   geom_point() +
#'   # Zplyr function
#'   share_scales(scale_color_discrete,  scale_size_discrete,
#'                scale_fill_discrete, scale_shape_discrete,
#'                name="Legend Label!",
#'                breaks=c(1,2,3,4,5,6),
#'                labels=c("uno","dos","tres","catorce","FIVE","siiiiiix"))
#' @export
share_scales <- function(...) {
  akw <- args_and_kwargs(...)
  geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
  geoms <- purrr::map(geom_func_list, ~quo_to_args(., akw$kwargs))
  return(geoms)
}



#' (Deprecated!) Share arguments amongst multiple ggplot functions
#'
#' \strong{DEPRECATED AS HELL, please use \code{\link{share_scales}} instead.} \cr
#' Often it is the case that one wishes to pass identical arguments to
#' multiple functions in \code{ggplot}.  For example, if one wants to scale
#' both \code{color} and \code{fill} with the same breaks and legend names/labels. \cr
#' The function \code{share_discrete_scales} lets you send identical arguments to a list of \code{ggplot} functions you
#' specify.  Note that you need to pipe the ggplot object into this function. If you don't put
#' this function immediately after invoking \code{ggplot(\dots)}, the preceding objects should be added to the
#' \code{ggplot} object with \code{\%+\%} instead of \code{+}.
#'
#' @param gg_obj The ggplot object
#' @param geom_func_list A vector or list of \code{ggplot} functions, e.g., \code{c(scale_color_discrete, scale_fill_discrete)}
#' @param \dots Whatever arguments you want passed to these functions
#' @examples
#' \dontrun{
#' df <- data.frame(
#'    x=rnorm(100),
#'    f=c(rep("Label1",50), rep("Label2",50))
#' )
#'
#' ggplot(df, aes(x = x, color=f, fill=f)) %+%
#'    geom_histogram() %>%
#'    share_discrete_scales(c(scale_color_discrete, scale_fill_discrete),
#'                          name="Legend Label!",
#'                          breaks=c("Label1","Label2"),
#'                          labels=c("NewName1","NewName2")) +
#'    xlab("Name")
#' }
#' @seealso \code{\link{share_scales}}
#' @export
share_discrete_scales <- function(gg_obj, geom_func_list, ...) {
  .Deprecated("share_scales")
  geoms <- Map(function(f) { f(...) }, geom_func_list)
  answer <- Reduce(function(a,b) {ggplot2::`%+%`(a,b)},
                   geoms, init=gg_obj)
  return(answer)
}

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
#' @param binaxis Set by default to "y"
#' @param stackdir Set by default to "center"
#' @param dotsize Set by default to 0.5
#' @param alpha Set by default to 0.2
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




