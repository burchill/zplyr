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
#' @seealso \code{\link{share_scales}}
#' @export
share_discrete_scales <- function(gg_obj, geom_func_list, ...) {
  .Deprecated("share_scales")
  geoms <- Map(function(f) { f(...) }, geom_func_list)
  answer <- Reduce(function(a,b) {ggplot2::`%+%`(a,b)},
                   geoms, init=gg_obj)
  return(answer)
}




