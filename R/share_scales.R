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
#' ggplot(df, aes(x = x, color=f, fill=f)) +
#'    geom_histogram() +
#'    share_scales(scale_color_discrete, scale_fill_discrete,
#'                          name="Legend Label!",
#'                          breaks=c("Label1","Label2"),
#'                          labels=c("NewName1","NewName2")) +
#'    xlab("Name")
#' @export
share_scales <- function(...) {
  akw <- args_and_kwargs(...)
  geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
  geoms <- purrr::map(geom_func_list, ~quo_to_args(., akw$kwargs))
  return(geoms)
}





