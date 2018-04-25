#' Does Python-esque \code{*args, **kwargs} argument separation
#'
#' This function will return a named a separated list of named and unnamed arguments
#' as quosures.
#'
#' @param \dots Whatever mix of named and unnamed arguments you want
#' @examples
#' # Like how I made `share_discrete_scales`
#'
#' share_discrete_scales <- function(gg_obj, ...) {
#'   akw <- args_and_kwargs(...)
#'   # unnamed arguments are ggplot scale functions
#'   geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
#'   # named args are to be passed into these functions
#'   geoms <- purrr::map(geom_func_list, ~quo_to_args(., akw$kwargs))
#'   answer <- purrr::reduce(geoms, ggplot2::`%+%`, .init=gg_obj)
#'   return(answer)
#' }
#' @export
args_and_kwargs <- function(...) {
  qs <- rlang::quos(...)
  l <- list(args =   qs[names(qs) == ""],
            kwargs = qs[names(qs) != ""])
  return(l)
}

#' Pass quosures into a function as arguments
#'
#' Generally to be paired with \code{\link{args_and_kwargs}}, \code{quo_to_args}
#' passes in a quosure or list of quosures (i.e. from \code{\link[rlang]{quos}})
#' into the supplied function as arguments to that function.
#'
#' @param .f The function the arguments will be passed into
#' @param quosures A quosure or list of quosures
#' @examples
#' px <- "Hello"
#' p <- function(x) print(x)
#' quo_to_args(p, quo(px))
#'
#' f <- function(x, y) x/y
#' quo_to_args(f, quos(3+3, 3))
#' quo_to_args(f, quos(y=3+3, x=3))
#'
#' m <- function(x, y, ...) paste0(x," ",y)
#' quo_to_args(m, quos(y="World", "HAHAHHA", x="Hello"))
#' @export
quo_to_args <- function(.f, quosures) {
  if (rlang::is_list(quosures)) {
    kwargs <- purrr::map(quosures, rlang::eval_tidy)
  } else {
    kwargs <- list(rlang::eval_tidy(quosures))
  }
  rlang::eval_tidy(rlang::quo(.f(!!!kwargs)))
}

