#' Separate \dots into Python-esque \code{*args} and \code{**kwargs}
#'
#' This function will return a named a separated list of named and unnamed arguments
#' as quosures.
#'
#' @param \dots Whatever mix of named and unnamed arguments you want
#' @return A named list of lists, with `$args` being a list of quosures of the unnamed arguments and `$kwargs` being a list of quosures of the named arguments.
#' @examples
#'
#' x <- args_and_kwargs(unnamed_1, named_1="ba", "unnamed_2", named_2 = letters)
#' print(x$args)
#' print(x$kwargs)
#'
#'
#' # Or like how I made `share_scales`
#' share_scales <- function(...) {
#'   akw <- args_and_kwargs(...)
#'   # Unnamed arguments are ggplot scales
#'   geom_func_list <- purrr::map(akw$args, rlang::eval_tidy)
#'   # Named arguments are to be passed into those scales
#'   geoms <- purrr::map(geom_func_list, ~quo_to_args(., akw$kwargs))
#'   return(geoms)
#' }
#' @export
args_and_kwargs <- function(...) {
  qs <- rlang::enquos(...)
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

