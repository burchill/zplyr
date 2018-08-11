#' Separate \dots into Python-esque \code{*args} and \code{**kwargs}
#'
#' This function will return a named a separated list of named and unnamed arguments
#' as quosures.
#'
#' @param \dots Whatever mix of named and unnamed arguments you want
#' @param .already_quosure if the arguments are already all quosures (in which case it will just sort them by named vs. unnamed arguments)
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
args_and_kwargs <- function(..., .already_quosure = FALSE) {
  if (.already_quosure == TRUE) qs <- list(...)
  else qs <- rlang::enquos(...)

  l <- list(args =   qs[name_vec(qs) == ""],
            kwargs = qs[name_vec(qs) != ""])
  return(l)
}

# Returns a vector of names of a list/vector, and if there aren't any, it makes them ""
name_vec <- function(l) {
  if (is.null(names(l))) rep("", length(l))
  else names(l)
}


#' Pass quosures into a function as arguments
#'
#' Generally to be paired with \code{\link{args_and_kwargs}}, \code{quo_to_args}
#' passes in a quosure or list of quosures (i.e. from \code{\link[rlang]{quos}})
#' into the supplied function as arguments to that function.
#'
#' This function has not been tested much with the inclusion of the non-quosure \dots
#' arguments. It gets a little fly-by-night beyond simple passing of quosures.
#'
#' @param .f The function the arguments will be passed into
#' @param quosures A quosure or list of quosures
#' @param \dots Any other arguments to be passed into `.f`. (Unless the ordering fits perfectly, i.e. almost never, use named arguments)
#' @param .quos_first whether the quosures should be inputted to `.f` before or after the other arguments. This is just for flexibility in some edge cases and users should try to avoid the need to change use this by naming the other arguments
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
quo_to_args <- function(.f, quosures, ..., .quos_first=TRUE) {
  other_args <- list(...)

  if (rlang::is_list(quosures)) kwargs <- purrr::map(quosures, rlang::eval_tidy)
  else  kwargs <- list(rlang::eval_tidy(quosures))

  if (.quos_first == TRUE) kwargs <- rlang::list2(!!!kwargs, !!!other_args)
  else kwargs <- rlang::list2(!!!other_args, !!!kwargs)

  rlang::eval_tidy(rlang::quo(.f(!!!kwargs)))
}

