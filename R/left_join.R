#' Warns for "unexpected" behavior of \code{\link[dplyr]{left_join}}
#'
#' This code is purposefully intended to hide \code{dplyr}'s \code{\link[dplyr]{left_join}} with a
#' wrapper for the same function, but with a warning if the number of rows of the resulting table is greater
#' than the input's. I generally forget about this behavior, so I'm just doing this to help myself remember. \cr \cr
#' The parameter descriptions are the same as \code{\link[dplyr]{left_join}}.
#'
#' @param x,y tbls to join
#' @param by a character vector of variables to join by.  If `NULL`, the
#'   default, `*_join()` will do a natural join, using all variables with
#'   common names across the two tables. A message lists the variables so
#'   that you can check they're right (to suppress the message, simply
#'   explicitly list the variables that you want to join).
#'
#'   To join by different variables on x and y use a named vector.
#'   For example, `by = c("a" = "b")` will match `x.a` to
#'   `y.b`.
#' @param copy If `x` and `y` are not from the same data source,
#'   and `copy` is `TRUE`, then `y` will be copied into the
#'   same src as `x`.  This allows you to join tables across srcs, but
#'   it is a potentially expensive operation so you must opt into it.
#' @param suffix If there are non-joined duplicate variables in `x` and
#'   `y`, these suffixes will be added to the output to disambiguate them.
#'   Should be a character vector of length 2.
#' @param ... other parameters passed onto methods, for instance, `na_matches`
#'   to control how `NA` values are matched.  See \link{join.tbl_df} for more.
#' @export left_join
left_join <- function(x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), ...) {
  nrow_x <- nrow(x)
  answer <- dplyr::left_join(x, y, by, copy, suffix, ...)
  if (nrow(answer) > nrow_x) {
    warning("zplyr warning: left_join has ADDED *EXTRA* rows!!!")
  }
  return(answer)
}
