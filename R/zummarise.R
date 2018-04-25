#' Summarises and then ungroups a tbl
#'
#' When I first started using \code{dplyr}, I thought \code{\link[dplyr]{summarise}}
#' would completely ungroup the object it was called on. Later, I learned that
#' it only peeled back the last-named grouping argument. To make sure my code was
#' doing what I wanted it to, I created this function as a way of implementing what
#' I thought \code{\link[dplyr]{summarise}} actually did
#'
#' @param .data A tbl. I believe that all main verbs are S3 generics and provide methods for \code{\link[dplyr]{tbl_df}}, \code{\link[dplyr]{tbl_dt}}, and \code{\link[dplyr]{tbl_sql}} since this is built on \code{dplyr} code.
#' @param ... From \code{dplyr}: Name-value pairs of summary functions. The name will be the
#'   name of the variable in the result. The value should be an expression
#'   that returns a single value like `min(x)`, `n()`, or `sum(is.na(y))`.
#'
#'   These arguments are automatically [quoted][rlang::quo] and
#'   [evaluated][rlang::eval_tidy] in the context of the data
#'   frame. They support [unquoting][rlang::quasiquotation] and
#'   splicing. See `vignette("programming")` for an introduction to
#'   these concepts.
#' @return An object of the same class as \code{.data}. \strong{All} grouping levels will be dropped. \cr \cr
#' Data frame row names are silently dropped. To preserve, convert to an explicit variable.
#' @examples
#' zummarise(dplyr::group_by(mtcars, cyl, gear), mean(disp))
#' @export zummarise
zummarise <- function(.data, ...) {
  dplyr::ungroup(
    dplyr::summarise_(.data, .dots = lazyeval::lazy_dots(...))
  )
}




