#' Print within pipes
#'
#' This function can be placed within a sequence of `%>%` pipes for debugging purposes.
#' It will literally print the output of the function you specify on whatever was passed into it, and then pass it out, untouched.  If you want to just print a string with no bearing on whatever is being piped, then set the `.text` variable to that string.
#'
#' @param .x Whatever is being piped down the pipeline
#' @param .f A function, formula, or atomic vector, which will be applied to `.x` and whose output will be printed. `print_and_pass()` uses the \code{\link[purrr]{as_mapper}} to turn this value into a function, so see that documentation. By default, it will just print `x`. 
#' @param .text When explicitly set to a non-`NULL` value, `print_and_pass` with just print that value instead. It is intended to be a string, if used.
#' @param \dots Additional \emph{named} arguments for whatever function is being passed in.
#' @return The value of `.x`
#' @examples
#' df <- data.frame(x=runif(10))
#' new_df <- df %>%
#'   print_and_pass(.text="BEGIN DEBUG") %>%
#'   dplyr::mutate(y=x+4) %>%
#'   print_and_pass(~.$y) %>%
#'   dplyr::filter(y>4) %>%
#'   print_and_pass(.text="END DEBUG")
#' @export
print_and_pass <- function(.x, .f=identity, .text=NULL, ...) {
  if (!is.null(.text)) print(.text)
  else {
    .f <- purrr::as_mapper(.f, ...)
    print(.f(.x)) 
  }
  return(.x)
}
  