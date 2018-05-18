#' Nested `ifelse` statements with less typing
#'
#' \emph{USE \code{dplyr}'s \code{\link[dplyr]{case_when}} for actual code!} This function is just code for my (Zach Burchill's) personal
#' reference in the future. I anticipate that the framework that I wrote here for might be useful for me in the future.
#'
#' @param \dots Paired unnamed arguments, where the first in each pair is an expression that evaluates to a logical vector, and the second is the replacement value. The last argument needs to be a single, unpaired default replacement value.
#' @return A vector of the same length as the logical vector that is the first argument
#' @examples
#' 
#' a <- runif(1000)
#' zifelse(a >= 0   & a <= 0.33, 1,
#'         a > 0.33 & a <= 0.66, 2,
#'         3)
#' @export
zifelse <- function(...) {
  a <- zplyr::args_and_kwargs(...) # separate args
  if (!is.null(a$kwargs) & 
      !rlang::is_empty(a$kwargs)) stop("There shouldn't be any named arguments!")
  
  len <- length(a$args)
  
  if (len %% 2 == 0) stop("There need to be an odd number of arguments (two for each ifelse, and one default value)")
  
  # Get every other element
  begin <- purrr::map2(a$args[ c(TRUE, FALSE)][1:((len-1)/2)], #exclude default
                       a$args[ c(FALSE, TRUE)],
                      ~paste0("ifelse(", rlang::quo_text(.x),
                              ", ", rlang::quo_text(.y), ", ")) %>%
    purrr::reduce(paste0)

  end <- paste0(rlang::quo_text(a$args[[len]]),
                paste0(rep(")", (len-1)/2), collapse = ""))

  q <- rlang::parse_quosure(paste0(begin,end))
  q <- rlang::quo_set_env(q, rlang::get_env(a$args[[1]]))
  return(rlang::eval_tidy(q))
}



# This approach doesn't actually work right, since it can't distinguish between
#   `s="X"` -> `.x==s, "X"` and `"s"="X"` -!-> `.x=="s", "X"`
# I like the named vs. unnamed approach though. Maybe it could work if I switched what the name and the value represented, e.g., `"mean(x)"=s` vs. `"mean(x)"="s"`.
# Nah, that wouldn't really solve the problem. I guess I could just require something like `"\"s\""="X"` but at that point...
zwitch <- function(.x, ...) {
  .x_expr <- rlang::enquo(.x) 
  print(enquos(...))
  
  a <- zplyr::args_and_kwargs(...) # separate args
  
  if (is.null(a$kwargs)) stop("Dots need to be named arguments!")
  
  # We only use one unnamed argument, and that's the default value
  # If there are more than one unnamed argument, we use the first one and give a warning
  if (!is.null(a$args) & length(a$args) > 1) {
    warning(paste0("Not using the following arguments: ",
                   paste0(purrr::map(a$args[2:length(a$args)], rlang::quo_text),
                          collapse = ", ")))
  }
  
  begin <- purrr::imap(a$kwargs,
                       ~paste0("ifelse(", rlang::quo_text(.x_expr),  
                               "==", .y, ", ", rlang::quo_text(.x), ", ")) %>% 
    unname() %>%
    purrr::reduce(paste0)
  
  end <- paste0(rlang::quo_text(a$args[[1]]), 
                paste0(rep(")",length(a$kwargs)), collapse = ""))
  
  q <- rlang::parse_quosure(paste0(begin,end))
  q <- rlang::quo_set_env(q, rlang::get_env(.x_expr))
  return(rlang::eval_tidy(q))
}

