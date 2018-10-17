#' Add columns from nested data frames
#'
#' Sometimes, when one is working with data frames that have data frames nested within
#' them (see \code{\link[tibble]{tibble-package}} or \code{\link[tidyr]{nest}}), one
#' will want to extract summary statistics or key aspects of information from the embedded
#' data frames and move them to columns in the top level.  This function applies summary
#' functions to the nested data frames and pulls them out into columns of the higher-level data frame.
#'
#' @param .data A data frame
#' @param data_col_name The column name of the nested data frames, bare or as a string.
#' @param \dots the name-value pairs of summary functions (see \code{\link[dplyr]{summarise}} for more information)
#' @param handle_nulls A boolean indicating whether rows with NULL values for the nested column should throw an error (`FALSE`) or should have NAs in the new columns.
#' @param scoped_in A boolean indicating whether the summary functions are scoped within the nested data frames alone (`TRUE`) or whether they also have access to the higher-level data frame. Changing this value can radically change the behavior.
#' @return A data frame / tibble
#' @examples
#' d <- mtcars %>%
#'   dplyr::mutate(Name=row.names(mtcars)) %>%
#'   as.tibble() %>%
#'   tidyr::nest(-cyl)
#'
#' d %>%
#'   summarise_sub(data, mean_mpg = mean(mpg),
#'                      sd_hp = sd(hp),
#'                      n=n())
#'
#' # Here we can see that if we set `scoped_in` to `FALSE`, `n()` will access the number of rows of the higher-level data frame instead of the nested ones. This could be useful in some circumstances, I just can't think of any.
#' d %>%
#'   summarise_sub(data, n=n(), scoped_in = FALSE)
#'
#' # If there's a NULL value in the nested column, by default it will throw an error
#' # If `handle_nulls` is `TRUE`, then rows with NULL values will return NAs
#' d[2,]$data <- list(NULL)
#' d %>% summarise_sub(data, mean_mpg = mean(mpg), n=n())
#' d %>% summarise_sub(data, mean_mpg = mean(mpg), n=n(), handle_nulls = TRUE)
#' @export
summarise_sub <- function(.data, data_col_name, ...,
                          handle_nulls = FALSE,
                          scoped_in = TRUE) {
  # Possibly flexible in the future
  outer_function <- dplyr::summarise
  stuff <- rlang::enquos(...) %>% rlang::quos_auto_name()

  # If it's a string, turn it into a quosure
  if (rlang::is_string(substitute(data_col_name)))
    data_col_name <- rlang::quo( !! rlang::sym(colname))
  # Otherwise, assume it's bare
  else data_col_name <- rlang::enquo(data_col_name)

  null_indices <- .data[[quo_name(data_col_name)]] %>%
    purrr::map_lgl(is.null)
  if (handle_nulls == FALSE && any(null_indices))
    stop("NULL nested data frames!")

  # So that things can scope right
  scoper <- function(.inner_df) outer_function(.inner_df, !!! stuff)

  .data <- .data %>%
    mutate(`ZachsTempCounterForOrderingThings!!!` = seq_along(!! data_col_name))

  unnested_vals <- .data[!null_indices,] %>%
    # Hack, but any other way is amazingly annoying
  {
    dval <- .
    if (scoped_in == TRUE) mutate(dval, NewVals = map(!! data_col_name, scoper))
    else mutate(dval, NewVals = map(!! data_col_name, ~outer_function(., !!! stuff)))
  } %>%
    tidyr::unnest(NewVals) %>%
    select(c(names(.data), names(stuff)))

  new_data <- bind_rows(.data[null_indices,],
                        unnested_vals) %>%
    arrange(`ZachsTempCounterForOrderingThings!!!`) %>%
    select(-`ZachsTempCounterForOrderingThings!!!`)

  stopifnot(nrow(new_data) == nrow(.data))
  new_data
}


#' Drop rows with empty nested data frames
#'
#' Drops rows that for a given column of nested data frames, have `NULL` values. Optionally, rows with nested data frames that have 0 rows can also be dropped.
#'
#' @param .data A data frame / tibble
#' @param data_col_name The column name of the nested data frames, bare or as a string.
#' @param empty_df If `TRUE`, also drops rows with data frames that have 0 rows.
#' @export
drop_empty_subs <- function(.data, data_col_name, empty_df = TRUE) {
  # If it's a string, turn it into a quosure
  if (rlang::is_string(substitute(data_col_name)))
    data_col_name <- rlang::quo( !! rlang::sym(colname))
  # Otherwise, assume it's bare
  else data_col_name <- rlang::enquo(data_col_name)

  if (empty_df == T) pred <- ~!is.null(.) && nrow(.) > 0
  else
    pred <- ~!is.null(.)
  .data %>%
    filter(purrr::map_lgl(!! data_col_name, pred))
}

#' Filter within nested data frames
#'
#' Applies \code{\link[dplyr]{filter}} within nested data frames.
#'
#' @param .data A data frame / tibble
#' @param data_col_name The column name of the nested data frames, bare or as a string.
#' @param \dots Logical predicates defined in terms of the variables in `.data`. Multiple conditions are combined with `&`. Only rows where the condition evaluates to `TRUE` are kept. See \code{\link[dplyr]{filter}} for more information.
#' @param handle_nulls If `TRUE`, drops rows with `NULL` data frames. Otherwise will throw an error if they are encountered.
#' @param drop_empty If `TRUE`, will drop rows that, after filtering, have no rows.
#' @param scoped_in A boolean indicating whether the summary functions are scoped within the nested data frames alone (`TRUE`) or whether they also have access to the higher-level data frame. Changing this value can radically change the behavior.
#' @return A data frame / tibble
#' @seealso \code{\link{filter_by_sub}}
#' @export
filter_in_sub <- function(.data, data_col_name, ...,
                          handle_nulls = FALSE,
                          drop_empty = FALSE,
                          scoped_in = TRUE) {
  # If it's a string, turn it into a quosure
  if (rlang::is_string(substitute(data_col_name)))
    data_col_name <- rlang::quo( !! rlang::sym(colname))
  # Otherwise, assume it's bare
  else data_col_name <- rlang::enquo(data_col_name)

  stuff <- rlang::enquos(...)
  # So that things can scope right
  scoper <- function(.inner_df) filter(.inner_df, !!! stuff)

  null_indices <- .data[[quo_name(data_col_name)]] %>%
    purrr::map_lgl(is.null)
  if (handle_nulls == FALSE && any(null_indices))
    stop("NULL nested data frames!")

  if (any(purrr::map_lgl(.data[[quo_name(data_col_name)]], is_grouped_df)))
    warning("Some nested data frames are grouped. `filter_in_sub` will be respecting these groups in filtering.")

  df <- df[!null_indices,] %>%
    mutate(!! rlang::quo_name(data_col_name) := purrr::map(!! data_col_name, scoper))
  if (drop_empty == TRUE) df %>% drop_empty_subs(!! data_col_name)
  else df
}






#' Filter *by* nested data frames
#'
#' Applies \code{\link[dplyr]{filter}} to rows in a data frame based on the results of that row's nested data frame. Each logical predicate supplied to \dots must evaluate to a logical of length 1, similar to \code{\link[dplyr]{summarise}} (which this function calls).
#'
#' Unlike \code{\link{filter_in_sub}}, which applies a filter *within* the nested data frames, `filter_by_sub` applies the filter to the top-level data frame.
#'
#' @param .data A data frame / tibble
#' @param data_col_name The column name of the nested data frames, bare or as a string.
#' @param \dots Logical predicates defined in terms of the variables in `.data`, that evaluate to a length of 1. Multiple conditions are combined with `&`. Only rows where the condition evaluates to `TRUE` are kept.
#' @param handle_nulls If `TRUE`, drops rows with `NULL` data frames. Otherwise will throw an error if they are encountered.
#' @param drop_empty If `TRUE`, will drop rows that, after filtering, have no rows.
#' @return A data frame / tibble
#' @seealso \code{\link{filter_in_sub}}
#' @examples
#' d <- mtcars %>%
#'   dplyr::mutate(Name=row.names(mtcars)) %>%
#'   as.tibble() %>%
#'   tidyr::nest(-cyl)
#'
#' d %>% filter_by_sub(data, any(grepl("Merc", Name)), n() > 12)
#' # We can see what happens when we make a nested data frame NULL and if we make it a row of 0
#' d[2,]$data <- list(NULL)
#' d[1,]$data <- list(d[1,]$data[[1]][FALSE,])
#' d %>% filter_by_sub(data, any(grepl("Merc", Name)), all(mpg < 20))
#' d %>% filter_by_sub(data, any(grepl("Merc", Name)), all(mpg < 20), handle_nulls = TRUE)
#' @export
filter_by_sub <- function(.data, data_col_name, ...,
                          handle_nulls = FALSE) {
  # If it's a string, turn it into a quosure
  if (rlang::is_string(substitute(data_col_name)))
    data_col_name <- rlang::quo( !! rlang::sym(colname))
  # Otherwise, assume it's bare
  else data_col_name <- rlang::enquo(data_col_name)

  stuff <- rlang::enquos(...)

  null_indices <- .data[[quo_name(data_col_name)]] %>%
    purrr::map_lgl(is.null)
  if (handle_nulls == FALSE && any(null_indices))
    stop("NULL nested data frames!")

  if (any(purrr::map_lgl(.data[[quo_name(data_col_name)]], is_grouped_df)))
    warning("Some nested data frames are grouped. These groups will be discarded when filtering.")

  .data[!null_indices,] %>%
    filter(purrr::map_lgl(!! data_col_name, ~(eval_sub_df(., stuff))))
}


eval_sub_df <- function(.data, quosures) {
  # this is taken from dplyr
  if (any(rlang::have_name(quosures))) {
    bad <- quosures[rlang::have_name(quosures)]
    # Just in case they change the name in later versions!
    if (exists('bad_eq_ops', where=asNamespace('dplyr'), mode="function"))
      dplyr:::bad_eq_ops(bad, "must not be named, do you need `==`?")
    else stop("Filter commands must not be named, do you need `==`?")
  } else if (rlang::is_empty(quosures)) {
    return(TRUE)
  }

  d1 <- .data %>% ungroup() %>% summarise( !!! quosures)
  all_logical <- d1 %>% mutate_all(rlang::is_logical) %>%
    unlist(use.names = FALSE) %>% all()
  if (!all_logical)
    stop("Not all filtering statements evaluate to logicals!")
  d1 %>% unlist(use.names = FALSE) %>% all()
}

