#' Add columns from nested data frames
#'
#' Sometimes, when one is working with data frames that have data frames nested within
#' them (see \code{\link[tibble]{`tibble-package`}} or \code{\link[tidyr]{nest}}), one
#' will want to extract summary statistics or key aspects of information from the embedded
#' data frames and move them to columns in the top level.  This function applies summary
#' functions to the nested data frames and pulls them out into columns of the higher-level data frame.
#'
#' @param .data A data frame
#' @param data_col_name The bare name of the column of nested data frames
#' @param \dots the name-value pairs of summary functions (see \code{\link[dplyr]{summarise}} for more information)
#' @param handle_null_vals A boolean indicating whether rows with NULL values for the nested column should throw an error (`FALSE`) or should have NAs in the new columns.
#' @param scoped_in A boolean indicating whether the summary functions are scoped within the nested data frames alone (`TRUE`) or whether they also have access to the higher-level data frame. Changing this value can radically change the behavior.
#' @return a data frame / tibble
#' @examples
#' d <- mtcars %>%
#'   dplyr::mutate(Name=row.names(mtcars)) %>%
#'   as.tibble() %>%
#'   tidyr::nest(-cyl)
#'
#' d %>%
#'   summarise_from_sub(data, mean_mpg = mean(mpg),
#'                      sd_hp = sd(hp),
#'                      n=n())
#'
#' # Here we can see that if we set `scoped_in` to `FALSE`, `n()` will access the number of rows of the higher-level data frame instead of the nested ones. This could be useful in some circumstances, I just can't think of any.
#' d %>%
#'   summarise_from_sub(data, n=n(), scoped_in = FALSE)
#'
#' # If there's a NULL value in the nested column, by default it will throw an error
#' # If `handle_null_vals` is `TRUE`, then rows with NULL values will return NAs
#' d[2,]$data <- list(NULL)
#' d %>% summarise_from_sub(data, mean_mpg = mean(mpg), n=n())
#' d %>% summarise_from_sub(data, mean_mpg = mean(mpg), n=n(), handle_null_vals = TRUE)
#' @export
summarise_from_sub <- function(.data, data_col_name, ...,
                          handle_null_vals = FALSE,
                          scoped_in = TRUE) {
  # Possibly flexible in the future
  outer_function <- dplyr::summarise
  data_col_name <- rlang::enquo(data_col_name)
  stuff <- rlang::enquos(...) %>% rlang::quos_auto_name()

  null_indices <- .data[[quo_name(data_col_name)]] %>%
    purrr::map_lgl(is.null)

  if (handle_null_vals == FALSE && any(null_indices))
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





