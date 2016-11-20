#' Combine/rename/reorder levels in a factor
#'
#' Instead of using  \code{ifelse} statements to combine values in a factor
#' (e.g. when you want to simplify variables for a model), you can rename,
#' combine, and reorder the levels of a factor with one easy list. If there's an empty
#' level that isn't included, \code{merge_factor} will warn you, but go ahead and remove it.
#'
#' The same effect \strong{could} be achieved with something like:
#' \code{levels(my_factor)<-c(old1=new1,old2=new1,old3=new2,old4=new2)},
#' but \code{merge_factor()} saves typing by letting you type the
#' inverse--in essence: \code{levels(my_factor)<-list(new1=c(old1,old2),new2=c(old3,old4))}.
#'
#'
#' @param .data the factor you want to respecify
#' @param arg_list a list whose names are the new levels, whose values are the old levels, and whose order is the new order of the levels
#' @return a factor with levels and values as you specified
#' @examples
#' my_factor <- factor(c("d","b","c","d","a","a","d","d"))
#' levels(my_factor)
#' merge_factor(my_factor, list("CIsFirst"="c","AandB"=c("a","b"),"d"))
#'
#' @export
merge_factor <- function(.data, arg_list) {
  arg_names <- names(arg_list)
  arg_names_expanded <- Reduce(
    function(x,i) {
      if (arg_names[i]=="") {
        if (length(arg_list[[i]]) > 1) {
          stop("Yer typin' it in wrong, laddy. All vectors have to be named")
        } else { new_val <- arg_list[[i]] }
      } else {new_val <- rep(arg_names[i], length(arg_list[[i]])) }
      c(x, new_val) },
    seq(1:length(arg_names)),
    init=c())

  new_factor <- .data
  arg_vals <- unlist(arg_list, use.names = FALSE)

  if (!all(arg_vals %in% unique(.data))) {
    newlevs<-arg_vals[arg_vals %in% unique(.data)]
    stop(paste0("Levels referenced in factor don't exist: ",
                paste0(newlevs, collapse = ", ")))
  }
  if (!all(unique(.data) %in% arg_vals)) {
    missedlevs <- unique(.data)[unique(.data) %in% arg_vals]
    stop(paste0("Not all non-empty levels in factor are covered: ",
                paste0(missedlevs, collapse = ", ")))
  }
  if (!all(levels(.data) %in% arg_vals)) {
    warning("An empty level has been removed")
    new_factor<-factor(new_factor)
  }
  # for levels(x) <- ... it's: c(old_val = new_val, ...)
  levels(new_factor) <- setNames(arg_names_expanded, arg_vals)
  return(new_factor)
}


#' Combine specified levels of a factor
#'
#' Similar to \code{merge_factor()} but generally reserved for situations
#' when you don't need to worry about specifying the entire factor.
#' Instead of taking a list of arguments as input, it takes named values.
#' The old levels that aren't altered stay put order-wise, and the new ones are swapped in to where their old levels were, as much as possible.
#'
#' @param .data the factor you want to re-specify
#' @param \dots values that must be named, where the name corresponds to the new level and the value corresponds to the old level. The values can be strings, numbers, of vectors of those.
#' @return a factor
#' @examples
#' my_factor <- factor(c("d","b","c","d","a","a"))
#' levels(my_factor)
#' merge_levels(my_factor,"AandB"=c("a","b"),"RenamedC"="c"))
#'
#' @export
merge_levels <- function(.data, ...) {
  arg_list <- list(...)
  arg_names <- names(arg_list)
  if ("" %in% arg_names) {
    stop("All levels need names")
  }
  arg_names_expanded <- Reduce(
    function(x,i) c(x, rep(arg_names[i], length(arg_list[[i]]))),
    seq(1:length(arg_names)),
    init=c())

  arg_vals <- unlist(arg_list, use.names = FALSE)
  if (!all(arg_vals %in% unique(.data))) {
    newlevs<-arg_vals[arg_vals %in% unique(.data)]
    stop(paste0("Levels referenced in factor don't exist: ",
                paste0(newlevs, collapse = ", ")))
  }
  # for levels(x) <- ... it's: c(old_val = new_val, ...)
  new_vec <- setNames(arg_names_expanded, arg_vals)
  old_levels <- levels(.data)
  levels(.data) <- ifelse(is.na(new_vec[old_levels]), old_levels, new_vec[old_levels])
  return(.data)
}

