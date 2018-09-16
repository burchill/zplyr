#' Collect warnings/errors/messages from an expression without rerunning it
#'
#' \code{collect_all} wraps expressions and returns the result of the expression along with
#' a list of warnings, errors, and messages raised by running the expression,
#' without having to run the expression more than once.
#'
#' I've personally found R's warning and message handling very confusing, and this represents "good enough" code for me.
#' Using \href{http://stackoverflow.com/questions/3903157/}{Aaron's answer to a question on stackexchange},
#' I was able to understand enough of it to make a function that would collect all the warnings and messages
#' raised by an expression and still run the code only once. (All other examples I encountered seemed to need to run
#' the code twice to get both the result and the warnings.) \cr
#' If, say, you're running a lot of models
#' all at once, then having to rerun the code (as most tutorials/answers to warning handling with R suggest)
#' would be a total pain in the butt.
#'
#' @param expr The expression you want to catch warnings and messages for.
#' @param catchErrors A boolean which, if true, will catch error messages just like it catches warnings and messages. It will then return \code{NA} as the value.
#' @param asStrings A boolean which, if true, will convert the conditions into strings.
#' @return A named list with the result of the expression, the warnings, and the messages raised by
#' the expression
#' @examples
#' # Let's say that `run_model_once(x)` fits a randomly generated glmer model with
#' #   a seed of `x`, as one might do in a power simulation
#' \dontrun{results = data.frame(IterationNumber = seq(NUMBER_ITERATIONS))
#' results = results %>%
#'   dplyr::tbl_df() %>%
#'   dplyr::mutate(models = purrr::map(IterationNumber,
#'    ~zplyr::collect_all(run_model_once(.))))
#' }
#' @export
collect_all <- function(expr, catchErrors = FALSE, asStrings = TRUE) {
  if (asStrings == TRUE)
    convert <- function(x) Map(as.character, x)
  else convert <- identity

  myErrors <- myWarnings <- myMessages <- NULL

  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  mHandler <- function(m) {
    myMessages <<- c(myMessages, list(m))
    invokeRestart("muffleMessage")
  }
  eHandler <- function(e) {
    myErrors <<- c(myErrors, list(e))
    invokeRestart("return_NA")
  }

  # Not the prettiest code, I'll give you that
  if (catchErrors) {
    val <- withCallingHandlers(
      withRestarts(expr, return_NA = function(x) NA),
      warning = wHandler, message = mHandler, error = eHandler)
    myWarningList <- convert(myWarnings)
    myMessageList <- convert(myMessages)
    myErrorList  <-  convert(myErrors)
    list(value = val, warnings = myWarningList, messages = myMessageList, errors = myErrorList)
  } else {
    val <- withCallingHandlers(expr, warning = wHandler, message = mHandler)
    myWarningList <- convert(myWarnings)
    myMessageList <- convert(myMessages)
    list(value = val, warnings = myWarningList, messages = myMessageList)
  }
}
