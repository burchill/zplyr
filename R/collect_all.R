#' Collect warnings/errors/messages from an expression without rerunning it
#'
#' \code{collect_all} wraps expressions and returns the result of the expression,
#' and lists for warnings, errors, and messages by the expression,
#' without having to run the expression more than once.
#'
#' I've personally found R's warning and message handling very confusing, and this represents "good enough" code for me.
#' Using Aaron's answer to a question on stackexchange (http://stackoverflow.com/questions/3903157/),
#' I was able to understand enough to make a function that would collect all the warnings and messages
#' raised by an expression and still run the code only once. If, say, you're running a lot of models
#' all at once, then having to rerun the code (as most tutorials/answers to warning handling with R suggest)
#' would be a total pain in the butt.
#'
#' @param expr the expression you want to catch warnings and messages for
#' @param catchErrors a boolean that, if true, will catch error messages just like it catches warnings and messages. It will then return \code{NA} as the value. I generally would not recommend using this, because I understood how this worked the least.
#' @return a named list with the result of the expression, the warnings, and the messages raised by
#' the expression
#'
#' @export
collect_all <- function(expr, catchErrors=FALSE) {
  myErrors   <- NULL
  myWarnings <- NULL
  myMessages <- NULL
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
    myWarningList <- Map(as.character, myWarnings)
    myMessageList <- Map(as.character, myMessages)
    myErrorList  <-  Map(as.character, myErrors)
    list(value = val, warnings = myWarningList, messages = myMessageList, errors = myErrorList)
  } else {
    val <- withCallingHandlers(expr, warning = wHandler, message = mHandler)
    myWarningList <- Map(as.character, myWarnings)
    myMessageList <- Map(as.character, myMessages)
    list(value = val, warnings = myWarningList, messages = myMessageList)
  }
}
