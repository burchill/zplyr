#' A 'safer' command-line restart (for RStudio)
#'
#' Removes all currently loaded objects and restarts RStudio, after prompting
#' the user if they really want to do it. \emph{Do NOT change/specify the `counter`
#' argument unless you set it to `2`, which forces a restart without prompting}.
#'
#' When I want to restart my RStudio R session anew, I have, in the past,
#' clicked on the broom icon, clicked 'yes' to remove all the current objects
#' in the environment, clicked on 'Session', and then clicked 'Restart R'.
#' This function does all that with one command, and prompts the user to confirm,
#' as a way of being safe.
#' 
#' This code takes inspiration and credit from two sources: Adam Lee Perelman's answer at https://stackoverflow.com/questions/6313079/quit-and-restart-a-clean-r-session-from-within-r, and Romain Francois' `nothing` package (https://github.com/romainfrancois/nothing/blob/master/R/zzz.R).
#'
#' @param counter Do not specify unless you set it to `2`, which will force a restart without prompting the user.
#' @return `NULL` (because I'm a lazy programmer)
#' @export
start_fresh <- function(counter=0) {
  if (counter != 0 & counter !=1 & counter !=2) stop("Do not supply any arguments to `clear_all`!")

  try_restart_rstudio <- function() {
    try_func <- function(warning_m) {
      if (exists(".rs.restartR")) {
        get0(".rs.restartR")()
      } else {
        warning(warning_m)
      }
    }

    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      if (rstudioapi::isAvailable("1.1.281")) {
        rstudioapi::callFun("restartSession")
      } else {
        try_func("Can't find out how to restart your RStudio session. Please restart manually!")
      }
    } else {
      try_func("RStudio doesn't seem to be installed. Please restart manually!")
    }
  }





  # Adapted from Adam Lee Perelman's answer in:
  # https://stackoverflow.com/questions/6313079/quit-and-restart-a-clean-r-session-from-within-r
  do_it <- function() {
    rm(list = ls(envir = globalenv()), envir = globalenv())
    # unloads packages as well
    x <- as.data.frame(installed.packages())
    x <- x[!is.na(x$Priority) & x$Priority=="base", c("Package", "Priority")]
    base_names <- x$Package
    
    # From https://github.com/romainfrancois/nothing/blob/master/R/zzz.R
    repeat{
      pkgs <- setdiff(loadedNamespaces(), base_names)
      if (!length(pkgs)) break
      for (pkg in pkgs) {
        try(unloadNamespace(pkg), silent = TRUE)
      }
    }
    
    # if (length(names(sessionInfo()$otherPkgs)) > 0)
    #   lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""), detach, character.only=TRUE, unload=TRUE)
    gc()
    try_restart_rstudio()
  }

  # To force it to restart without prompting
  if (counter==2) return(do_it)

  p <- "Remove all objects from environment? Can't be undone. (y/n) "
  if (counter==1) p <- "Empty line was returned. Press enter again to clear, or n to cancel."
  response <- readline(prompt=p)

  if (grepl("[yY]", response)) {
    return(do_it())
  } else if (grepl("[nN]", response)) {
    return()
  } else if (response=="") {
    if (counter==0) return(start_fresh(1))
    return(do_it())
  }
}

