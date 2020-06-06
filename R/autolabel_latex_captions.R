#' Autolabel figures in Latex
#'
#' I hate having to add `\\label{fig:blah}` to `fig.cap` in Latex .Rmd files.  There's probably a better way to do this, but if you call this function, it will automatically add `\\label{fig:<label>}` to the `fig.cap` option of any chunk with a `fig.cap` and a label, substituting whitespace stretches (after trimming it)
#'
#' @return Nothing
#' @export
autolabel_latex_figs <- function() {
  knitr::opts_hooks$set(
    .autolabel =
      function(options) {
        if (!is.null(options$label) & length(options$fig.cap) > 0) {
          label = trimws(gsub("\\s+", "_", options$label))
          options$fig.cap = paste0(options$fig.cap,
                                   "\\label{fig:",label,"}")
          options
        } else {
          options
        }
      })
  knitr::opts_chunk$set(.autolabel = TRUE)
}

#' Beep on successful knit
#'
#' This function requires the `beepr` package. It will play a sound when the document finishes knitting.
#'
#' @param beep_sound Input to `beepr::beep()` for what sound to play
#' @param sleep The amount of seconds to sleep before moving on (so that the sound isn't cut off after it knits)
#'
#' @return Nothing
#' @export
beep_on_knit <- function(beep_sound=3, sleep=4) {
  library(beepr)
  prev_fn <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    beepr::beep(beep_sound)
    Sys.sleep(sleep)
    prev_fn(x)
  })
}




