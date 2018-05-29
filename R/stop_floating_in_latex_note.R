#' How to stop figures from floating away in LaTeX
#'
#' This isn't a function---it's just a note to document something that I found to be very useful. \cr
#'  \cr
#' When I convert R Markdown files to LaTeX via `knitr`, I end up putting `\\FloatBarrier` after every figure
#' so it doesn't float past where I want it in the document. Using `knitr`'s hooks, I can make this
#' process happen automatically. \cr
#'  \cr
#' In order to do that, you first need to include the LaTeX package `placeins` 
#' (i.e., via `header-includes: \\usepackage{placeins}`). Then, in the first chunk, you 
#' should put the function: \cr
#' ```
#' knitr::knit_hooks$set(plot = function(x, options)  {
#'   if (is.null(options$regfloat) || options$regfloat==FALSE) {
#'     paste0(knitr::hook_plot_tex(x, options), "\n\\FloatBarrier\n")
#'   } else {
#'     knitr::hook_plot_tex(x, options)
#'   }
#' })
#' ```
#' In order to disable this behavior for specific chunks, just put `regFloat=TRUE` as a chunk option.
#'
#'
#' @docType note
#' @name stop_floating_in_latex
NULL
