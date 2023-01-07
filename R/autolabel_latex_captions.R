#' Autolabel figures in Latex
#'
#' I hate having to add `\\label{fig:blah}` to `fig.cap`
#' in Latex .Rmd files.  There's probably a better way to do
#' this, but if you call this function, it will automatically
#' add `\\label{fig:<label>}` to the `fig.cap` option of any
#' chunk with a `fig.cap` and a label, substituting whitespace
#' stretches (after trimming it)
#'
#' @export
autolabel_latex_figs <- function() {
  knitr::opts_hooks$set(
    .autolabel =
      function(options) {
        if (!is.null(options$label) && isTRUE(options$.autolabel)) {
          label = trimws(gsub("\\s+", "_", options$label))
          options$fig.cap = paste0(
            rlang::eval_tidy(options$fig.cap),
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
#' @export
beep_on_knit <- function(beep_sound=3, sleep=4) {
  library(beepr)
  prev_fn <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    # Prevents beeping when child document is closed
    if (knitr::opts_knit$get("child") == FALSE) {
      beepr::beep(beep_sound)
      Sys.sleep(sleep)
    }
    prev_fn(x)
  })
}

#' Beep on knit error
#'
#' This function requires the `beepr` package. It will first set the global chunk function to `error=TRUE` (so knitr won't stop on errors), but then adds a hook that will catch these errors, play a sound, and then throw an error anyway, shutting it down. \cr \cr
#' Will not beep for non-code chunk errors.
#'
#' @param beep_sound Input to `beepr::beep()` for what sound to play
#' @param sleep The amount of seconds to sleep before moving on (so that the sound isn't cut off after it knits)
#' @export
beep_on_knit_error <- function(beep_sound=9, sleep=2) {
  library(beepr)
  knitr::opts_chunk$set(error=TRUE)
  prev_err <- knitr::knit_hooks$get("error")
  knitr::knit_hooks$set(error = function(x, options) {
    beepr::beep(beep_sound)
    Sys.sleep(sleep)
    stop(x, call. = FALSE)
    # why not?
    prev_err(x, options)
  })
}

#' Stop figures from floating in LaTeX
#'
#' When I knit R Markdown files to LaTeX, I end up putting `\\FloatBarrier` after every figure
#' so they don't float past where I want them in the document. Using `knitr`'s hooks, I can make this
#' process happen automatically. \cr
#'  \cr
#' In order to do that, you first need to include the LaTeX package `placeins`
#' (i.e., via `header-includes: \\usepackage{placeins}`).
#' @export
stop_floating <- function() {
  prev_plot_hook <- knitr::knit_hooks$get("plot")
  knitr::knit_hooks$set(plot = function (x, options) {
    if (is.null(options$regfloat) || isFALSE(options$regfloat))
      paste0(prev_plot_hook(x, options), "\n\n\\FloatBarrier\n")
    else
      prev_plot_hook(x, options)
  })
}







#' Set knitr hooks for htmlwidgets with Jekyll/GitHub Pages
#'
#' Modified from my gist: https://gist.github.com/burchill/8392b2a753652e24a35a8a1dd707c1b1.
#'  This functions sets a `knitr` hook so that any HTML widgets that were
#'  printed (i.e., objects that inherit the `'htmlwidget'`, like those from
#'  the `htmlwidgets` or `plotly` packages) will work with a Jekyll system,
#'  like the one used for GitHub Pages.
#'
#' It essentially sets a hook so that, when the document is finished being
#'  knitted, it moves all the dependencies necessary for the widgets to a
#'  directory, and then adds HTML code to the document to load those files
#'  from their new location. Additionally, it sets the default
#'  `screenshot.force` chunk option to `FALSE`, so `knitr` doesn't try to
#'  use a screenshot instead of the widget.
#'
#' See https://www.zachburchill.ml/plotly_with_jekyll/ for background
#'  and (https://gist.github.com/burchill/9df0f6245ea7768e5b6bbd0a1c22db08)
#'  for the old, bad version of this script.
#'
#' @param dep_dir The directory you want to save the dependencies to.
#' @param base_path The directory you want to make the dependency links relative to.
#'  For example, if your post is at the url `yoursite.com/this_post/`, then you'll
#'  want to make `base_path` the home directory of your project.
#' @param hrefFilter This function lets you perform any additional manipulations
#'  to the dependency links. For some reason, when I give my base directory as my
#'  project's home directory, the links come back without the necessary "/" in front
#'  of them. This function adds that to each link.
#' @export
set_widget_hooks <- function(dep_dir    = knitr::opts_chunk$get("plotly.savepath"),
                             base_path  = knitr::opts_chunk$get("proj.basedir"),
                             hrefFilter = function(x) paste0("/", x)) {
  # Move the dependencies into the specified folder,
  #  makes them relative to the base directory,
  #  and outputs the HTML that loads them.
  render_deps <- function() {
    l <- knitr::knit_meta(class = "html_dependency",
                          clean = FALSE)
    if (length(l) > 0)
      dir.create(dep_dir, showWarnings = FALSE, recursive = TRUE)
    l <- lapply(unique(l), function(dep) {
      dep <- htmltools::copyDependencyToDir(dep, dep_dir, FALSE)
      dep <- htmltools::makeDependencyRelative(dep, base_path, FALSE)
      dep } )
    l <- htmltools::renderDependencies(l,  hrefFilter=hrefFilter)
    htmltools::htmlPreserve(l)
  }

  # Adds the dependency-loading HTML at the end of the doc,
  #  without upsetting the previous doc-hook.
  prev_doc_hook <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    prev_doc_hook(append(x, render_deps()))
  })

  # Sets the default of all chunks to not force
  #  screenshots. You can change it to `TRUE`
  #  on the chunks you want it to screenshot.
  knitr::opts_chunk$set(screenshot.force=FALSE)
}



#' Setting knitr options for my blog
#'
#' This is my lame attempting at standardizing default knitr options for my blog, with some versioning for consistency / replicability. `get_blog_knitr_opts()` is the getter version of this function.
#'
#' @export
set_blog_knitr_opts <- function(version=NULL) {
  if (is.null(version))
    version <- names(version_list)[[1]]
  v_exp <- version_list[[version]]

  # Variables to the version list
  desired_dpi = 200
  base_retina_val = desired_dpi/knitr::opts_chunk$get("dpi")
  base_fig_height = 6
  base_fig_width = 8
  base_max_width = 'style="max-width:80%;"'

  rlang::eval_tidy(v_exp)

  prev_doc_hook <- knitr::knit_hooks$get("document")
  knitr::knit_hooks$set(document = function(x) {
    s = paste0("<!--html_preserve-->",
              "<!--zoption version:",
              version, "-->",
              "<!--/html_preserve-->")
    prev_doc_hook(append(x, s))
  })


}

#' @export
get_blog_knitr_opts <- function(version=NULL) {
  if (is.null(version))
    version_list[[1]]
  else
    version_list[[version]]
}



version_list <- list(
  "0.01" = expr({
    # Sets default sizes
    knitr::opts_chunk$set(
      fig.width  = base_fig_width,
      fig.height = base_fig_height,
      out.extra  = base_max_width,
      fig.retina = base_retina_val,
      autocaption= TRUE
    )
    # Plotly hook
    knitr::opts_hooks$set(plotlier = function(options) {
      if (isTRUE(options$plotlier)) {
        options$out.width   <- "100%"
        options$out.height  <- "400px"
        options$autocaption <- FALSE
        options$out.width.px <- NULL
        options$out.height.px <- NULL
        if (is.null(options$fig.cap))
          options$fig.cap <- ""
        options
      }
    })
  })
)
