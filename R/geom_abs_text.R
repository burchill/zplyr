#' Text positioned relative to a ggplot panel/facet
#'
#' `geom_abs_text` functions similarly to ggplot2's `geom_text`, except that instead of taking `x` and `y` aesthetics, it requires `xpos` and `ypos` aesthetics, values from 0-1 that determine the absolute `x` and `y` coordinates _with respect to whatever viewport they are being plotted in_, be that the full panel or a facet panel. A value of 0.5 indicates the center of the axis, and a value of 1 indicates the far edge of the axis.
#' \cr \cr This code and its documentation is based off code in the ggplot2 package, and thus is subject to the copy-left licenses of the original package.
#'
#' This function has only been tested in a few scenarios, and only in `ggplot2 v2.2.1`.
#'
#' @param mapping,data,\dots,stat,parse,inherit.aes See the documentation for [ggplot2::geom_text()]
#' @examples
#' df <- data.frame(
#'   x = c(99, 0, 1),
#'   y = c(-100, 0, 100),
#'   label = c("A","B", "C")
#' )
#' ggplot(df, aes(x=x,y=y,label=label, color=label)) +
#'   geom_point() +
#'   geom_text() +
#'   facet_wrap(~label, scales="free") +
#'   geom_abs_text(aes(xpos=0.5, ypos=0.75, label = paste0("relative: ", label)))
#'
#' @export
geom_abs_text <- function (mapping = NULL, data = NULL, stat = "identity",
                           ..., parse = FALSE, inherit.aes = TRUE) {
  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomAbsText,
                 position = ggplot2::PositionIdentity, inherit.aes = inherit.aes,
                 params = list(parse = parse, ...))
}

#' @param xpos A numeric between 0 and 1, indicating the x position
#' @param ypos A numeric between 0 and 1, indicating the y position 
#' @param label A character vector to be displayed on the plot
#' @rdname stat_errorbar
#' @export
annotate_abs_text <- function(xpos, ypos, label, ...) {
  data_l <- list(xpos=xpos, ypos=ypos, label=label)
  aesthetics <- c(data_l, list(...))
  lengths <- vapply(aesthetics, length, integer(1))
  unequal <- length(unique(setdiff(lengths, 1L))) > 1L
  if (unequal) {
    bad <- lengths != 1L
    details <- paste(names(aesthetics)[bad], " (", lengths[bad], 
                     ")", sep = "", collapse = ", ")
    stop("Unequal parameter lengths: ", details, call. = FALSE)
  }
  data <- data.frame(data_l)
  ggplot2::layer(
    geom = GeomAbsText, params = list(...), 
    stat = ggplot2::StatIdentity, 
    position = ggplot2::PositionIdentity, 
    data = data, mapping = aes_all(names(data)), 
    inherit.aes = FALSE, show.legend = FALSE)
}



#' Displaying skew/kurtosis text in plots
#'
#' `stat_moments()` summarises the data supplied to the x-axis, and draws text that displays the skewness and/or kurtosis of the data, with a variety of options. This is almost chiefly meant to be used in conjunction with a density plot, such as [ggplot2::geom_density()] or [ggplot2::stat_density()].  Since this object is returning text, it needs to be given coordinates on where to be placed. It requires the aesthetics `xpos` and `ypos` (see [geom_abs_text()]), which are coordinates (from 0-1) relative to the panel/facet panel the text is to be displayed in.
#'
#' @param mapping,data,\dots,inherit.aes,parse See [ggplot2::geom_text()] for details.
#' @param moment A string determining which moment to display. Can be one of three values: `"skewness"`, `"kurtosis"`, or `"both"`, which displays both moments.
#' @param sig A logical; if true, will test the skewness for significance using [moments::agostino.test()], i.e., the D'Agostino test of skewness. Significance will be indicated via asterisks.
#' @param excess_kurtosis A logical; if `TRUE`, kurtosis will be expressed as "excess" kurtosis (i.e., kurtosis - 3, as 3 is the kurtosis of a normal distribution). If kurtosis is not displayed, this will be ignored.
#' @param digits The number of digits after the decimal place to display for the moment values.
#' @param alternative A string specifying the alternative hypothesis for the D'Agostino test. Must be one of `"less"` (default) `"two.sided"` or `"greater"`. You can specify just the initial letter. If `sig` = `FALSE`, this will be ignored.
#' @examples
#' make_log_normal <- function(n, mu, sd, name) {
#'   log_mu <- log(mu)
#'   df <- data.frame(x=exp(rnorm(n, log_mu, sd=sd)))
#'   df$Name <- name
#'   df
#' }
#'
#' new_df <- rbind(make_log_normal(1000, 100, 1, "Distr1"),
#'                 make_log_normal(1000, 500, 0.3, "Distr2"),
#'                 make_log_normal(1000, 900, 0.5, "Distr3"))
#'
#' ggplot(new_df, aes(x=x, color=Name)) +
#'   geom_density() +
#'   facet_wrap(~Name, scales="free") +
#'   stat_moments(aes(xpos=0.5, ypos=0.75),
#'                sig = TRUE,
#'                moment = "both",
#'                fontface="bold") +
#'   theme_bw()
#'
#' @export
stat_moments <- function (mapping = NULL, data = NULL,
                          ...,
                          moment = c("skewness","kurtosis","both"),
                          sig = FALSE,
                          excess_kurtosis = FALSE,
                          digits = 1,
                          alternative = c("less", "greater", "two.sided"),
                          inherit.aes = TRUE,
                          parse = FALSE) {
  moment <- match.arg(moment)
  alternative <- match.arg(alternative)
  ggplot2::layer(
    data = data, mapping = mapping, stat = MomentLabel,
    geom = GeomAbsText,
    position = ggplot2::PositionIdentity,
    inherit.aes = inherit.aes,
    params = list(parse = parse, moment = moment, sig = sig,
                  excess_kurtosis = excess_kurtosis,
                  alternative = alternative, digits = digits,
                  ...))
}



#' @format NULL
#' @usage NULL
#' @export
GeomAbsText <- ggplot2::ggproto(
  "GeomAbsText",
  ggplot2::GeomCustomAnn,

  default_aes = ggplot2::GeomText$default_aes,
  draw_key = ggplot2::draw_key_blank,
  # required_aes = GeomText$required_aes,
  required_aes = c("xpos", "ypos", "label"),

  draw_panel = function (data, panel_scales, coord, parse = FALSE,
                         xpos, ypos) {
    if (!inherits(coord, "CoordCartesian")) {
      stop("annotation_custom only works with Cartesian coordinates",
           call. = FALSE)
    }

    lab <- data$label
    if (parse) {
      lab <- parse(text = as.character(lab))
    }

    grob <- grid::textGrob(
      lab, x = grid::unit(data$xpos, "npc"),
      y = grid::unit(data$ypos, "npc"),
      default.units = "npc", hjust = data$hjust,
      vjust = data$vjust, rot = data$angle,
      gp = grid::gpar(col = scales::alpha(
        data$colour,
        data$alpha),
        fontsize = data$size * .pt,
        fontfamily = data$family,
        fontface = data$fontface,
        lineheight = data$lineheight),
      check.overlap = FALSE)

    corners <- data.frame(x = c(-Inf, Inf), y = c(-Inf, Inf))
    data <- coord$transform(corners, panel_scales)
    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)
    vp <- grid::viewport(x = mean(x_rng), y = mean(y_rng), width = diff(x_rng),
                         height = diff(y_rng), just = c("center", "center"))
    grid::editGrob(grob, vp = vp, name = grob$name)
  }
)






#' @format NULL
#' @usage NULL
#' @export
MomentLabel <- ggplot2::ggproto(
  "MomentLabel",
  ggplot2::Stat,
  compute_group = function(data, scales,
                           moment = c("skewness","kurtosis","both"),
                           sig = FALSE,
                           digits = 1,
                           excess_kurtosis = FALSE,
                           alternative = "less") {
    moment <- match.arg(moment)
    get_moment_text <- function(df) {

      label_text <- ""

      if (moment %in% c("skewness","both")) {
        skewness <- round(moments::skewness(df$x, na.rm = TRUE),
                          digits = digits)

        if (sig==TRUE) {
          if (requireNamespace("moments", quietly = TRUE) == TRUE) {
            xx <- df$x
            if (length(xx) > 46340)
              xx <- sample(xx, size=46340, replace=FALSE)

            p.value <- moments::agostino.test(xx, alternative = alternative)$p.value
            tmp <- Map(function(x) x > p.value, c(0.05, 0.01, 0.001))
            asterisks <- switch(length(tmp[which(tmp == TRUE)]),
                                "0" = " n.s.", "1" = "*", "2" = "**", "3" = "***", "***")
            skewness <- paste0(skewness, asterisks)
          } else {
            warning("The `moments` package is need to test skewness for significance")
          }
        }
        label_text <- paste0("skewness: ", skewness)
      }
      if (moment=="both")
        label_text <- paste0(label_text, "\n")

      if (moment %in% c("kurtosis","both")) {
        kurt <- round(moments::kurtosis(df$x, na.rm = TRUE), digits=digits)
          if (excess_kurtosis)
            kurt <- kurt - 3.0
        label_text <- paste0(label_text, "kurtosis: ", kurt)
      }
      data.frame(label=label_text)
    }

    old_uniq_cols <- function (df) {
      df <- df[1, sapply(df, function(x) length(unique(x)) == 1),
               drop = FALSE]
      rownames(df) <- 1:nrow(df)
      df
    }

    summary <- plyr::ddply(data, "group", get_moment_text)
    unique <- plyr::ddply(data, c("group"), old_uniq_cols)
    unique$x <- NULL
    merge(summary, unique, by = c("group"), sort = FALSE)
  },

  required_aes = c("x", "xpos", "ypos")
)




