#' Extended geom_segment
#'
#' Still a WIP. The dropping and warning of segments switching directions isn't really working correctly yet.  `geom_segment_plus` is an updated version of mo-seph's Stack Overflow code (https://stackoverflow.com/q/14647988/4560765), which no longer works. mo-seph's code in turn is an extension of `ggplot2`'s `geom_segment`, but with the ability to offset segments that overlap, and to shorten the end or beginning of the segment.
#'
#' This code basically has three new possible parameters:
#' - **`shorten.start`/`shorten.end`**: this moves the start/end of the segment closer to the midpoint by a proportion
#' - **`shorten.start.abs`/`shorten.end.abs`**: this moves the start/end of the segment closer to the midpoint by an absolute value
#' - **`offset`**: if two segments fully overlap, this offsets them slightly
#'
#' @param mapping,data,\dots,stat,parse,inherit.aes See the documentation for [ggplot2::geom_segment()]
#' @examples
#' d <- data.frame(x=c(-1,1),
#'            y=c(-1,1),
#'            x.to=c(2,-1),
#'            y.to=c(-1,1))
#' d %>%
#'   ggplot(aes( x=x, y=y ) ) +
#'   geom_point() +
#'   geom_segment_plus(aes( x=x, y=y, xend=x.to, yend=y.to ),
#'                     lineend="round", arrow=arrow(length=unit(0.15, "inches")),
#'                     offset=0.00, shorten.start=0.5, shorten.end=0,
#'                     shorten.end.abs=0.15,
#'                     shorten.limit = 'drop')
#' d %>%
#'   ggplot(aes( x=x, y=y ) ) +
#'   geom_point() +
#'   geom_segment_plus(aes( x=x, y=y, xend=x.to, yend=y.to ),
#'                     lineend="round", arrow=arrow(length=unit(0.15, "inches")),
#'                     offset=0.00, shorten.start=1, shorten.end=0,
#'                     shorten.end.abs=1.25,
#'                     shorten.limit = 'drop')
#' @export
geom_segment_plus <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", ...,
                              arrow = NULL, arrow.fill = NULL, lineend = "butt",
                              linejoin = "round",
                              shorten.limit = c("warn","drop","ignore"),
                              na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  shorten.limit = match.arg(shorten.limit)
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSegmentPlus,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(arrow = arrow, arrow.fill = arrow.fill,
                      lineend = lineend, linejoin = linejoin,
                      shorten.limit = shorten.limit,
                      na.rm = na.rm,
                      ...))
}


approx_equal <- function(x,y,tolerance= .Machine$double.eps ^ 0.5) {
  purrr::map2_lgl(x,y,function(xx,yy) {
    abs(xx-yy) < tolerance
  })
}


GeomSegmentPlus <- ggplot2::ggproto(
  "GeomSegmentPlus",
  ggplot2::GeomSegment,

  default_aes = aes(colour="black", size=0.5, linetype=1, alpha = NA,
                    shorten.start=0, shorten.end=0, offset=0,
                    shorten.start.abs=0, shorten.end.abs=0),
  # draw_key    = ggplot2::draw_key_blank,
  # required_aes = GeomText$required_aes,
  required_aes = c("x", "y", "xend", "yend"),

  draw_panel = function (data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                         lineend = "butt", linejoin = "round",
                         shorten.limit = "warn",
                         na.rm = FALSE)
  {
    data <- remove_missing(
      data, na.rm = na.rm,
      c("x", "y", "xend", "yend", "linetype", "size", "shape",
        "shorten.start", "shorten.end", "offset",
        "shorten.start.abs", "shorten.end.abs"),
      name = "geom_segment_plus")
    if (ggplot2:::empty(data))
      return(zeroGrob())
    if (coord$is_linear()) {
      if (nrow(dplyr::filter(data, shorten.end.abs < 0)) > 0)
        stop("shorten.end.abs must be positive")
      if (nrow(dplyr::filter(data, shorten.start.abs < 0)) > 0)
        stop("shorten.start.abs must be positive")
      if (nrow(dplyr::filter(data, shorten.end != 0 & shorten.end.abs != 0)) > 0)
        stop("shorten.end and shorten.end.abs cannot both be non-zero")
      if (nrow(dplyr::filter(data, shorten.start != 0 & shorten.start.abs != 0)) > 0)
        stop("shorten.start and shorten.start.abs cannot both be non-zero")

      arrow.fill <- arrow.fill %||% data$colour

      for (i in 1:dim(data)[1]) {
        match = data$xend == data$x[i] &
          data$x == data$xend[i] &
          data$yend == data$y[i] &
          data$y == data$yend[i]
        if( sum( match ) == 0 ) data$offset[i] <- 0
      }

      data$dx = data$xend - data$x
      data$dy = data$yend - data$y
      data$dist = sqrt( data$dx^2 + data$dy^2 )
      data$px = data$dx/data$dist
      data$py = data$dy/data$dist

      data$angle = atan2(data$dy,data$dx)
      data$absdxstart = cos(data$angle) * data$shorten.start.abs
      data$absdystart = sin(data$angle) * data$shorten.start.abs
      data$absdxend = cos(data$angle) * data$shorten.end.abs
      data$absdyend = sin(data$angle) * data$shorten.end.abs

      data$x = data$x + data$px * data$shorten.start + data$absdxstart
      data$y = data$y + data$py * data$shorten.start + data$absdystart
      data$xend = data$xend - data$px * data$shorten.end - data$absdxend
      data$yend = data$yend - data$py * data$shorten.end - data$absdyend
      data$x = data$x - data$py * data$offset
      data$xend = data$xend - data$py * data$offset
      data$y = data$y + data$px * data$offset
      data$yend = data$yend + data$px * data$offset

      data$new_angle = atan2(data$yend - data$y, data$xend - data$x)
      oopsies <- nrow(data[!approx_equal(data$new_angle, data$angle),])
      if (oopsies>0) {
        if (shorten.limit=="warn")
          warning(oopsies, " segments were shortened too much and now have different directions")
        if (shorten.limit=="drop") {
          message(oopsies, " segments were dropped due to shortening")
          data <- data[approx_equal(data$new_angle, data$angle),]
        }
      }

      data <- coord$transform(data, panel_params)

      return(with(data,
                  segmentsGrob(x, y, xend, yend, default.units="native",
                               gp = gpar(col=alpha(colour, alpha),
                                         fill = alpha(colour, alpha),
                                         lwd = size * ggplot2::.pt,
                                         lty = linetype,
                                         lineend = lineend,
                                         linejoin = linejoin),
                               arrow = arrow)
      ))

    }
    data$group <- 1:nrow(data)
    starts <- subset(data, select = c(-xend, -yend))
    ends <- rename(subset(data, select = c(-x, -y)), c(xend = "x",
                                                       yend = "y"))
    pieces <- rbind(starts, ends)
    pieces <- pieces[order(pieces$group), ]
    GeomPath$draw_panel(pieces, panel_params, coord, arrow = arrow,
                        lineend = lineend)
  }
)
