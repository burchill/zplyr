% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggplot_functions.R
\name{share_scales}
\alias{share_scales}
\title{Share arguments amongst multiple ggplot functions}
\usage{
share_scales(...)
}
\arguments{
\item{\dots}{Unnamed arguments should be \code{ggplot2} functions
(e.g., \code{scale_color_discrete}, \code{scale_fill_discrete}, etc.) and
named arguments should be whatever arguments you want passed to these functions}
}
\value{
A list of the \code{ggproto} objects
}
\description{
Often it is the case that one wishes to pass identical arguments to
multiple functions in \code{ggplot}.  For example, if one wants to scale
both \code{color} and \code{fill} with the same breaks and legend names/labels. \cr
The function \code{share_scales} lets you send identical arguments to a list of \code{ggplot} functions you
specify.
}
\examples{
df <- data.frame(
   x<-rnorm(100),
   f<-c(rep("Label1",50), rep("Label2",50))
)

l <- c(1,2,3,4,5,6)
df <- data.frame(x = rgamma(100, 2, 2),
                 y = factor(sample(l, size=100, replace = TRUE)))

  ggplot(df, aes(x=x, y=y, size=y, color=y, fill=y, shape=y)) +
  geom_point() +
  # Zplyr function
  share_scales(scale_color_discrete,  scale_size_discrete,
               scale_fill_discrete, scale_shape_discrete,
               name="Legend Label!",
               breaks=c(1,2,3,4,5,6),
               labels=c("uno","dos","tres","catorce","FIVE","siiiiiix"))
}
