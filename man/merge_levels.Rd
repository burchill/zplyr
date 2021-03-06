% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge_levels.R
\name{merge_levels}
\alias{merge_levels}
\title{Combine specified levels of a factor}
\usage{
merge_levels(.data, ...)
}
\arguments{
\item{.data}{The factor you want to re-specify.}

\item{\dots}{Values that must be named, where the name corresponds to the new level and the value corresponds to the old level. The values can be strings, numbers, of vectors of those.}
}
\value{
a factor with levels and values as you specified, with those unspecified not changing.
}
\description{
Similar to \code{merge_factor()} but generally reserved for situations
when you don't need to worry about specifying the entire factor.
Instead of taking a list of arguments as input, it takes named values.
The old levels that aren't altered stay put order-wise, and the new ones are swapped in to where their old levels were, as much as possible.
}
\examples{
my_factor <- factor(c("d", "b", "c", "d", "a", "a"))
levels(my_factor)
merge_levels(my_factor, "AandB"=c("a","b"), "RenamedC"="c")

}
\seealso{
\code{\link{merge_factor}}
}
