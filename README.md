# zplyr

As I'm working on more experiments, I've been growing a sort of collection of home-made functions that I use.  This is that collection.  It doesn't really have anything to do with `plyr` or `dplyr` necessarily, the name is just a joke.

For example, in one of my papers, I'm using something called "sliding contrast coding", also known as "forward/backward difference coding". `R` doesn't have a base function that makes this contrast matrix, so I made my own, tweaking the code from `stats::contr.helmert`, `zplyr::contr.slide`.

Or another, less intelligent example was that until recently, I always thought that `dplyr::summarise()` completely ungroups the dataframe, but I recently found out that evidently it just peels back the last grouping element. 
To make sure all of my code was safe, I went back and substituted `summarise` for my own code, `zummarise` which is just a `nse` wrapper for `summarise_` that also ungroups the dataframe aferwards.  Sometimes this is helpful if you're piping into code that doesn't play nice with tibbles.

# Requires

Currently, this package requires at least `dplyr >= 0.5` and `ggplot2 > 2`. I don't require them in the package _itself_ because I had a traumatic experience once with an R package I made a while back, which inadvertantly caused me to update all my packages at once. Plus, I'm lazy, this is for personal use, and I forgot how.
