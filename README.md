# zplyr

As I'm working on more experiments, I've been growing a sort of collection of home-made functions that I use.  This is that collection.  It doesn't really have anything to do with `plyr` or `dplyr` necessarily, the name is just a joke.

For example, until recently, I always thought that `dplyr::summarise()` would completely ungroup the dataframe, but evidently it just peels back the last grouping element. 
To make sure all of my code was safe, I went back and substituted `summarise` for my own code, `zummarise` which is just a nse wrapper for `summarise_` that also ungroups the dataframe aferwards.

# Requires

Currently, this package requires at least `dplyr >= 0.5` and `ggplot2 > 2`. I don't require them in the package itself because I had a traumatic experience once with an R package I made once that inadvertantly caused me to update all my packages at once. Plus, I'm lazy, this is for personal use, and I forgot how.
