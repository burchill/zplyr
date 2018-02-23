# zplyr: my personal package

### Note: "zplyr" has nothing to do with "plyr" or "dplyr", the name was just a bad joke

As I'm working on more experiments, I've been growing a sort of collection of home-made functions that I use.  This is that collection.  

For example, in one of my papers, I'm using something called "sliding contrast coding", also known as "forward/backward difference coding". `R` doesn't have a base function that makes this contrast matrix, so I made my own, tweaking the code from `stats::contr.helmert()` to make: `zplyr::contr.slide()`. Evidently that was already a function, `MASS::contr.sdif()`, but I didn't know that ahead of time.

Or another, less intelligent example of code in here comes from that until second year of grad school, I always thought that `dplyr::summarise()` completely ungroups the data frame before returning it. I found out that actually, it just peels back the last grouping element. 
To make sure all of my code was "safe," I went back and substituted `summarise` for my own code, `zummarise` which is just a `nse` wrapper for `summarise_` that also ungroups the dataframe aferwards.  Sometimes this is helpful if you're piping into code that doesn't play nice with grouped tibbles.

There are also a bunch of `ggplot2` "shortcuts" that I've made for graphing things. Pretty much all of this code was written before the amazing `dplyr` revamp, so some of the nonstandard evaluation stuff I wrote seems pretty hacky now in comparison.

# Requires

Currently, this package requires at least `dplyr >= 0.5` and `ggplot2 > 2`. I don't require them in the package _itself_ because I had a traumatic experience once with an R package I made a while back, which inadvertantly forced an update to all my packages at once. Plus, I'm lazy, this is for personal use, and I forgot how.
