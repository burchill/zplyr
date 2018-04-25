#' Sliding contrast / backward difference coding
#'
#' Similiar to the base contrast functions (e.g., \code{\link[stats]{contr.sum}}),
#' this coding scheme is known as 'sliding contrast coding' or 'backward difference coding'.
#' This factor coding scheme compares the mean of the dependent variable on one level to the mean
#' of the previous level. This function with return a matrix of contrasts that follow this scheme.
#' Evidently this is similar (if not identical) to the function \code{\link[MASS]{contr.sdif}}.
#'
#' @param n A vector of levels for a factor, or the number of levels.
#' @param contrasts A logical indicating whether contrasts should be computed.
#' @param sparse A logical indicating if the result should be sparse (of class \code{\link[Matrix]{dgCMatrix-class}}), using package \pkg{Matrix}.
#'
#' @examples
#' n<-1000
#' ys <- c(rnorm(n, mean = 0, sd = 50),
#'         rnorm(n, mean = 100, sd = 50),
#'         rnorm(n, mean = 100, sd = 50),
#'         rnorm(n, mean = 5, sd = 50))
#' dists <- c(rep("A",n),
#'            rep("B",n),
#'            rep("C",n),
#'            rep("D",n))
#' df <- data.frame(
#'     y<-ys,
#'     fac<-factor(dists)
#' )
#' # Default coding
#' summary(lm(y~fac,data=df))
#'
#' contrasts(df$fac) <- contr.slide(4)/4
#' # With sliding contrast coding
#' summary(lm(y~fac,data=df))
#'
#' @export
contr.slide <- function (n, contrasts = TRUE, sparse = FALSE)
{
    if (length(n) <= 1L) {
        if (is.numeric(n) && length(n) == 1L && n > 1L)
            levels <- seq_len(n)
        else stop("not enough degrees of freedom to define contrasts")
    }
    else levels <- n
    levels <- as.character(levels)
    if (contrasts==TRUE) {
        n <- length(levels)
        cont <- array(0, c(n, n - 1L), list(levels, NULL))

        cont[col(cont) > row(cont) -1L] <- -unlist(sapply(seq_len(n-1L),
                                                          function(x) {rep(n-x,x)}))
        cont[col(cont) <= row(cont) -1L] <- unlist(sapply(rev(seq_len(n-1L)),
                                                          function(x) {rep(n-x,x)}))
        colnames(cont) <- NULL
        if (sparse)
            stats:::.asSparse(cont)
        else cont
    }
    else stats:::.Diag(levels, sparse = sparse)
}
