#' Balance test for sample size between treatments
#'
#' @description This function implements chi-squared test for
#'     the null hypothesis that sample size across treatments is equal.
#'
#' @param d a formula object whose right hand side is treatment variable (`~ d`)
#' @param data an object whose class is `data.frame`
#'
#' @return this function returns a matrix including
#'     sample size in each treatment and p-value of chi-squared test.
#'     This matrix has `RCT` and `nbalance` classes.
#'
#' @importFrom stats chisq.test
#'
#' @export
#+
chi2n <- function(d, data) {

  # extract treatment variable as strings
  dvar <- all.vars(d)

  # table of sample size
  sample <- table(data[, dvar])

  # count #.treatment status and extract label
  treat <- names(sample); tn <- length(names(sample))

  # Chi-squared test
  pval <- chisq.test(sample, p = rep(1 / tn, tn))$p.value

  # make matrix
  tab <- matrix(c(sample, pval), nrow = 1)
  colnames(tab) <- c(treat, "p-value")

  class(tab) <- append(c("RCT", "nbalance"), class(tab))
  return(tab)
}
