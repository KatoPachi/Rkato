#' Balance test of Covariates across treatments
#'
#' @description This function implements F-test for the null hypothesis that
#'     observed variables are balanced across treatments.
#'     First, this function runs a linear regression of covariate on treatments,
#'     using `lm()`.
#'     After that, we obtaine F-statistics and its p-value, using `anova()`.
#'
#' @param x a formula object whose right hand side is covariates (`~ x1 + x2`).
#' @param d a formula object whose right hand side is treatment variable (`~ d`)
#' @param data an object whose class is `data.frame`
#'
#' @return A matrix including mean of covariate in each treatment and
#'     p-value of F-statistics. This matrix has `RCT` and `xbalance` classes.
#'
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#'
#' @export
#+
fbalance <- function(x, d, data) {

  # make formula
  xvar <- all.vars(x)
  model <- xvar %>%
    map(~ paste0(., "~", all.vars(d))) %>%
    map(~ as.formula(.))

  # estimate OLS and extract p-value of F-value via anova()
  ols1 <- model %>% map(~ lm(., data = data))
  pval <- ols1 %>% map(~ anova(.)[["Pr(>F)"]][1])

  # extract average value of each treatment via lm()
  ols2 <- model %>% map(~ lm(update(., . ~ -1 + .), data = data))
  mu <- ols2 %>% map(~ coef(.))

  # make matric including results
  tab <- seq_len(length(model)) %>%
    map(~ matrix(c(mu[[.]], pval[[.]]), nrow = 1)) %>%
    reduce(rbind)

  colnames(tab) <- c(names(mu[[1]]), "p-value (F test)")
  rownames(tab) <- xvar

  # return
  class(tab) <- append(c("RCT", "xbalance"), class(tab))
  return(tab)

}
