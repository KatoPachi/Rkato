#'
#' @importFrom stats na.omit
#' @importFrom stats var
#+
se <- function(x, na.rm = FALSE) {

  # na.rm = TRUE, we remove NA using na.omit()
  if (na.rm) x <- na.omit(x)

  # calculate and return se
  sqrt(var(x) / length(x))
}

#' t-test across multiple treatment status
#'
#' @description This function picks baseline group
#'     from multiple treatment groups,
#'     and implements t-test between the baseline group and
#'     each treatment group.
#'
#' @param y a formula object whose left hand side is outcome (`y ~ .`).
#' @param d a formula object whose right hand side is treatment variable (`~ d`)
#' @param base a string or numeric value of the baseline group.
#' @param data an object whose class is `data.frame`
#'
#' @return A matrix including mean and its standard error of outcome
#'     in each treatment and p-value of t-test.
#'     This object has `RCT` and `ttest` class.
#'
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom magrittr %>%
#'
#' @export
#+
ttest_rct <- function(y, d, base, data) {

  # extract treatment and outcome variables
  dvar <- all.vars(d)
  yvar <- all.vars(y)[1]

  # extract treatment status
  assign <- unique(data[, dvar]) %>% unlist(use.names = FALSE)

  # extract outcome vector for baseline treatment status
  basevec <- subset(data, get(dvar) == base)[yvar]

  # implement t-test
  test <- assign %>%
    map(~ subset(data, get(dvar) == .)[yvar]) %>%
    map(~ list(
      t = t.test(x = ., y = basevec),
      se = se(unlist(.), na.rm = TRUE)
    )) %>%
    map(~ matrix(c(.$t$estimate[1], .$se, .$t$p.value), nrow = 1)) %>%
    reduce(rbind)

  colnames(test) <- c("estimate", "se", "pvalue")
  rownames(test) <- assign

  class(test) <- append(c("RCT", "ttest"), class(test))
  return(test)

}
