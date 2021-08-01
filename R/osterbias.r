#' Calculate Unobserved Selection Bias
#'
#' @description We calculate an effect of selection on unobservables
#'     on estiamted coefficient of treatment. This method is presented by
#'     Oster (2019, Journal of Business & Economic Statistics).
#'     Let b0 and r0 be the coefficient of treatment and R-squared
#'     resulting from the regression of outcome on treatment.
#'     Let b1 and r1 be the coefficient of treatment and R-squared
#'     resulting from the regression of outcome on
#'     treatment and observed covariates.
#'     Let rmax be the R-squared resulting from the hypothetical regression
#'     of outcome on treatment, observed covairates, and unobserved covariates.
#'     The value rmax depends on researchers.
#'     Oster' suggestion is `rmax = r1 * 1.3`
#'     (See a paper for detailed discussion).
#'     Define b^ = b1 - (b0 - b1)*(rmax - r1)/(r1 - r0).
#'     Under the following assumpetions,
#'     the value b^ converges to a true b in probability one.
#'     Assumption 1 is that
#'     covariates are orthogonal to unobservables.
#'     Assumption 2 is that
#'     the unobservable and observables are equally related to the treatment.
#'     Assumption 3 is that
#'     the coefficient of covariates on treatment is same as
#'     the coefficient of covairates on outcome.
#'     These assumptions are not precise. See a paper for detailed discussion.
#'
#'
#' @param x an object whose class is `RCT`.
#' @param r_max a numerical value of rmax. Default is null.
#' @param r_multiply a numerical value of multiple of r1. Default is `1.3`.
#'
#' @return a numeric vector whose class is `RCT` and `OsterBias`.
#'
#' @importFrom stringr str_split
#' @importFrom purrr map
#' @importFrom purrr as_vector
#' @importFrom magrittr %>%
#' @importFrom stats coef
#'
#' @export
#+
osterbias <- function(x, r_max = NULL, r_multiply = 1.3) {

  # class check
  if (sum(class(x) == "RCT") == 0) stop("Unsupported class.")

  # extract treatment variables
  d <- str_split(x$treat, "\\+") %>%
    map(~ gsub(" ", "", .)) %>%
    as_vector()

  # extract coefficients
  b0 <- coef(x$fit[[1]])[d]
  r0 <- summary(x$fit[[1]])$r.squared

  b1 <- coef(x$fit[[2]])[d]
  r1 <- summary(x$fit[[2]])$r.squared


  if (!is.null(r_max)) {
    bias <- d %>%
      map(~ (b0[.] - b1[.]) * ((r_max - r1) / (r1 - r0))) %>%
      as_vector()
  } else {
    bias <- d %>%
      map(~ (b0[.] - b1[.]) * ((r_multiply * r1 - r1) / (r1 - r0))) %>%
      as_vector()
  }

  names(bias) <- d
  class(bias) <- append(c("RCT", "OsterBias"), class(bias))

  return(bias)
}
