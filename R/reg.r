#' Unified regression analysis
#'
#' @description This function implements various regression analysis,
#'     especially econometrics.
#'     This function has some advantages.
#'     (1) Unifying augments.
#'     By just changing method augment, you can use other regression analyses.
#'     (2) Specifying each components of formula.
#'     You can easily incorporate this function in a loop system.
#'
#' @param method a srting of method of the generic function `reg`
#' @param ... augments which pass on each method.
#'
#' @details There are four methods.
#'
#' ### `fe` method
#' Using the package lfe or fixest, you implement the fixed effect, and
#' the panel IV model. In addition to original class,
#' a returned object also has a class `fe_felm` (lfe pkg)
#' or `fe_fixest` (fixest pkg). Thus, you can use `summary()`.
#' You can pass following augments on this method:
#' \itemize{
#'   \item `y`: a formula whose rhs is outcome (`y ~ .`)
#'   \item `x`: a formula whose lhs is covariates (`~ x1 + x2`)
#'   \item `z`: a formula whose rhs is endogenous variable
#'     and lhs is exogenous variable (`x3 ~ z`)
#'   \item `fixef`: a formula whose lhs is fixed effects (`~ fix1 + fix2`)
#'   \item `cluster`: a formula whose lhs is cluster variable (`~ clust1`)
#'   \item `data`: a object whose class is `data.frame`.
#'   \item `pkg`: a string of package. Default is `"fixest"`
#'   \item `se`: method of caclulation of se (only `fixest` pkg).
#'     the augment `se` passes a character scalar:
#'     "standard", "hetero", "cluster", "twoway", "threeway" or "fourway."
#'     By default if there are clusters in the estimation: se = "cluster";
#'     otherwise se = "standard".
#' }
#'
#' ### `lpm` method
#' Using the `lm()` in the stats package,
#' you implement the linear proability model.
#' To claculate robust standard errors,
#' this method uses `coeftest` in the lmtest pkg,
#' and `vcovHC` in the sandwich pkg.
#' If treatment variable is specified,
#' you can estimate this model without covariates and with covariates.
#' A returned object is a list which includes `lm` result called `fit`,
#' robust se test called `test`,
#' and treatment variable formula called `treat` (if specified).
#' The returned object has `lpm` class in addition to original classes.
#' If treatment variable is specified, we add `RCT` class.
#' You can pass following augments on this method.
#' \itemize{
#'   \item `y`: a formula whose rhs is outcome (`y ~ .`)
#'   \item `x`: a formula whose lhs is covariates (`~ x1 + x2`)
#'   \item `d`: a formula whose lhs is treatment variable (`~ d`).
#'   \item `data`: an object whose class is `data.frame`
#' }
#'
#' ### `ols` method
#' Using the `lm` in the stats package,
#' you implement the linear regression model.
#' If treatment variable is specified,
#' you can estimate this model without covariates and with covariates.
#' A returned object is a list which includes `lm` result called `fit`,
#' and treatment variable formula called `treat` (if specified).
#' The returned object has `ols` class in addition to original classes.
#' If treatment variable is specified, we add `RCT` class.
#' You can pass following augments on this method.
#' \itemize{
#'   \item `y`: a formula whose rhs is outcome (`y ~ .`)
#'   \item `x`: a formula whose lhs is covariates (`~ x1 + x2`)
#'   \item `d`: a formula whose lhs is treatment variable (`~ d`).
#'   \item `data`: an object whose class is `data.frame`
#' }
#'
#' ### `binomial` method
#' Using the `glm` in the stats package,
#' you implement the logit or probit model.
#' A returned object has `binomial` class in addition to original classes.
#' You can pass following augments on this method.
#' \itemize{
#'   \item `y`: a formula whose rhs is outcome (`y ~ .`)
#'   \item `x`: a formula whose lhs is covariates (`~ x1 + x2`)
#'   \item `link`: character string: `"logit"` or `"probit"`.
#'   \item `data`: an object whose class is `data.frame`
#' }
#'
#' @importFrom fixest feols
#' @importFrom lfe felm
#' @importFrom stats lm
#' @importFrom lmtest coeftest
#' @importFrom sandwich vcovHC
#' @importFrom purrr map
#' @importFrom stats glm
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom magrittr %>%
#'
#' @export
#+
reg <- function(method, ...) {

    method <- structure(method, class = method)

    fitreg <- function(...) {
        UseMethod("reg", method)
    }

    fitreg(...)

}

reg.fe <- function(
    y, x, z = 0, fixef = 0, cluster = 0, se = NULL, pkg = "fixest", data
) {

    # make regression model
    yvar <- all.vars(y)[1]
    xeq <- as.character(x)[-1]
    model <- paste(yvar, "~", xeq)

    if (pkg == "fixest") {

       if (fixef != 0) {
           fixeq <- as.character(fixef)[-1]
           model <- paste(model, "|", fixeq)
       }

       if (z != 0) {
           zeq <- as.character(z)
           zeq <- paste(zeq[2], "~", zeq[3])
           model <- paste(model, "|", zeq)
       }

       model <- as.formula(model)

    } else if (pkg == "lfe") {

        if (fixef != 0) {
            fixef <- as.character(fixef)[-1]
        }

        if (z != 0) {
            z <- as.character(z)
            z <- paste("(", zeq[2], "~", zeq[3], ")")
        }

        if (cluster != 0) {
            cluster <- as.character(cluster)[-1]
        }

        model <- paste(model, "|", fixef, "|", z, "|", cluster)
        model <- as.formula(model)

    } else {
        stop("Unsupported pkg specified.")
    }

    # estimation
    if (pkg == "fixest") {

       if (cluster == 0) {

           if (is.null(se)) se <- "standard"
           est_model <- feols(model, data = data, se = se)

       } else {

           if (is.null(se)) se <- "cluster"
           est_model <- feols(
               model, data = data, cluster = cluster, se = se
           )

       }

       class(est_model) <- append("fe_fixest", class(est_model))

    } else if (pkg == "lfe") {

       est_model <- felm(model, data = data)

       class(est_model) <- append("fe_felm", class(est_model))

    }

    return(est_model)

}

reg.lpm <- function(y, d = NULL, x, data) {

    # make regression model
    yvar <- all.vars(y)[1]
    xeq <- as.character(x)[-1]

    if (is.null(d)) {

        model <- as.formula(paste(yvar, "~", xeq))

        # estimaton
        ols <- lm(model, data = data)
        rob <- lmtest::coeftest(ols, vcov = sandwich::vcovHC(ols, type = "HC0"))

        # return
        res <- list(fit = ols, test = rob)
        class(res) <- "lpm"

        return(res)


    } else {

        deq <- as.character(d)[-1]
        model1 <- as.formula(paste(yvar, "~", deq))
        model2 <- as.formula(paste(yvar, "~", deq, "+", xeq))
        model <- list(model1, model2)

        # estimation
        ols <- model %>% purrr::map(~ lm(., data = data))
        rob <- ols %>% purrr::map(
            ~ lmtest::coeftest(., vcov = sandwich::vcovHC(., type = "HC0"))
        )

        # return
        res <- list(fit = ols, test = rob, treat = deq)
        class(res) <- c("RCT", "lpm")

        return(res)

    }

}

reg.ols <- function(y, d = NULL, x, data) {

    # make regression model
    yvar <- all.vars(y)[1]
    xeq <- as.character(x)[-1]

    if (is.null(d)) {

        model <- as.formula(paste(yvar, "~", xeq))

        # estimaton
        ols <- lm(model, data = data)

        # return
        class(ols) <- append("ols", class(ols))
        return(ols)

    } else {

        deq <- as.character(d)[-1]
        model1 <- as.formula(paste(yvar, "~", deq))
        model2 <- as.formula(paste(yvar, "~", deq, "+", xeq))
        model <- list(model1, model2)

        # estimation
        ols <- model %>% purrr::map(~ lm(., data = data))

        # return
        res <- list(fit = ols, treat = deq)
        class(res) <- c("RCT", "ols")
        return(res)
    }
}

reg.binomial <- function(y, x, link, data) {
  # make regression model
  yvar <- all.vars(y)[1]
  xeq <- as.character(x)[-1]
  model <- as.formula(paste(yvar, "~", xeq))

  # estimation
  est <- glm(model, family = binomial(link = link), data = data)

  # return
  class(est) <- append("binomial", class(est))
  return(est)
}
