#' Make regression table
#'
#' @description This function makes a character data frame of regression table.
#' An advantage point of this function is
#' that you can pass on output-format function such as `kable`.
#'
#' @param obj a list or object including regression analysis
#' @param keep_coef a character vector of variables kept
#' @param rm_coef a character vector of variables removed from table
#' @param label_coef a list including `ols variable name = new variable name`
#' @param add_line a list including additional contents of each column.
#' @param ... other augments which pass on each augment. See details.
#'
#' @return a data frame which includes all character strings
#'
#' @details the `binomial` method can show average marginal effect
#' if `ame = TRUE`, using `margins` in the `margins` package
#'
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @importFrom stringr str_detect
#' @importFrom dplyr recode
#' @importFrom dplyr full_join
#' @importFrom dplyr bind_rows
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom tidyr pivot_longer
#' @importFrom tibble tribble
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom magrittr %>%
#' @importFrom margins margins
#' @importFrom fixest r2
#' @importFrom stats logLik
#' @importFrom stats nobs
#' @importFrom stats setNames
#'
#' @export
#'
#+
regtab <- function(
  obj,
  keep_coef = NULL, rm_coef = NULL, label_coef = NULL,
  add_line = NULL,
  ...
) {

  # if not list, make list
  if (sum(class(obj) == "list") == 0) obj <- list(obj)

  # get and trim coeftab
  coeftab <- obj %>%
    map(function(x) ctab(x, ...)) %>%
    reduce(full_join, by = c("vars", "stat"))

  coeftab <- setNames(
    coeftab,
    c("vars", "stat", paste0("reg", seq_len(ncol(coeftab) - 2)))
  )

  # keep variables in coeftab
  if (!is.null(keep_coef)) {
    strings <- keep_coef %>% paste(collapse = "|")
    coeftab <- coeftab[str_detect(coeftab$vars, strings), ]
  }

  # drop variables from coeftab
  if (!is.null(rm_coef)) {
    strings <- rm_coef %>% paste(collapse = "|")
    coeftab <- coeftab[!str_detect(coeftab$vars, strings), ]
  }

  # rename variables
  if (!is.null(label_coef)) {
    coeftab <- coeftab %>%
      mutate(vars = recode(vars, !!!label_coef, .default = vars))
  }

  # make tabulation of regression stats
  stattab <- obj %>%
    map(~ stab(.)) %>%
    reduce(full_join, by = c("vars", "stat"))

  stattab <- setNames(
    stattab,
    c("vars", "stat", paste0("reg", seq_len(ncol(stattab) - 2)))
  )

  # make add line tab
  if (!is.null(add_line)) {
    addtab <- as_tibble(add_line) %>%
      mutate(stat = "add")

    tab <- bind_rows(coeftab, addtab) %>%
      bind_rows(stattab)
  } else {
    tab <- bind_rows(coeftab, stattab)
  }

  return(tab)

}

#' A fucntion of getting coefficient table
#'
#' @param x a list or object including regression analysis
#' @param ... other augments which pass on each augment.
#'
#+
ctab <- function(x, ...) {
  UseMethod("ctab")
}

ctab.fe_felm <- function(x) {
  x <- summary(x)$coefficients
  trim_ctab(x)
}

ctab.fe_fixest <- function(x) {
  x <- summary(x)$coeftable
  trim_ctab(x)
}

ctab.lpm <- function(x) {
  x <- x$test
  trim_ctab(x)
}

ctab.ols <- function(x) {
  x <- summary(x)$coefficients
  trim_ctab(x)
}

ctab.RCT <- function(x) {
  if (sum(class(x) == "lpm") > 0) {
    x <- lapply(x$test, trim_ctab)
  } else if (sum(class(x) == "ols") > 0) {
    x <- lapply(x$fit, function(x) summary(x)$coefficients)
    x <- lapply(x, trim_ctab)
  } else {
    stop("Unsupported class applied.")
  }
  x %>% reduce(full_join, by = c("vars", "stat"))
}

ctab.binomial <- function(x, ame = FALSE, ...) {
  if (ame) {
    x <- margins(x, data = x$model, ...)
    mat <- as.matrix(summary(x)[, 2:5])
    rownames(mat) <- summary(x)[, 1]
    trim_ctab(mat)
  } else {
    x <- summary(x)$coefficients
    trim_ctab(x)
  }
}

trim_ctab <- function(x) {

  tab <- tibble(
    vars = rownames(x),
    coef = x[, 1],
    se = x[, 2],
    p = x[, 4]
  )

  tab <- tab %>%
    mutate(
      coef = case_when(
        p <= .01 ~ sprintf("%1.3f***", coef),
        p <= .05 ~ sprintf("%1.3f**", coef),
        p <= .1 ~ sprintf("%1.3f*", coef),
        TRUE ~ sprintf("%1.3f", coef)
      ),
      se = sprintf("(%1.3f)", se)
    ) %>%
    select(-p) %>%
    pivot_longer(-vars, names_to = "stat", values_to = "val")

  return(tab)

}

#' A function of making regression stats table
#'
#' @param x a list or object including regression analysis
#+
stab <- function(x) {
  UseMethod("stab")
}

stab.fe_felm <- function(x) {
  tribble(
    ~vars, ~stat, ~val,
    "N", "stat", sprintf("%3d", nobs(x)),
    "R-squared", "stat", sprintf("%1.3f", summary(x)$r.squared)
  )
}

stab.fe_fixest <- function(x) {
  tribble(
    ~vars, ~stat, ~val,
    "N", "stat", sprintf("%3d", nobs(x)),
    "Adjusted R-squared", "stat", sprintf("%1.3f", r2(x)["ar2"])
  )
}

statlm <- function(x) {
  tribble(
    ~vars, ~stat, ~val,
    "N", "stat", sprintf("%3d", nobs(x)),
    "R-squared", "stat", sprintf("%1.3f", summary(x)$r.squared)
  )
}

stab.lpm <- function(x) {
  statlm(x$fit)
}

stab.ols <- function(x) {
  statlm(x)
}

stab.RCT <- function(x) {
  x <- lapply(x$fit, statlm)
  x %>% reduce(full_join, by = c("vars", "stat"))
}

stab.binomial <- function(x) {
  tribble(
    ~vars, ~stat, ~val,
    "N", "stat", sprintf("%3d", nobs(x)),
    "Log-likelihood", "stat", sprintf("%1.3f", logLik(x)[1])
  )
}
