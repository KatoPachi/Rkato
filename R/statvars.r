#' Calculate Standard Error of Mean
#'
#' @param x a numerical vector
#' @param na.rm a logical value whether we remove N/A value
#'
#' @return a numeric which is a statndard error of mean
#'
#' @importFrom stats na.omit
#' @importFrom stats var
#'
#' @export
#+
se <- function(x, na.rm = FALSE) {

  # na.rm = TRUE, we remove NA using na.omit()
  if (na.rm) x <- na.omit(x)

  # calculate and return se
  sqrt(var(x) / length(x))
}

#' Summary table of variables
#'
#' @description This is alternative method of `summary()`
#'     for countable or continuous vairbales.
#'     Since this function returns a data frame,
#'     you can pass some output format function (e.g. `kable`) easily.
#'
#' @param data a `data.frame` object.
#' @param vars a character vector of variables.
#' @param stat a character vector of statisics.
#'     "mean" is an average, "se" is its standard errors,
#'     "sd" is a standard error, "min" is minimum, "max" is maximum,
#'     "q25" is a first quantile, "median" is median, "q75" is a third quantile.
#'     "sum" is a sum of variable, and "N" is sample size.
#'
#' @return an object whose class is `data.frame`
#'
#' @importFrom dplyr summarize_at
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect everything
#' @importFrom magrittr %>%
#'
#' @export
#+
statvars <- function(
    data, vars,
    stat = c(
        "mean", "se", "sd", "min", "q25", "median", "q75", "max", "sum", "N"
    )
) {

  tab <- data %>%
    summarize_at(
      vars,
      list(
        mean = ~mean(., na.rm = TRUE),
        se = ~se(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        q25 = ~quantile(., prob = .25, na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        q75 = ~quantile(., prob = .75, na.rm = TRUE),
        max = ~max(., na.rm = TRUE),
        sum = ~sum(., na.rm = TRUE),
        N = ~sum(!is.na(.))
      )
    )

    if (length(vars) == 1) {

        tab <- tab %>%
          mutate(vars = vars) %>%
          select(c("vars", stat))

    } else {

        pattern <- "(.*)_(mean|se|sd|min|q25|median|q75|max|sum|N)"

        tab <- tab %>%
          pivot_longer(
            everything(),
            names_to = c("vars", ".value"),
            names_pattern = pattern
          ) %>%
          select(c("vars", stat))

    }

    return(tab)

}