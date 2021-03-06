% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statvars.r
\name{statvars}
\alias{statvars}
\title{Summary table of variables}
\usage{
statvars(
  data,
  vars,
  stat = c("mean", "se", "sd", "min", "q25", "median", "q75", "max", "sum", "N")
)
}
\arguments{
\item{data}{a \code{data.frame} object.}

\item{vars}{a character vector of variables.}

\item{stat}{a character vector of statisics.
"mean" is an average, "se" is its standard errors,
"sd" is a standard error, "min" is minimum, "max" is maximum,
"q25" is a first quantile, "median" is median, "q75" is a third quantile.
"sum" is a sum of variable, and "N" is sample size.}
}
\value{
an object whose class is \code{data.frame}
}
\description{
This is alternative method of \code{summary()}
for countable or continuous vairbales.
Since this function returns a data frame,
you can pass some output format function (e.g. \code{kable}) easily.
}
