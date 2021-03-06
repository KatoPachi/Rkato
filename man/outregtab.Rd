% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outregtab.r
\name{outregtab}
\alias{outregtab}
\title{Output-formated regression table}
\usage{
outregtab(
  x,
  colnames = NULL,
  caption = NULL,
  header_rows = NULL,
  align = NULL,
  indent = NULL,
  packing = NULL,
  footnote = NULL,
  fontsize = 7,
  digits = 3,
  outpkg = c("kable", "flextable")
)
}
\arguments{
\item{x}{a data.frame or tibble object}

\item{colnames}{a list of column names including "old one" = "new one"}

\item{caption}{strings of table caption}

\item{header_rows}{a list of vector like \verb{c("name" = number of columns, ...)}}

\item{align}{strings of table align.
The left-aligned, center-aligned, and right-aligned columns
are \code{"l"}, \code{"c"}, and \code{"r"}, respectively.}

\item{indent}{a list of numeric vector}

\item{packing}{a list of indent with subcaption
like \code{"title" = c(start, end)}}

\item{footnote}{strings of table footnote}

\item{fontsize}{numeric of font size of table. Default is \code{7}.}

\item{digits}{numeric of digits of table. Default is \code{3}.}

\item{outpkg}{which packages(\code{c("kable", "flextable")}) uses.}
}
\description{
This function transform regression table made by \code{regtab} into
some output formats like pdf, HTML, MSWord, etc,
using \code{knitr::kable} and \code{flextable::flextable}
}
