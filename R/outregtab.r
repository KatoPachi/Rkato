#'
#' @importFrom rlist list.append
#' @importFrom knitr kable
#+
basekable <- function(
  x, caption, align, digits, colnames
) {
  # collect augments passing on knitr::kable
  kable_aug <- list(
    x = x,
    booktabs = TRUE,
    escape = FALSE,
    linesep = "",
    digits = digits,
    caption = caption,
    align = align
  )

  if (!is.null(colnames)) {
    kable_aug <- list.append(
      kable_aug,
      col.names = unlist(colnames)[names(x)]
    )
  }

  # run knitr::kable via do.call
  do.call(kable, kable_aug)
}

#'
#' @importFrom magrittr %>%
#' @importFrom kableExtra kable_styling
#' @importFrom kableExtra add_header_above
#' @importFrom kableExtra add_indent
#' @importFrom kableExtra pack_rows
#' @importFrom kableExtra footnote
#+
extrakable <- function(
  x, fontsize, header_rows,
  indent, packing, footnote
) {
  # specify font size
  tbl <- x %>% kable_styling(font_size = fontsize)

  # add rows in header if specified
  if (!is.null(header_rows)) {
    for (i in seq_len(length(header_rows))) {
      tbl <- tbl %>% add_header_above(header_rows[[i]])
    }
  }

  # make indent if specified
  if (!is.null(indent)) {
    for (i in seq_len(length(indent))) {
      tbl <- tbl %>% add_indent(indent[[i]])
    }
  }

  # make packing (indent plus caption) if specified
  if (!is.null(packing)) {
    for (i in seq_len(length(packing))) {
      tbl <- tbl %>%
        pack_rows(
          names(packing)[i],
          packing[[i]][1], packing[[i]][2],
          escape = FALSE, bold = FALSE
        )
    }
  }

  # add footnote if specified
  if (!is.null(footnote)) {
    tbl <- tbl %>%
      footnote(general_title = "", general = footnote, threeparttable = TRUE)
  }

  return(tbl)
}

#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_split
#' @importFrom stringr str_replace
#'
#+
flexaugs <- function(
  colnames, caption, header_rows, align,
  indent, packing, footnote, fontsize, digits
) {
  augs <- list(
    colnames = colnames,
    caption = caption,
    header_rows = header_rows,
    align = align,
    indent = indent,
    packing = packing,
    footnote = footnote,
    fontsize = fontsize,
    digits = digits
  )

  if (!is.null(augs$packing)) {
    group <- NULL
    for (i in seq_len(length(augs$packing))) {
      start <- augs$packing[[i]][1]
      end <- augs$packing[[i]][2]
      group[start:end] <- names(augs$packing)[i]
    }
    augs$packing <- group
  }

  if (!is.null(augs$header_rows)) {
    for (i in seq_len(length(augs$header_rows))) {
      rows <- NULL
      for (j in seq_len(length(augs$header_rows[[i]]))) {
        if (is.na(as.numeric(augs$header_rows[[i]][j]))) {
          rows <- c(rows, names(augs$header_rows[[i]])[j])
        } else {
          rows <- c(
            rows,
            rep(
              names(augs$header_rows[[i]])[j],
              as.numeric(augs$header_rows[[i]][j])
            )
          )
        }
      }
      augs$header_rows[[i]] <- rows
    }
  }

  if (!is.null(augs$align)) {
    align_pt <- str_split(align, pattern = "")[[1]] %>%
      str_replace("l", "left") %>%
      str_replace("r", "right") %>%
      str_replace("c", "center")

    augs$align <- align_pt
  }

  return(augs)
}

#'
#' @importFrom dplyr bind_cols
#' @importFrom flextable as_grouped_data
#' @importFrom flextable as_flextable
#' @importFrom flextable flextable
#' @importFrom flextable set_caption
#' @importFrom flextable set_header_labels
#' @importFrom flextable add_footer_lines
#' @importFrom flextable add_header_row
#' @importFrom flextable merge_h
#' @importFrom flextable align
#' @importFrom flextable padding
#' @importFrom flextable border_remove
#' @importFrom flextable hline_top
#' @importFrom flextable hline_bottom
#' @importFrom flextable fontsize
#' @importFrom flextable autofit
#' @importFrom officer fp_border
#+
flextab <- function(x, augs) {
  # make flextable obj and packing before if specified
    if (!is.null(augs$packing)) {
      tbl <- bind_cols(group = augs$packing, x) %>%
        as_grouped_data(groups = "group") %>%
        as_flextable(hide_grouplabel = TRUE)
    } else {
      tbl <- flextable(x)
    }

    # add caption if specified
    if (!is.null(augs$caption)) {
      tbl <- tbl %>% set_caption(augs$caption)
    }

    # change column name if speficied
    if (!is.null(augs$colnames)) {
      tbl <- tbl %>% set_header_labels(values = augs$colnames)
    }

    # add footnote if specifid
    if (!is.null(augs$footnote)) {
      tbl <- tbl %>% add_footer_lines(values = augs$footnote)
    }

    # add rows in header if specified
    if (!is.null(augs$header_rows)) {
      for (i in seq_len(length(augs$header_rows))) {
        tbl <- tbl %>% add_header_row(values = augs$header_rows[[i]])
      }
      tbl <- tbl %>% merge_h(part = "header")
    }

    # set align if specified
    if (!is.null(augs$align)) {
      for (j in seq_len(length(augs$align))) {
        tbl <- align(tbl, j = j, align = augs$align[j], part = "all")
      }
    }

    # set indent if speficied
    if (!is.null(augs$indent)) {
      for (i in seq_len(length(augs$indent))) {
        tbl <- tbl %>% padding(augs$indent[[i]], 1, padding.left = 20)
      }
    }

    # change border style
    tbl <- tbl %>%
      border_remove() %>%
      hline_top(part = "all", border = fp_border()) %>%
      hline_bottom(part = "header", border = fp_border()) %>%
      hline_bottom(part = "body", border = fp_border()) %>%
      fontsize(size = augs$fontsize, part = "all") %>%
      autofit()

  return(tbl)
}

#' Output-formated regression table
#'
#' @description This function transform regression table made by `regtab` into
#' some output formats like pdf, HTML, MSWord, etc,
#' using `knitr::kable` and `flextable::flextable`
#'
#' @param x a data.frame or tibble object
#' @param colnames a list of column names including "old one" = "new one"
#' @param caption strings of table caption
#' @param header_rows a list of vector like `c("name" = number of columns, ...)`
#' @param align strings of table align.
#' The left-aligned, center-aligned, and right-aligned columns
#' are `"l"`, `"c"`, and `"r"`, respectively.
#' @param indent a list of numeric vector
#' @param packing a list of indent with subcaption
#' like `"title" = c(start, end)`
#' @param footnote strings of table footnote
#' @param fontsize numeric of font size of table. Default is `7`.
#' @param digits numeric of digits of table. Default is `3`.
#' @param outpkg which packages(`c("kable", "flextable")`) uses.
#'
#' @importFrom magrittr %>%
#'
#' @export
#+
outregtab <- function(
  x, colnames = NULL, caption = NULL,
  header_rows = NULL, align = NULL,
  indent = NULL, packing = NULL, footnote = NULL,
  fontsize = 7, digits = 3,
  outpkg = c("kable", "flextable")
) {
  if (outpkg == "kable") {

    basekable(x, caption, align, digits, colnames) %>%
      extrakable(fontsize, header_rows, indent, packing, footnote)

  } else if (outpkg == "flextable") {

    augs <- flexaugs(
      colnames, caption, header_rows, align,
      indent, packing, footnote, fontsize, digits
    )
    flextab(x, augs)

  }
}
