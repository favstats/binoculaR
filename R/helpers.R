#' Helper Function 1
#'
#'
sju.rmspc <- function (html.table)
{
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE,
                  useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  return(cleaned)
}


#' Helper Function 2
#'
#'
get.encoding <- function (encoding, data = NULL)
{
  if (is.null(encoding)) {
    if (!is.null(data) && is.data.frame(data)) {
      labs <- sjlabelled::get_label(data[[1]])
      if (!is.null(labs) && is.character(labs))
        encoding <- Encoding(sjlabelled::get_label(data[[1]]))
      else encoding <- "UTF-8"
      if (encoding == "unknown")
        encoding <- "UTF-8"
    }
    else if (.Platform$OS.type == "unix")
      encoding <- "UTF-8"
    else encoding <- "Windows-1252"
  }
  return(encoding)
}

#' Helper 3
#'
#'
has_value_labels <- function (x)
{
  !(is.null(attr(x, "labels", exact = T)) && is.null(attr(x,
                                                          "value.labels", exact = T)))
}



#' View Dataframe Function adapted from sjPlot
#'
#' @export
df_overview <- function (x, weight.by = NULL, alternate.rows = TRUE, show.id = TRUE,
                         show.type = FALSE, show.values = TRUE, show.string.values = FALSE,
                         show.labels = TRUE, show.frq = FALSE, show.prc = FALSE,
                         show.wtd.frq = FALSE, show.wtd.prc = FALSE, show.na = FALSE,
                         max.len = 15, sort.by.name = FALSE, wrap.labels = 50, hide.progress = FALSE,
                         verbose = TRUE, CSS = NULL, encoding = NULL, file = NULL,
                         use.viewer = TRUE, remove.spaces = TRUE)
{
  if (!missing(hide.progress)) {
    message("`hide.progress` is deprecated. Please use `verbose` instead.")
    verbose <- !hide.progress
  }
  encoding <- get.encoding(encoding, x)
  if (!is.data.frame(x))
    stop("`x` needs to be a data frame!", call. = FALSE)
  dfname <- deparse(substitute(x))
  if (!missing(weight.by)) {
    weights <- rlang::quo_name(rlang::enquo(weight.by))
    w.string <- tryCatch({
      eval(weight.by)
    }, error = function(x) {
      NULL
    }, warning = function(x) {
      NULL
    }, finally = function(x) {
      NULL
    })
    if (!is.null(w.string) && is.character(w.string))
      weights <- w.string
    if (sjmisc::is_empty(weights) || weights == "NULL")
      weights <- NULL
  }
  else weights <- NULL
  all.na <- purrr::map_lgl(x, ~all(is.na(.x)))
  id <- seq_len(ncol(x))
  cnames <- colnames(x)
  if (any(all.na)) {
    rem.col <- seq_len(ncol(x))[all.na]
    message(sprintf("Following %i variables have only missing values and are not shown:",
                    sum(all.na)))
    cat(paste(sprintf("%s [%i]", cnames[all.na], rem.col),
              collapse = ", "))
    cat("\n")
    id <- id[!all.na]
    cnames <- cnames[!all.na]
  }
  df.var <- sjlabelled::get_label(x)
  df.val <- sjlabelled::get_labels(x)
  if (sort.by.name)
    id <- id[order(cnames)]
  tag.table <- "table"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.arc <- "arc"
  tag.caption <- "caption"
  tag.omit <- "omit"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "border-bottom:double; font-style:italic; font-weight:normal; padding:0.2cm; text-align:left; vertical-align:top;"
  css.tdata <- "padding:0.2cm; text-align:left; vertical-align:top;"
  css.arc <- "background-color:#eeeeee"
  css.caption <- "font-weight: bold; text-align:left;"
  css.omit <- "color:#999999;"
  if (!is.null(CSS)) {
    if (!is.null(CSS[["css.table"]]))
      css.table <- ifelse(substring(CSS[["css.table"]],
                                    1, 1) == "+", paste0(css.table, substring(CSS[["css.table"]],
                                                                              2)), CSS[["css.table"]])
    if (!is.null(CSS[["css.thead"]]))
      css.thead <- ifelse(substring(CSS[["css.thead"]],
                                    1, 1) == "+", paste0(css.thead, substring(CSS[["css.thead"]],
                                                                              2)), CSS[["css.thead"]])
    if (!is.null(CSS[["css.tdata"]]))
      css.tdata <- ifelse(substring(CSS[["css.tdata"]],
                                    1, 1) == "+", paste0(css.tdata, substring(CSS[["css.tdata"]],
                                                                              2)), CSS[["css.tdata"]])
    if (!is.null(CSS[["css.arc"]]))
      css.arc <- ifelse(substring(CSS[["css.arc"]], 1,
                                  1) == "+", paste0(css.arc, substring(CSS[["css.arc"]],
                                                                       2)), CSS[["css.arc"]])
    if (!is.null(CSS[["css.caption"]]))
      css.caption <- ifelse(substring(CSS[["css.caption"]],
                                      1, 1) == "+", paste0(css.caption, substring(CSS[["css.caption"]],
                                                                                  2)), CSS[["css.caption"]])
    if (!is.null(CSS[["css.omit"]]))
      css.omit <- ifelse(substring(CSS[["css.omit"]],
                                   1, 1) == "+", paste0(css.omit, substring(CSS[["css.omit"]],
                                                                            2)), CSS[["css.omit"]])
  }
  page.style <- sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n%s { %s }\n.%s { %s }\n</style>",
                        tag.table, css.table, tag.thead, css.thead, tag.tdata,
                        css.tdata, tag.arc, css.arc, tag.caption, css.caption,
                        tag.omit, css.omit)
  toWrite <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n%s\n</head>\n<body>\n",
                     encoding, page.style)
  page.content <- sprintf("<table>\n  <caption>Data frame: %s</caption>\n",
                          dfname)
  page.content <- paste0(page.content, "  <tr>\n    ")
  if (show.id)
    page.content <- paste0(page.content, "<th class=\"thead\">ID</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Name</th>")
  if (show.type)
    page.content <- paste0(page.content, "<th class=\"thead\">Type</th>")
  page.content <- paste0(page.content, "<th class=\"thead\">Label</th>")
  if (show.na)
    page.content <- paste0(page.content, "<th class=\"thead\">missings</th>")
  if (show.values)
    page.content <- paste0(page.content, "<th class=\"thead\">Values</th>")
  if (show.labels)
    page.content <- paste0(page.content, "<th class=\"thead\">Value Labels</th>")
  if (show.frq)
    page.content <- paste0(page.content, "<th class=\"thead\">Freq.</th>")
  if (show.prc)
    page.content <- paste0(page.content, "<th class=\"thead\">%</th>")
  if (show.wtd.frq)
    page.content <- paste0(page.content, "<th class=\"thead\">weighted Freq.</th>")
  if (show.wtd.prc)
    page.content <- paste0(page.content, "<th class=\"thead\">weighted %</th>")
  page.content <- paste0(page.content, "\n  </tr>\n")
  if (verbose)
    pb <- utils::txtProgressBar(min = 0, max = length(id),
                                style = 3)
  for (ccnt in 1:length(id)) {
    index <- id[ccnt]
    arcstring <- ""
    if (alternate.rows)
      arcstring <- ifelse(sjmisc::is_even(ccnt), " arc",
                          "")
    page.content <- paste0(page.content, "  <tr>\n")
    if (show.id)
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%i</td>\n",
                                                   arcstring, index))
    if (!is.list(x[[index]]) && !is.null(comment(x[[index]])))
      td.title.tag <- sprintf(" title=\"%s\"", comment(x[[index]]))
    else td.title.tag <- ""
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\"%s>%s</td>\n",
                                                 arcstring, td.title.tag, colnames(x)[index]))
    if (show.type) {
      vartype <- sjmisc::var_type(x[[index]])
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                   arcstring, vartype))
    }
    if (index <= length(df.var)) {
      varlab <- df.var[index]
      if (!is.null(wrap.labels)) {
        varlab <- sjmisc::word_wrap(varlab, wrap.labels,
                                    "<br>")
      }
    }
    else {
      varlab <- "<NA>"
    }
    page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                 arcstring, varlab))
    if (show.na) {
      if (is.list(x[[index]])) {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\"><span class=\"omit\">&lt;list&gt;</span></td>\n",
                                                     arcstring))
      }
      else {
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%i (%.2f%%)</td>\n",
                                                     arcstring, sum(is.na(x[[index]]), na.rm = T),
                                                     100 * sum(is.na(x[[index]]), na.rm = T)/nrow(x)))
      }
    }
    if (is.numeric(x[[index]]) && !has_value_labels(x[[index]])) {
      if (show.values || show.labels) {
        if (sjmisc::is_float(x[[index]]))
          valstring <- paste0(sprintf("%.1f", range(x[[index]],
                                                    na.rm = T)), collapse = "-")
        else valstring <- paste0(sprintf("%i", as.integer(range(x[[index]],
                                                                na.rm = T))), collapse = "-")
        if (show.values && show.labels) {
          colsp <- " colspan=\"2\""
          valstring <- paste0("<em>range: ", valstring,
                              "</em>")
        }
        else {
          colsp <- ""
        }
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\"%s>%s</td>\n",
                                                     arcstring, colsp, valstring))
      }
    }
    else {
      if (show.values) {
        valstring <- ""
        if (index <= ncol(x)) {
          if (is.list(x[[index]])) {
            valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
          }
          else {
            vals <- sjlabelled::get_values(x[[index]])
            if (!is.null(vals)) {
              loop <- stats::na.omit(seq_len(length(vals))[1:max.len])
              for (i in loop) {
                valstring <- paste0(valstring, vals[i])
                if (i < length(vals))
                  valstring <- paste0(valstring, "<br>")
              }
              if (max.len < length(vals))
                valstring <- paste0(valstring, "<span class=\"omit\">&lt;...&gt;</span>")
            }
          }
        }
        else {
          valstring <- "<NA>"
        }
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                     arcstring, valstring))
      }
      if (show.labels) {
        valstring <- ""
        if (index <= length(df.val)) {
          if (is.list(x[[index]])) {
            valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
          }
          else {
            vals <- df.val[[index]]
            if (!is.null(vals))
              vals <- stats::na.omit(vals)
            if (is.character(x[[index]]) && !is.null(vals) &&
                !sjmisc::is_empty(vals)) {
              if (show.string.values)
                vals <- sort(vals)
              else vals <- "<span class=\"omit\" title =\"'show.string.values = TRUE' to show values.\">&lt;output omitted&gt;</span>"
            }
            if (!is.null(vals)) {
              loop <- stats::na.omit(seq_len(length(vals))[1:max.len])
              for (i in loop) {
                valstring <- paste0(valstring, vals[i])
                if (i < length(vals))
                  valstring <- paste0(valstring, "<br>")
              }
              if (max.len < length(vals))
                valstring <- paste0(valstring, "<span class=\"omit\">&lt;... truncated&gt;</span>")
            }
          }
        }
        else {
          valstring <- "<NA>"
        }
        page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                     arcstring, valstring))
      }
    }
    if (show.frq) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else valstring <- frq.value(index, x, df.val)
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                   arcstring, valstring))
    }
    if (show.prc) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else valstring <- frq.value(index, x, df.val, as.prc = TRUE)
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                   arcstring, valstring))
    }
    if (show.wtd.frq && !is.null(weights)) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else valstring <- frq.value(index, x, df.val, weights)
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                   arcstring, valstring))
    }
    if (show.wtd.prc && !is.null(weights)) {
      if (is.list(x[[index]]))
        valstring <- "<span class=\"omit\">&lt;list&gt;</span>"
      else valstring <- frq.value(index, x, df.val, weights,
                                  as.prc = TRUE)
      page.content <- paste0(page.content, sprintf("    <td class=\"tdata%s\">%s</td>\n",
                                                   arcstring, valstring))
    }
    if (verbose)
      utils::setTxtProgressBar(pb, ccnt)
    page.content <- paste0(page.content, "  </tr>\n")
  }
  if (verbose)
    close(pb)
  page.content <- paste(page.content, "</table>", sep = "\n")
  toWrite <- paste0(toWrite, sprintf("%s\n</body></html>",
                                     page.content))
  knitr <- page.content
  knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table),
                knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE,
                useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE,
                useBytes = TRUE)
  knitr <- gsub(tag.arc, css.arc, knitr, fixed = TRUE, useBytes = TRUE)
  if (remove.spaces) {
    knitr <- sju.rmspc(knitr)
    toWrite <- sju.rmspc(toWrite)
    page.content <- sju.rmspc(page.content)
  }
  structure(class = c("sjTable", "df_overview"), list(page.style = page.style,
                                                      page.content = page.content, page.complete = toWrite,
                                                      header = NULL, knitr = knitr, file = file, viewer = use.viewer))
}



