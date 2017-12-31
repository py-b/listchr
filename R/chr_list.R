#' Separated character to list
#'
#' Convert a string into list, given separators.
#'
#' The function is vectorized hence \code{chr} can be
#' a character vector with more than one element. Consequently
#' the returned value is always a list (even of length 1).
#' If all elements of the returned list are atomic,
#' the list is simplified.
#'
#' @param chr string to convert.
#' @inheritParams list_to_chr
#' @return A list (\code{NULL} if the string is empty or \code{NA}).
#' @examples
#' chr_to_list("1;2;3|a;b")
#' chr_to_list("1;2;3")
#' chr_to_list("1|2|3")
#' chr_to_list(c("1;3", "2|4")) # vectorized usage
#' chr_to_list(c("1;3", "2;4")) # simplification
#' @export

chr_to_list <- function(chr,
                        atom_sep = ";",
                        list_sep = "|") {

  chr <- as.character(chr)
  chr[chr == ""] <- NA

  res <- strsplit(chr, list_sep, fixed = TRUE)
  res <- lapply(res, strsplit, atom_sep, fixed = TRUE)
  res <- lapply(res, function(x) if (anyNA(x[[1]])) NULL else x)

  # simplification éventuelle
  inf1 <- all(sapply(res, length) <= 1)
  if (inf1) res <- lapply(res, function(x) x[[1]])

  res

}


#' Separated character to list-columns (for a data frame)
#'
#' Convert the specified columns of a data frame into list-columns,
#' given separators.
#'
#' @param df a data frame containing columns to transform into
#'   list-columns.
#' @param vars columns to transform (vector of names or indexes)
#' @inheritParams list_to_chr
#' @return A data frame. Columns specified in \code{vars} are
#'   transformed into list-columns, other are unchanged.
#' @examples
#' df <-
#'   data.frame(
#'     x = c("1;2|3", "a|b"),
#'     y = c("1;3", "2|4"),
#'     z = c("1;3", "2;4")
#'   )
#' chr_to_listcol(df, "x")
#' chr_to_listcol(df, 2:3)
#' @export

chr_to_listcol <- function(df,
                           vars,
                           atom_sep = ";",
                           list_sep = "|") {

  df %>%
    dplyr::mutate_at(
      vars,
      chr_to_list,
      atom_sep,
      list_sep
    )

}
