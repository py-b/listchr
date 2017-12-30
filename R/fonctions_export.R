#' Simple list to character
#'
#' Returns a text representation of a list, whose lower level
#' elements are atomic vectors.
#'
#' Even if the list has several hierarchical levels, the
#' returned value is always a string. To preserve a complex
#' hierarchy, use lapply or purr::map_chr at a given
#' hierarchical level.
#'
#' @param x a list, whose lower level elements are atomic vectors.
#' @param atom_sep separator for the elements of atomic vectors.
#' @param list_sep separator for the elements of lists.
#' @param simplify_threshold number of atomic elements from which
#'   the character string is simplified (only first and last elements
#'   separated by a separator, "->" by default).
#' @param simplify_sep a separator (one or several characters)
#'   if the string has to be simplified.
#' @return A string, the text representation of x.
#' @examples
#' list_to_chr(list(letters[1:2], 1:3, 4))
#' list_to_chr(list(letters, LETTERS), simplify_threshold = 10)
#' @export

list_to_chr <- function(x,
                        atom_sep = ";",
                        list_sep = "|",
                        simplify_threshold = Inf,
                        simplify_sep = "->") {

  if (is.null(x)) return(NA_character_)

  if (is.atomic(x)) {
    if (length(x) >= simplify_threshold) {
      res <- paste0(x[1], simplify_sep, rev(x)[1])
    } else {
      res <- paste0(x, collapse = atom_sep)
    }
    return(res)
  }

  res <- ""
  for (ele in x) {
    res <-
      paste(
        res,
        list_to_chr(
          x = ele,
          atom_sep = atom_sep,
          simplify_threshold = simplify_threshold,
          simplify_sep = simplify_sep
        ),
        sep = list_sep
      )
  }
  substring(res, nchar(list_sep) + 1)

}


#' List-columns to character (for a data frame)
#'
#' Convert all list-columns of a data frame into character vectors.
#'
#' @param df a data frame containing list-columns.
#' @inheritParams list_to_chr
#' @return A data frame. Simple columns (atomic) remain the same,
#'   list-columns (recursive) are transformed into character vectors.
#' @examples
#' df <- data.frame(x = 1:2)
#' df$y <- list(letters[1:3], 1:2)
#' df$z <- list(list(3:4, 5:7), "d")
#' listcol_to_chr(df)
#' @export
#' @importFrom dplyr "%>%"

listcol_to_chr <- function(df,
                           atom_sep = ";",
                           list_sep = "|",
                           simplify_threshold = Inf,
                           simplify_sep = "->") {

  df %>%
    dplyr::mutate_if(
    is.recursive,
    function(lc) {
      lc %>%
        vapply(
          list_to_chr,
          FUN.VALUE = character(1),
          atom_sep = atom_sep,
          list_sep = list_sep,
          simplify_threshold = simplify_threshold,
          simplify_sep = simplify_sep
        )
    }
  )

}


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
