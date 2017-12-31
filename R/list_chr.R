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
#' @param simplify_threshold non-negative number of atomic elements from
#'   which the character string is simplified (only first and last elements
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

  if (is.infinite(simplify_threshold)) {
    if (simplify_sep != "->")
      warning(paste(
        "Redefining \"simplify_sep\", but not \"simplify_threshold\".",
        "Forgot to set \"simplify_threshold\" to a non-negative",
        "integer value?"
      ))
  } else {
    if (!is.integer(as.integer(simplify_threshold)) | simplify_threshold < 0)
      stop("\"simplify_threshold\" must be a non-negative integer")
  }

  if (is.atomic(x)) {
    if (length(x) > 1 & length(x) >= simplify_threshold) {
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
